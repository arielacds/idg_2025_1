# ----------------------------------------
# 1. LIBRERÍAS
# ----------------------------------------
# Estas librerías permiten cargar datos, hacer microsimulación, conectarse a PostgreSQL,
# generar gráficos y manipular geometría espacial.
library(rakeR)        # Microsimulación espacial
library(RPostgres)    # Conexión con PostgreSQL
library(DBI)          # Manejo de bases de datos
library(ggplot2)      # Visualización de datos
library(sf)           # Manejo de datos espaciales
library(data.table)   # Tablas de alta eficiencia
library(viridis)      # Escalas de color accesibles

# ----------------------------------------
# 2. CARGA Y PREPARACIÓN DE DATOS
# ----------------------------------------
# Leemos la base CASEN 2022 (Región Metropolitana) y el CENSO 2017 procesado.
# Objetivo: tener la información individual (CASEN) y las restricciones agregadas (CENSO)
# para aplicar la microsimulación.

cons_censo_df <- readRDS("data/cons_censo_df.rds")
casen_raw <- readRDS("data/casen_rm.rds")

# Seleccionamos variables necesarias
vars_base <- c("estrato", "esc", "edad", "sexo", "e6a", "s13")
casen <- casen_raw[, vars_base, drop = FALSE]
rm(casen_raw)

# Preprocesamiento: extraer comuna desde estrato
casen$Comuna <- substr(as.character(casen$estrato), 1, 5)
casen$estrato <- NULL

# Conversión a tipos compatibles
casen$esc <- as.integer(unclass(casen$esc))
casen$edad <- as.integer(unclass(casen$edad))
casen$e6a <- as.numeric(unclass(casen$e6a))
casen$sexo <- as.integer(unclass(casen$sexo))
casen$s13 <- as.numeric(unclass(casen$s13))

# ----------------------------------------
# 3. VARIABLES DE INTERÉS
# ----------------------------------------
# Creamos variables dummy (binarias) para las poblaciones que queremos microsimular:
# mujeres en FONASA, mujeres en ISAPRE, y hombres en ISAPRE.
# Estas serán las variables a simular espacialmente.

casen$mujer_fonasa   <- ifelse(casen$sexo == 2 & casen$s13 %in% 1, 1, 0)
casen$mujer_isapre   <- ifelse(casen$sexo == 2 & casen$s13 == 2, 1, 0)
casen$hombre_isapre  <- ifelse(casen$sexo == 1 & casen$s13 == 2, 1, 0)

# ----------------------------------------
# 4. IMPUTACIÓN Y RECODIFICACIÓN
# ----------------------------------------
# Imputamos escolaridad cuando falta, usando regresión lineal con e6a (años aprobados).
# Luego categorizamos edad, escolaridad y sexo, como requiere rakeR.

idx_na <- which(is.na(casen$esc))
fit <- lm(esc ~ e6a, data = casen[-idx_na,])
pred <- predict(fit, newdata = casen[idx_na, ,drop = FALSE])
casen$esc[idx_na] <- as.integer(round(pmax(0, pmin(29, pred))))

casen$ID <- as.character(seq_len(nrow(casen)))
col_cons <- sort(setdiff(names(cons_censo_df), c("GEOCODIGO", "COMUNA")))
age_levels  <- grep("^edad", col_cons, value = TRUE)
esc_levels  <- grep("^esco", col_cons, value = TRUE)
sexo_levels <- grep("^sexo_", col_cons, value = TRUE)

casen$edad_cat <- cut(casen$edad, breaks = c(0,30,40,50,60,70,80,Inf),
                      labels = age_levels, right = FALSE, include.lowest = TRUE)
casen$esc_cat <- factor(ifelse(casen$esc == 0, esc_levels[1],
                               ifelse(casen$esc <= 8, esc_levels[2],
                                      ifelse(casen$esc <= 12, esc_levels[3], esc_levels[4]))),
                        levels = esc_levels)
casen$sexo_cat <- factor(ifelse(casen$sexo == 2, sexo_levels[1],
                                ifelse(casen$sexo == 1, sexo_levels[2], NA)),
                         levels = sexo_levels)

# ----------------------------------------
# 5. MICROSIMULACIÓN CON rakeR
# ----------------------------------------
# Simulamos la distribución de cada grupo de interés a nivel de zona censal,
# respetando las distribuciones agregadas del CENSO.

cons_censo_comunas <- split(cons_censo_df, cons_censo_df$COMUNA)
inds_list <- split(casen, casen$Comuna)

simular_grupo <- function(var_dummy) {
  lapply(names(cons_censo_comunas), function(zona) {
    cons_i <- cons_censo_comunas[[zona]]
    col_order <- sort(setdiff(names(cons_i), c("COMUNA", "GEOCODIGO")))
    cons_i <- cons_i[, c("GEOCODIGO", col_order), drop = FALSE]
    
    tmp <- inds_list[[zona]]
    inds_i <- tmp[, c("ID", "edad_cat", "esc_cat", "sexo_cat"), drop = FALSE]
    names(inds_i) <- c("ID", "Edad", "Escolaridad", "Sexo")
    
    w_frac <- weight(cons = cons_i, inds = inds_i, vars = c("Edad", "Escolaridad", "Sexo"))
    sim_i <- integerise(weights = w_frac, inds = inds_i, seed = 123)
    
    merge(sim_i, tmp[, c("ID", var_dummy)], by = "ID", all.x = TRUE)
  })
}

sim_df_mf <- data.table::rbindlist(simular_grupo("mujer_fonasa"), idcol = "COMUNA")
sim_df_mi <- data.table::rbindlist(simular_grupo("mujer_isapre"), idcol = "COMUNA")
sim_df_hi <- data.table::rbindlist(simular_grupo("hombre_isapre"), idcol = "COMUNA")

# ----------------------------------------
# 6. AGREGACIÓN DE RESULTADOS
# ----------------------------------------
# Para cada zona censal, calculamos el porcentaje de personas que pertenecen al grupo simulado.

agg_dummy <- function(df, var) {
  aggregate(
    formula(paste(var, "~ zone")),
    data = df,
    FUN = function(x) round(100 * sum(x == 1, na.rm = TRUE) / sum(!is.na(x)), 2)
  )
}

zonas_mf <- agg_dummy(sim_df_mf, "mujer_fonasa")
zonas_mi <- agg_dummy(sim_df_mi, "mujer_isapre")
zonas_hi <- agg_dummy(sim_df_hi, "hombre_isapre")

names(zonas_mf) <- c("geocodigo", "mujeres_fonasa")
names(zonas_mi) <- c("geocodigo", "mujeres_isapre")
names(zonas_hi) <- c("geocodigo", "hombres_isapre")

# ----------------------------------------
# 7. CONEXIÓN A BASE DE DATOS Y VISUALIZACIÓN
# ----------------------------------------
# Exportamos los resultados simulados a PostgreSQL y generamos mapas temáticos
# que muestran la distribución espacial de cada grupo. Agregamos bordes de comunas
# y etiquetas para facilitar la interpretación visual.

con <- dbConnect(Postgres(), dbname = "censo_rm_2017", host = "localhost", port = 5432, user = "postgres", password = "postgres")

exportar_y_mapear <- function(df, nombre_tmp, nombre_final, var_color, titulo, subtitulo, viridis_option) {
  dbWriteTable(con, Id(schema = "dpa", table = nombre_tmp), df, overwrite = TRUE)
  dbExecute(con, paste0("CREATE INDEX ON dpa.", nombre_tmp, "(geocodigo)"))
  dbExecute(con, paste0("ANALYZE dpa.", nombre_tmp))
  dbExecute(con, paste0("DROP TABLE IF EXISTS dpa.", nombre_final))
  
  dbExecute(con, sprintf("
    CREATE TABLE dpa.%s AS
    SELECT z.*, t.%s
    FROM dpa.zonas_censales_rm z
    LEFT JOIN dpa.%s t ON z.geocodigo::text = t.geocodigo
    WHERE urbano = 1
      AND (nom_provin = 'SANTIAGO' OR nom_comuna IN ('PUENTE ALTO', 'SAN BERNARDO'))
  ", nombre_final, var_color, nombre_tmp))
  
  zonas_sf <- st_read(con, query = sprintf("SELECT * FROM dpa.%s", nombre_final))
  comunas_borde <- st_read(con, query = "
    SELECT nom_comuna, ST_Union(geom) AS geometry
    FROM dpa.zonas_censales_rm
    WHERE urbano = 1 AND (nom_provin = 'SANTIAGO' OR nom_comuna IN ('PUENTE ALTO', 'SAN BERNARDO'))
    GROUP BY nom_comuna
  ")
  comunas_texto <- suppressWarnings(st_centroid(comunas_borde))
  
  ggplot() +
    geom_sf(data = zonas_sf, aes_string(fill = var_color), color = "grey70", size = 0.1) +
    geom_sf(data = comunas_borde, fill = NA, color = "black", size = 0.5) +
    geom_sf_text(data = comunas_texto, aes(label = nom_comuna), size = 2, color = "black") +
    scale_fill_viridis_c(option = viridis_option, direction = -1, name = paste0("% ", gsub("_", " ", toupper(var_color)))) +
    theme_minimal() +
    labs(title = titulo, subtitle = subtitulo) +
    theme(axis.title = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank(), panel.grid = element_blank())
}

# Se genera un mapa por cada grupo
exportar_y_mapear(zonas_mf, "tmp_fonasa_rm", "zonas_fonasa", "mujeres_fonasa", "Concentración de Mujeres Afiliadas a FONASA", "Gran Santiago", "H")
exportar_y_mapear(zonas_mi, "tmp_isapre_m", "zonas_isapre_m", "mujeres_isapre", "Concentración de Mujeres Afiliadas a ISAPRE", "Gran Santiago", "D")
exportar_y_mapear(zonas_hi, "tmp_isapre_h", "zonas_isapre_h", "hombres_isapre", "Concentración de Hombres Afiliados a ISAPRE", "Gran Santiago", "B")
