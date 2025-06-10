# Instalación libreria, solo una vez 
install.packages("rakeR")

# Libreria 
library(rakeR)
library(RPostgres)

# CENSO
cons_censo_df <- readRDS("data/cons_censo_df.rds")
casen_raw = readRDS("data/casen_rm.rds")

# 3. PREPROCESAMIENTO

## 3.1 CENSO

# Ordenar y extraer una sola vez los nombres de las columnas de constraints
col_cons   = sort(setdiff(names(cons_censo_df), c("GEOCODIGO","COMUNA")))

# De ahí generar dinámicamente los niveles que luego deben coincidir con los factor levels
age_levels  = grep("^edad", col_cons, value = TRUE)    # p.ej. "edad_menor_30", "edad_30_40", …
esc_levels  = grep("^esco", col_cons, value = TRUE)    # p.ej. "esco_0","esco_1_8",…
sexo_levels = grep("^sexo_",col_cons, value = TRUE)    # p.ej. "sexo_f","sexo_m"

## 3.2 CASEN

# Se seleccionan variables de interés

vars_base = c("estrato", # Para extraer la comuna
              "esc", # Para escolaridad
              "edad", 
              "sexo",
              "e6a",
              "ypc",
              "s13") # Variable a microsimular s13


# Se filtra la CASEN con las variables de interés
casen = casen_raw[ , vars_base, drop = FALSE]

# Se limpia memoria
rm(casen_raw)


# Extraemos la comuna
casen$Comuna = substr(as.character(casen$estrato), 1, 5)
casen$estrato = NULL

# Se quitan etiquetas haven y se cambian tipos de datos
casen$esc = as.integer(unclass(casen$esc))
casen$edad = as.integer(unclass(casen$edad))
casen$e6a = as.numeric(unclass(casen$e6a))
casen$sexo = as.integer(unclass(casen$sexo))
casen$ypc = as.numeric(unclass(casen$ypc)) 
casen$s13 = as.integer(unclass(casen$s13)) # Variable a estudiar 


# Imputación lineal de esc en base a e6a
idx_na = which(is.na(casen$esc))

# Ajustar modelo con casos en donde no hay na's
fit = lm(esc ~ e6a, data = casen[-idx_na,])

# Predicción para los casos con NA
pred = predict(fit, newdata = casen[idx_na, ,drop = FALSE])

# Imputar acotada
casen$esc[idx_na] = as.integer(round(pmax(0, pmin(29, pred))))

# Le añadimos un ID fijo
casen$ID = as.character(seq_len(nrow(casen)))

## Recodificamos 

casen$edad_cat <- cut(
  casen$edad,
  breaks = c(0,30,40,50,60,70,80,Inf),
  labels = age_levels,
  right = FALSE, include.lowest = TRUE
)

casen$esc_cat <- factor(
  with(casen,
       ifelse(esc == 0,           esc_levels[1],
              ifelse(esc <= 8,    esc_levels[2],
                     ifelse(esc <= 12, esc_levels[3],
                            esc_levels[4])))),
  levels = esc_levels
)

casen$sexo_cat <- factor(
  ifelse(casen$sexo == 2, sexo_levels[1],  
         ifelse(casen$sexo == 1, sexo_levels[2], NA)), 
  levels = sexo_levels
)

## Microsimulación
# crear la lista de constraints POR COMUNA
cons_censo_comunas = split(cons_censo_df, cons_censo_df$COMUNA)

# Lista de INDS 
inds_list = split(casen, casen$Comuna)

sim_list = lapply(names(cons_censo_comunas), function(zona) {
  cons_i    = cons_censo_comunas[[zona]]
  col_order = sort(setdiff(names(cons_i), c("COMUNA","GEOCODIGO")))
  cons_i    = cons_i[, c("GEOCODIGO", col_order), drop = FALSE]
  
  tmp    = inds_list[[zona]]
  inds_i = tmp[, c("ID","edad_cat","esc_cat","sexo_cat"), drop = FALSE]
  names(inds_i) = c("ID","Edad","Escolaridad","Sexo")
  
  
  w_frac  = weight(cons = cons_i, inds = inds_i,
                   vars = c("Edad","Escolaridad","Sexo"))
  sim_i   = integerise(weights = w_frac, inds = inds_i, seed = 123)
  merge(sim_i,
        tmp[, c("ID","s13", "sexo_cat")],
        by = "ID", all.x = TRUE)
})

# Data Frame de toda la población
sim_df = data.table::rbindlist(sim_list, idcol = "COMUNA")

# Imputamos un ingreso a cada persona, de sim_df
# si son valores grandes, hacer mediana o media para la representacion geografica
# si obtenemos 1 y 0 solo sumamos segun la variable

# Variable dummy: fonasa = 1 si esta afiliada a FONASA 
sim_df$fonasa = ifelse(sim_df$s13 == 1 & sim_df$sexo_cat == "sexo_f", 1, 0)
sim_df$es_mujer  = ifelse(sim_df$sexo_cat == "sexo_f", 1, 0)
# proporcion de mujeres en fonasa por comuna 
fonasa = aggregate(
  fonasa ~ COMUNA,
  data = sim_df,
  FUN  = function(x) mean(x, na.rm = TRUE)
)
names(fonasa) <- c("geocodigo", "fonasa")
#zonas_ypc = aggregate(
#  ypc ~ zone,
 # data = sim_df,
  #FUN  = function(x) median(x, na.rm = TRUE)
#)

# El grafico de mi variable (considerar que los varoles extremos arruinan la distribucion)

# Guardamos en base de datos

con <- dbConnect(
  Postgres(),
  dbname   = "censo_rm_2017",
  host     = "localhost",
  port     = 5432,
  user     = "postgres",
  password = "postgres"
)

fonasa$geocodigo <- as.character(fonasa$geocodigo)

# la tabla adjuntada la crea con ese esquema y ese nombre 
# 5. Guardar en base de datos
dbWriteTable(
  conn      = con,
  name      = Id(schema = "dpa", table = "tmp_fonasa"),
  value     = fonasa,
  overwrite = TRUE,
  row.names = FALSE
)
dbExecute(con, "CREATE INDEX ON dpa.tmp_fonasa(geocodigo)")
dbExecute(con, "ANALYZE dpa.tmp_fonasa")

# hace el join de la geometria a la tabla que hice en R 
# 1) Crea la nueva capa directamente con un SELECT … LEFT JOIN
# aplico filtro de que pertenezcan al gran santiago 

# Unions espacil y filtrado Gran Santiago
# En SQL
dbExecute(con, "
  CREATE TABLE dpa.zonas_gs_fonasa AS
  SELECT
    z.*,
    t.fonasa
  FROM dpa.zonas_censales_rm AS z
  LEFT JOIN dpa.tmp_fonasa AS t
    ON z.geocodigo::text = t.geocodigo
  WHERE urbano = 1 AND (nom_provin = 'SANTIAGO' OR nom_comuna = 'SAN BERNARDO' OR nom_comuna = 'PUENTE ALTO')
")
# APERTURA DE QGIS , para ver si funciono la simulacion 

# Ahora sigue lo mismo que se hizo en el trabajo 1. Mostrar mapa. 
# SELCCION DE VARIABLE Y PORCENTAJE DE PERSONAS QUE RESPONDEN 
# HAY QUE RECODIFICAR 

