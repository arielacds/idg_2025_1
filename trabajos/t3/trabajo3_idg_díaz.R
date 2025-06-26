# =============================================================================
# Carga de librerias
# =============================================================================
library(DBI)          # Conexión a bases de datos
library(RPostgres)    # Driver PostgreSQL
library(sf)           # Manejo de geometría espacial
library(dplyr)        # Manipulación de datos
library(ggplot2)      # Gráficos base
library(factoextra)   # Método del codo
library(GGally)       # ggpairs

# =============================================================================
# Conexión base de datos 
# =============================================================================
con <- dbConnect(
  Postgres(),
  dbname = "censo_rm_2017",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "postgres"
)

# =============================================================================
# Consulta SQL de variables del CENSO, para complementar análisis
# =============================================================================
sql_indicadores <- "
SELECT
  z.geocodigo::double precision AS geocodigo,
  c.nom_comuna,
  --- Porcentaje de escolaridad >= 12
  ROUND(
    COUNT(*) FILTER (WHERE p.escolaridad >= 12) * 100.0
    / NULLIF(COUNT(*) FILTER (WHERE p.escolaridad IS NOT NULL), 0),
  2) AS esc_mayor_12
FROM public.personas   AS p
JOIN public.hogares    AS h ON p.hogar_ref_id = h.hogar_ref_id
JOIN public.viviendas  AS v ON h.vivienda_ref_id = v.vivienda_ref_id
JOIN public.zonas      AS z ON v.zonaloc_ref_id = z.zonaloc_ref_id
JOIN public.comunas    AS c ON z.codigo_comuna = c.codigo_comuna
GROUP BY z.geocodigo, c.nom_comuna
ORDER BY esc_mayor_12 DESC;
"
df_indicadores <- dbGetQuery(con, sql_indicadores)

# =============================================================================
# Consulta de variables simuladas en el trabajo2, Mujeres en FONASA y Mujeres en ISAPRE 
# Incorporando la simulación de la variable Ingreso (ypc)
# =============================================================================
sql_simuladas <- "
SELECT
  f.geocodigo::double precision AS geocodigo,
  f.mujeres_fonasa,
  m.mujeres_isapre,
  y.mediana_ingreso
FROM dpa.tmp_fonasa_rm  AS f
JOIN dpa.tmp_isapre_m   AS m ON f.geocodigo = m.geocodigo
JOIN dpa.tmp_ingreso_rm AS y ON f.geocodigo = y.geocodigo;
"
df_sim <- dbGetQuery(con, sql_simuladas)

# =============================================================================
# Unión de todo en solo una data 
# =============================================================================
df_clusters <- df_sim %>%
  left_join(df_indicadores, by = "geocodigo")

# =============================================================================
# Escalar y hacer clusters
# =============================================================================
vars_scaled <- scale(df_clusters[, c(
  "mujeres_fonasa",
  "mujeres_isapre",
  "mediana_ingreso",
  "esc_mayor_12"
)])

# Método del codo para determinar número óptimo de clusters
fviz_nbclust(vars_scaled, kmeans, method = "wss") +
  labs(title = "Método del Codo", x = "Número de Clusters", y = "WSS")

# Cluster con K-means
set.seed(123)
km <- kmeans(vars_scaled, centers = 3, nstart = 25)
# k = 3, despues de este no hay inclinaciones considerables
df_clusters$cluster <- as.factor(km$cluster)

# =============================================================================
# Carga y unión de zonas censales 
# =============================================================================
sql_geom <- "
SELECT geocodigo::double precision AS geocodigo, geom
FROM dpa.zonas_censales_rm
WHERE nom_provin = 'SANTIAGO' AND urbano = 1;
"
sf_zonas <- st_read(con, query = sql_geom)
sf_mapa <- merge(sf_zonas, df_clusters, by = "geocodigo")

# =============================================================================
# Gráficos de distribución por variables 
# =============================================================================
# Mujeres FONASA vs Ingreso
ggplot(df_clusters, aes(x = mujeres_fonasa, y = mediana_ingreso, color = cluster)) +
  geom_point(size = 2) +
  labs(
    title = "Ingreso vs Mujeres afiliadas a FONASA",
    x = "% Mujeres FONASA",
    y = "Mediana ingreso per cápita"
  ) +
  theme_minimal()

# Mujeres ISAPRE vs Ingreso
ggplot(df_clusters, aes(x = mujeres_isapre, y = mediana_ingreso, color = cluster)) +
  geom_point(size = 2) +
  labs(
    title = "Ingreso vs Mujeres afiliadas a ISAPRE",
    x = "% Mujeres ISAPRE",
    y = "Mediana ingreso per cápita"
  ) +
  theme_minimal()

# Mujeres ISAPRE vs Escolaridad
ggplot(df_clusters, aes(x = mujeres_isapre, y = ptje_esc_mayor_12, color = cluster)) +
  geom_point(size = 2) +
  labs(
    title = "Escolaridad vs Mujeres afiliadas a ISAPRE",
    x = "% Mujeres ISAPRE",
    y = "% Escolaridad ≥ 12 años"
  ) +
  theme_minimal()

# =============================================================================
# Mapa de clusters
# =============================================================================
sql_comunas <- "
SELECT cut, nom_comuna, geom
FROM dpa.comunas_rm_shp
WHERE nom_provin = 'SANTIAGO';
"
sf_comunas <- st_read(con, query = sql_comunas)
bbox <- st_bbox(sf_mapa)

ggplot() +
  geom_sf(data = sf_mapa, aes(fill = cluster), color = NA) +
  geom_sf(data = sf_comunas, fill = NA, color = "black", size = 0.4) +
  geom_sf_text(data = st_centroid(sf_comunas), aes(label = nom_comuna), size = 2) +
  scale_fill_brewer(palette = "Set2", name = "Cluster") +
  labs(title = "Clusters por afiliación femenina, ingreso y escolaridad",
       subtitle = "Zonas Censales Urbanas – Gran Santiago") +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE) +
  theme_void()

# =============================================================================
# Matriz de correlación 
# =============================================================================
df_plot <- df_clusters[, c(
  "mujeres_fonasa",
  "mujeres_isapre",
  "mediana_ingreso",
  "ptje_esc_mayor_12",
  "cluster"
)]

ggpairs(
  df_plot,
  columns = 1:5,
  mapping = aes(color = cluster),
  upper = list(continuous = "points"),
  lower = list(continuous = "points"),
  diag = list(continuous = "densityDiag")
)

# =============================================================================
# Promedio de los clusters (sirve para el análisis)
# =============================================================================
df_clusters %>%
  group_by(cluster) %>%
  summarise(
    ingreso = mean(mediana_ingreso, na.rm = TRUE),
    fonasa  = mean(mujeres_fonasa, na.rm = TRUE),
    isapre  = mean(mujeres_isapre, na.rm = TRUE),
    escolaridad = mean(ptje_esc_mayor_12, na.rm = TRUE)
  )
