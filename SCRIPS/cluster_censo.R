# =============================================================================
# 1) CARGAR LIBRERÍAS NECESARIAS
# =============================================================================

library(DBI)
library(RPostgres)
library(sf)
library(ggplot2)
library(cowplot)
library(factoextra)
library(ggfortify)
library(GGally)

# =============================================================================
# 2) CONEXIÓN A BASE DE DATOS
# =============================================================================
con <- dbConnect(
  Postgres(),
  dbname   = "censo_rm_2017",
  host     = "localhost",
  port     = 5432,
  user     = "postgres",
  password = "postgres"
)

# =============================================================================
# 3) CONSULTA SQL
# =============================================================================
sql_indicadores <- "
SELECT
  z.geocodigo::double precision AS geocodigo,
  c.nom_comuna,

  -- Porcentaje de migrantes
  ROUND(
    COUNT(*) FILTER (WHERE p.p12 NOT IN (1, 2, 98, 99)) * 100.0
    / NULLIF(COUNT(*), 0),
  2) AS ptje_migrantes,

  -- Porcentaje de personas con escolaridad mayor a 16 años
  ROUND(
    COUNT(*) FILTER (WHERE p.escolaridad >= 16) * 100.0
    / NULLIF(COUNT(*) FILTER (WHERE p.escolaridad IS NOT NULL), 0),
  2) AS ptje_esc_mayor_16,

  -- Porcentaje de adultos mayores
  ROUND(
    COUNT(*) FILTER (WHERE p.p09 >= 65) * 100.0
    / NULLIF(COUNT(*) FILTER (WHERE p.p09 IS NOT NULL), 0),
  2) AS ptje_adulto_mayor

FROM public.personas   AS p
JOIN public.hogares    AS h ON p.hogar_ref_id    = h.hogar_ref_id
JOIN public.viviendas  AS v ON h.vivienda_ref_id = v.vivienda_ref_id
JOIN public.zonas      AS z ON v.zonaloc_ref_id  = z.zonaloc_ref_id
JOIN public.comunas    AS c ON z.codigo_comuna   = c.codigo_comuna

GROUP BY z.geocodigo, c.nom_comuna
ORDER BY ptje_esc_mayor_16 DESC;
"
df_indicadores <- dbGetQuery(con, sql_indicadores)

# =============================================================================
# 4) Seleccionar variables y escalar 

vars_clusters = df_indicadores[,c("ptje_migrantes", "ptje_esc_mayor_16", "ptje_adulto_mayor")]

# se escalan las variables 
vars_scaled = scale(vars_clusters)

# 5) metodo del codo para elegir k 
fviz_nbclust(vars_scaled, kmeans, method = "wss")
  labs(tittle = "Metodo del codo", x = "Numero de clusteres (k)" , y = "wss")
# CODO EN EL 4, mucha mas elevada la pendiente 
  
# K-mean 
set.seed(123)
km = kmeans(vars_scaled, centers = 4, nstart = 25)

df_indicadores$cluster = as.factor(km$cluster)

# Escolaridad vs Migracion 
# Ingreso vs Score
ggplot(df_indicadores, aes(x = ptje_esc_mayor_16, y = ptje_migrantes, color = cluster)) +
  geom_point(size = 2) +
  labs(title = "Escolaridad v/s Migrantes",
       x = "% poblacion con 16 años de escolaridad",
       y = "% población migrante" ) +
  theme_minimal()
## Resultados con 12 años de escolaridad 
  # C1 = escolaridad Media Baja, Pocos Migrantes, Pobacion joven  
  # C2 = Escolaridad Media Alta, Alta Migracion, Población joven 
  # C3 = Escolaridad Alta, Migración Media Baja, Población Mayor  
  # C4 = Escolaridad Baja, Migracion Media Baja, Poblacion Mayor 

## Resultados con 16 años de escolaridad 
  # 

if (!require("plotly")) install.packages("plotly", dependencies = TRUE)
library(plotly)

# Crear gráfico 3D
fig <- plot_ly(
  data = df_indicadores,
  x = ~ptje_migrantes,
  y = ~ptje_esc_mayor_16,
  z = ~ptje_adulto_mayor,
  color = ~as.factor(cluster),  # Asegurarse de que sea un factor para colorear por grupo
  colors = c("red", "green", "blue", "purple"), # puedes ajustar esto
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 4)
)
# Mostrar el gráfico
fig

# Seleccionar variables más el cluster como factor
df_plot = df_indicadores[, c("ptje_migrantes", "ptje_esc_mayor_16", "ptje_adulto_mayor", "cluster")]

# Gráfico de colrrelaciones
p = ggpairs(
  df_plot,
  columns = 1:3,
  mapping = aes(color = cluster), 
  upper = list(continuous = "points"),
  lower = list(continuous = "points"),
  diag  = list(continuous = "densityDiag")
)

print(p)

## Conclusiones del grafico, con 12 años de escolarida 
# C1 (ROJO) --> Bajo % de migrantes, Meadia Baja % escolaridad y bajo % adulto mayor (jovenes)
# C2 --> Migracion Media, escolaridad alta y poblacion joven 
# C3 --> Migracion Alta, Escolaridad Alta y Poblacion Mayor 
# C4 --> Migracion baja, escolaridad baja y poblacion mayor 

## Conclusiones del grafico, con 16 años de escolaridad 
# C1 (ROJO) --> Bajo % de migrantes, Baja % escolaridad y adulto mayor
# C2 (VERDE)--> Migracion alta, escolaridad Alta y poblacion mayor 
# C3 (CELESTE) --> Migracion media, Escolaridad Variable y Poblacion joven 
# C4 (MORADO) --> Migracion baja, escolaridad baja y poblacion joven  

## Si pasa la probabilidad variable en el trabajo, ver el cluster, ESTUDIAR EL POR QUÉ

# CONSULTA DE GEOMETRÍA
sql_geometria = "
SELECT
  geocodigo::double precision AS geocodigo,
  geom
FROM dpa.zonas_censales_rm
WHERE nom_provin = 'SANTIAGO'
  AND urbano     = 1;
"
## esta parte puede cambiar en el trabajo

# LEER CAPA GEOGRÁFICA
sf_zonas = st_read(con, query = sql_geometria)

# COMBINAR CON INDICADORES
sf_mapa = merge(
  x     = sf_zonas,
  y     = df_indicadores,
  by    = "geocodigo",
  all.x = FALSE
)

# EXPORTAR A GEOJSON PARA USAR EN QGIS
st_write(sf_mapa, "zonas_clusters.geojson", driver = "GeoJSON", delete_dsn = TRUE)


# Se obtiene geometría comunal para Santiago
sql_comunas = "
SELECT cut, nom_comuna, geom
FROM dpa.comunas_rm_shp
WHERE nom_provin = 'SANTIAGO';
"
# tambien cambiaria en el trabajo 
sf_comunas_santiago = st_read(con, query = sql_comunas)

# Calcular bounding box para limitar el mapa al área urbana de Santiago
## Delimita el zoom para hacer un mapa o 4 pares de coordenadas 
bbox = st_bbox(sf_mapa)

# Crear mapa de clusters
mapa_clusters = ggplot() +
  geom_sf(data = sf_mapa, aes(fill = cluster), color = NA) +
  geom_sf(data = sf_comunas_santiago, fill = NA, color = "black", size = 0.4) +
  # Agrega el nombre de la comuna dentro del mapa y donde coloco este 
  geom_sf_text(data = st_centroid(sf_comunas_santiago), aes(label = nom_comuna), size = 2, fontface = "bold") +
  scale_fill_brewer(palette = "Set2", name = "Cluster") +
  labs(
    title = "Mapa de Clusters de Zonas Censales",
    subtitle = "Provincia de Santiago, Región Metropolitana"
  ) +
  coord_sf(
    xlim = c(bbox["xmin"], bbox["xmax"]),
    ylim = c(bbox["ymin"], bbox["ymax"]),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(mapa_clusters)
