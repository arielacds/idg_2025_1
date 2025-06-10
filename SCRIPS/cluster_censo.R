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
# 3) CONSULTA SQL: RAZÓN EDUCATIVA Y HACINAMIENTO
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

  -- Porcentaje de personas con escolaridad mayor a 12 años
  ROUND(
    COUNT(*) FILTER (WHERE p.escolaridad >= 12) * 100.0
    / NULLIF(COUNT(*) FILTER (WHERE p.escolaridad IS NOT NULL), 0),
  2) AS ptje_esc_mayor_12,

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
ORDER BY ptje_esc_mayor_12 DESC;
"
df_indicadores <- dbGetQuery(con, sql_indicadores)

# =============================================================================
# 4) Seleccionar variables y escalar 

vars_clusters = df_indicadores[,c("ptje_migrantes", "ptje_esc_mayor_12", "ptje_adulto_mayor")]

# se escalan las variables 
vars_scaled = scale(vars_clusters)

# 5) metodo del codo para elegir k 
fviz_nbclust(vars_scaled, kmeans, method = "wss")
  labs(tittle = "Metodo del codo", x = "Numero de clusteres (k)" , y = "wss")
  
# K-mean 
set.seed(123)
km = kmeans(vars_scaled, centers = 4, nstart = 25)

df_indicadores$cluster = as.factor(km$cluster)

# Escolaridad vs Migracion 
# Ingreso vs Score
ggplot(df_indicadores, aes(x = ptje_esc_mayor_12, y = ptje_migrantes, color = cluster)) +
  geom_point(size = 2) +
  labs(title = "Escolaridad v/s Migrantes",
       x = "% poblacion con 12 años de escolaridad",
       y = "% población migrante" ) +
  theme_minimal()
## Resultados
  # C1 = escolaridad Media Baja, Pocos Migrantes, Pobacion joven  
  # C2 = Escolaridad Media Alta, Alta Migracion, Población joven 
  # C3 = Escolaridad Alta, Migración Media Baja, Población Mayor  
  # C4 = Escolaridad Baja, Migracion Media Baja, Poblacion Mayor 

if (!require("plotly")) install.packages("plotly", dependencies = TRUE)
library(plotly)

# Crear gráfico 3D
fig <- plot_ly(
  data = df_indicadores,
  x = ~ptje_migrantes,
  y = ~ptje_esc_mayor_12,
  z = ~ptje_adulto_mayor,
  color = ~as.factor(cluster),  # Asegurarse de que sea un factor para colorear por grupo
  colors = c("red", "green", "blue", "purple"), # puedes ajustar esto
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 4)
)

