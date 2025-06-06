# =============================================================================
# 1) CARGAR LIBRERÍAS NECESARIAS
# =============================================================================
library(DBI)
library(RPostgres)
library(sf)
library(ggplot2)
library(cowplot)
library(biscale)

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
# 4) CARGAR GEOMETRÍA DE ZONAS CENSALES
# =============================================================================
sql_geometria <- "
SELECT
  geocodigo::double precision AS geocodigo,
  geom
FROM dpa.zonas_censales_rm
WHERE nom_provin = 'SANTIAGO' AND urbano = 1;
"
sf_zonas <- st_read(con, query = sql_geometria)
