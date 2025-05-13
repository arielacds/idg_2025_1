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

  -- Razón educativa: mujeres con educación básica / mujeres con educación superior
  COALESCE(ROUND(
    COUNT(*) FILTER (
      WHERE p.p08 = 2 AND p.p09 BETWEEN 15 AND 49 AND p.p15 <= 7
    )::numeric /
    NULLIF(COUNT(*) FILTER (
      WHERE p.p08 = 2 AND p.p09 BETWEEN 15 AND 49 AND p.p15 > 7
    ), 0), 2), 0) AS razon_educativa,

  -- % viviendas con hacinamiento (v.ind_hacin_rec IN (2,4))
  COALESCE(ROUND(
    COUNT(*) FILTER (
      WHERE v.ind_hacin_rec IN (2,4)
    ) * 100.0 /
    NULLIF(COUNT(*), 0), 2), 0) AS hacinamiento

FROM public.personas AS p
JOIN public.hogares    AS h ON p.hogar_ref_id    = h.hogar_ref_id
JOIN public.viviendas  AS v ON h.vivienda_ref_id = v.vivienda_ref_id
JOIN public.zonas      AS z ON v.zonaloc_ref_id  = z.zonaloc_ref_id
JOIN public.comunas    AS c ON z.codigo_comuna   = c.codigo_comuna

GROUP BY z.geocodigo, c.nom_comuna
ORDER BY razon_educativa DESC;
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

# =============================================================================
# 5) COMBINAR DATOS TABULARES Y ESPACIALES
# =============================================================================
sf_mapa <- merge(x = sf_zonas, y = df_indicadores, by = "geocodigo", all.x = FALSE)

# =============================================================================
# 6) MAPAS TEMÁTICOS
# =============================================================================
map_razon <- ggplot(sf_mapa) +
  geom_sf(aes(fill = razon_educativa), color = "gray80") +
  labs(title = "Razón educativa (básica/superior) en mujeres 15–49 años", fill = "Razón") +
  theme_minimal()

map_hacinamiento <- ggplot(sf_mapa) +
  geom_sf(aes(fill = hacinamiento), color = "gray80") +
  labs(title = "Hacinamiento en viviendas", fill = "% Hacinamiento") +
  theme_minimal()

print(map_razon)
print(map_hacinamiento)

# =============================================================================
# 7) GRÁFICO DE DISPERSIÓN POR CUADRANTES
# =============================================================================
mediana_razon   <- median(sf_mapa$razon_educativa, na.rm = TRUE)
mediana_hacinamiento <- median(sf_mapa$hacinamiento, na.rm = TRUE)

sf_mapa$cuadrante <- with(sf_mapa, ifelse(
  razon_educativa >= mediana_razon & hacinamiento >= mediana_hacinamiento, 'Q1: Alta razón / Alto hacinamiento',
  ifelse(razon_educativa >= mediana_razon & hacinamiento <  mediana_hacinamiento, 'Q2: Alta razón / Bajo hacinamiento',
         ifelse(razon_educativa <  mediana_razon & hacinamiento <  mediana_hacinamiento, 'Q3: Baja razón / Bajo hacinamiento',
                'Q4: Baja razón / Alto hacinamiento'))))

colores_cuadrantes <- c(
  'Q1: Alta razón / Alto hacinamiento' = '#08519c',
  'Q2: Alta razón / Bajo hacinamiento' = '#6baed6',
  'Q3: Baja razón / Bajo hacinamiento' = '#eff3ff',
  'Q4: Baja razón / Alto hacinamiento' = '#bdd7e7'
)

grafico_cuadrantes <- ggplot(sf_mapa, aes(x = razon_educativa, y = hacinamiento, color = cuadrante)) +
  geom_point(size = 2) +
  geom_vline(xintercept = mediana_razon, linetype = "dashed") +
  geom_hline(yintercept = mediana_hacinamiento, linetype = "dashed") +
  scale_color_manual(values = colores_cuadrantes) +
  labs(x = "Razón educativa (básica/superior)", y = "% Hacinamiento", title = "Dispersión: razón educativa vs hacinamiento") +
  theme_minimal()

print(grafico_cuadrantes)

# =============================================================================
# 8) MAPA BIVARIADO CON BISCALE
# =============================================================================
sf_mapa_bi <- bi_class(sf_mapa, x = razon_educativa, y = hacinamiento, dim = 3, style = "jenks")

sql_comunas <- "
SELECT cut, nom_comuna, geom
FROM dpa.comunas_rm_shp
WHERE nom_provin = 'SANTIAGO';
"
sf_comunas_santiago <- st_read(con, query = sql_comunas)
sf_centroides <- st_centroid(sf_comunas_santiago)

caja <- st_bbox(sf_mapa_bi)

mapa_bivariado_etiquetas <- ggplot() +
  geom_sf(data = sf_mapa_bi, aes(fill = bi_class), color = NA) +
  geom_sf(data = sf_comunas_santiago, fill = NA, color = 'black', size = 0.3) +
  geom_sf_text(data = sf_centroides, aes(label = nom_comuna), size = 2.2, fontface = 'bold') +
  bi_scale_fill(pal = 'DkBlue', dim = 3) +
  labs(title = 'Mapa bivariado: razón educativa vs hacinamiento (mujeres 15–49)') +
  coord_sf(xlim = c(caja['xmin'], caja['xmax']), ylim = c(caja['ymin'], caja['ymax']), expand = FALSE) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

leyenda_bivariada <- bi_legend(
  pal = 'DkBlue', dim = 3,
  xlab = 'Razón educativa', ylab = '% Hacinamiento', size = 8
)

mapa_final <- ggdraw() +
  draw_plot(mapa_bivariado_etiquetas, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(leyenda_bivariada, x = 0.7, y = 0.05, width = 0.25, height = 0.25)

print(mapa_final)
