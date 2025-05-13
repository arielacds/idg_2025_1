SELECT 
  z.geocodigo::double precision AS geocodigo,
  c.nom_comuna,

  -- % mujeres 15–49 con educación básica (p15 <= 7)
  COALESCE(ROUND(
    COUNT(*) FILTER (
      WHERE p.p08 = 2 AND p.p09 BETWEEN 15 AND 49 AND p.p15 <= 7
    ) * 100.0 /
    NULLIF(COUNT(*) FILTER (
      WHERE p.p08 = 2 AND p.p09 BETWEEN 15 AND 49
    ), 0), 2), 0) AS educ_basica,

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
ORDER BY educ_basica DESC;