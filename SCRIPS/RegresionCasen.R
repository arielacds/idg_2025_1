# ejercicio clase del 04-04-2025

# Entradas

ruta_rds = "data/casen_rm.rds"
casen_rm = readRDS(ruta_rds)

# TAREA HACER UNA REGRESION QUE TE DE EL INGRESO (modulo y), CON LAS VARIABLES QUE UNA DETERMINE
# ANALISIS EXPLORATORIO HASTA EL VIERNES 11 
# BETAS SIGNIFICATIVOS, R'S 
# variable independiente "YPC" = INGRESO 
# variable dependiente "ESC" = NIVEL DE ESCOLARIDAD / SEXO, agregar al final 
# LIMPIEZA DE DATAFRAME(ELIMINAR VARIABLES QUE NO ME SIRVEN Y VALORES ATIPICOS, PREGUNTAS SIN MUCHAS RESPUESTAS), 
# ANALISIS DE CORRELACION Y UNA REGRESION. 
# VISUALIZACION DE DATOS. 

# PRIMERO VER LOS ATIPICOS 
umbral = quantile(casen_rm$ypc , 0.95, na.rm = TRUE) 

