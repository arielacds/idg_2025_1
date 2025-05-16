cons_censo_df <- readRDS("data/cons_censo_df.rds")
casen_raw = readRDS("data/casen_rm.rds")

# Cada registro de la case representa 1 perosna. ID, escolaridad, edad, sexo

# Sleccionar variables.. Se deben elimnar N.A

vars_base = c("estrato", # para extraer ID de comuna
              "esc", # Escolaridad
              "edad",
              "sexo",
              "e6a", # Imputar escolaridad
              "ypc") # Var a micro simular

# Flitrar CASEN
casen = casen_raw[ , vars_base, drop = FALSE]
rm(casen_raw) # Eliminar data sin utilizar

# Extraer comuna
casen$Comuna = substr(as.character(casen$estrato), 1, 5)
casen$estrato = NULL

# Se quitan etiquetas (transformar de heyven a dtaframe normal)
casen$e6a = as.integer(unclass(casen$e6a))
casen$ypc = as.integer(unclass(casen$ypc))
casen$Comuna = as.integer(unclass(casen$Comuna))
casen$sexo = as.integer(unclass(casen$sexo))
casen$edad = as.integer(unclass(casen$edad))