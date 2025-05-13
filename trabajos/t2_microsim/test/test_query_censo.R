# =============================================================================
# 1) INSTALAR PAQUETES (solo una vez)
# =============================================================================
# Estos paquetes permiten conexión a BD, manejo de geometrías y visualización
install.packages(c("DBI" ,"RPostgres", "sf", "ggplot2", "cowplot", "biscale"))

# =============================================================================
# 2) CARGAR LIBRERÍAS NECESARIAS
# =============================================================================

library(DBI)
library(RPostgres)
library(sf)
library(ggplot2)
library(cowplot)
library(biscale)

# =============================================================================
# 3) CONFIGURAR CONEXIÓN A BASE DE DATOS
# =============================================================================
# Definir parámetros de conexión
db_host     = "localhost"       # servidor de BD
db_port     = 5432                # puerto de escucha
db_name     = "censo_rm_2017"   # nombre de la base
db_user     = "postgres"        # usuario de conexión
db_password = "postgres"        # clave de usuario

# Establecer conexión usando RPostgres
con = dbConnect(
  Postgres(),
  dbname   = db_name,
  host     = db_host,
  port     = db_port,
  user     = db_user,
  password = db_password
)

query_censo = readLines("trabajos/t2_microsim/consulta_censo_microsim.sql") |> paste(collapse = "\n")
cons_df = dbGetQuery(con, query_censo)
