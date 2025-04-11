# ==============================================================================
# Tarea con Encuesta CASEN, viernes 11/04
# Regresión Lineal para obtener el Ingreso

# ==============================================================================
# Carga de paquetes y Base de Datos 

# Instalación de paquetes para el desarrollo correcto
install.packages("tidyverse") # Manipulación y visualizacion de datos 
install.packages("haven") # Importacion de datos Stata
install.packages("corrplot") # Visualizacion de matrices de correlación 
install.packages("ggcorrplot") # Alternativa para las correlaciones 

# Carga de librerías 
library(tidyverse)
library(haven)
library(corrplot)
library(ggcorrplot)

# Cargar la base de datos .rds
ruta_rds = "data/casen_rm.rds"
casen_rm = readRDS(ruta_rds)

# ==============================================================================
# SELECCIÓN Y TRANSFORMACIÓN DE VARIABLES RELEVANTES

# Transformamos las variables de interés
casen = casen_rm %>% 
  transmute(
    ingreso = as.numeric(zap_labels(ytot)), # ingreso total
    educ = as.numeric(zap_labels(educ)), # nivel educativo
    edad = as.numeric(zap_labels(edad)), # edad de la persona
  #  n_ocupados = as.numeric(zap_labels(n_ocupados)), # n° ocupados en el hogar
    sexo = as.factor(zap_labels(sexo)), # sexo (factor)
    tot_hog = as.numeric(zap_labels(tot_hog))) # tamaño del hogar

# ==============================================================================
# Limpieza de datos 

# Filtro de valores faltantes y atipicos 
casen = casen %>% 
  filter(
    !is.na(ingreso) & ingreso > 0,
    !is.na(educ),
    !is.na(edad),
   # !is.na(n_ocupados),
    !is.na(sexo),
    !is.na(tot_hog),
   edad < 65
  ) %>%
  filter(ingreso < quantile(ingreso, 0.80)) # eliminamos los valores extremos 

# ==============================================================================
# Análisis de Exploratorios: Histogramas, Boxplots y densidades 

# Estadística descriptiva general de las variables a considerar 
summary(casen)

# Histograma de ingreso
ggplot(casen, aes(x = ingreso)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black") +
  labs(title = "Distribución del Ingreso", x = "Ingreso per cápita", y = "Frecuencia")

# Histograma de edad
ggplot(casen, aes(x = edad)) +
  geom_histogram(bins = 40, fill = "darkseagreen", color = "black") +
  labs(title = "Distribución de Edad", x = "Edad", y = "Frecuencia")

# Boxplot de ingreso según nivel educativo
ggplot(casen, aes(x = as.factor(educ), y = ingreso)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Ingreso según Nivel Educacional", x = "Nivel Educativo", y = "Ingreso")

# Boxplot de ingreso según sexo 
ggplot(casen, aes(x = sexo, y = ingreso, fill = sexo)) + 
  geom_boxplot() + 
  labs(title = "Distribución del Ingreso por Sexo", x = "Sexo", y = "Ingreso") + 
  theme_minimal()

# Densidad de ingreso segun sexo 
ggplot(casen, aes(x = ingreso, fill = sexo)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución del Ingreso por Sexo", x = "Ingreso", y = "Densidad")

# ==============================================================================
# Matriz de Correlación

# Convertimos variables categóricas a numéricas para correlación
casen_cor = casen %>%
  mutate(
    sexo = as.numeric(sexo)
  )

# Calculamos la matriz de correlación
cor_matriz = cor(select(casen_cor, ingreso, educ, edad, sexo, tot_hog))

# Visualizacion gráfica de la matriz de correlación
ggcorrplot(
  cor_matriz,
  method = "circle",
  type = "full",
  lab = TRUE,
  lab_size = 3,
  colors = c("red", "white", "blue"),
  title = "Matriz de Correlación entre Variables",
  ggtheme = theme_minimal()
)

# ==============================================================================
# Tabla de correlaciones ordenadas 

# Extraer correlaciones con la variable ingreso 
cor_y = cor_matriz["ingreso", ]
cor_df = data.frame(variable = names(cor_y), correlacion = cor_y) %>%
  filter(variable != "ingreso") %>%
  mutate(abs_cor = abs(correlacion)) %>%
  arrange(desc(abs_cor))

# Imprimir tabla 
print(cor_df)

# Visualización de las correlaciones ordenadas con ingreso
ggplot(cor_df, aes(x = reorder(variable, abs_cor), y = correlacion)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  geom_text(aes(label = round(correlacion, 2)), hjust = ifelse(cor_df$correlacion > 0, -0.2, 1.2)) +
  labs(
    title = "Correlación con el Ingreso per cápita (ypch)",
    x = "Variable",
    y = "Correlación"
  ) +
  theme_minimal()

# ==============================================================================
# Modelo de Regresióno Lineal 

# Convertimos variables categóricas a factor (revisión) 
casen = casen %>%
  mutate(
    sexo = as.factor(sexo)
  )

# Creamos modelo de regresión lineal
modelo = lm(ingreso ~ educ + edad + sexo + tot_hog, data = casen)

# Mostramos los resultados
summary(modelo)
