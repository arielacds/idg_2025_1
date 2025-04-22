# ==============================================================================
# Tarea con Encuesta CASEN, viernes 11/04
# Regresión Lineal para obtener el Ingreso

# ==============================================================================
# Carga de paquetes y Base de Datos 

# Instalación de paquetes para el desarrollo correcto (es opcional)
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
    ingreso = as.numeric(zap_labels(ypc)), # ingreso per capita
    educ = as.numeric(zap_labels(e6a)),  # nivel educativo mas alto al que asiste o asisitio 
    edad = as.numeric(zap_labels(edad)),  # edad de la persona
    tam_empresa = as.numeric(zap_labels(o25)), # tamaño empresa
    sexo = as.factor(zap_labels(sexo)))  # sexo (factor)

# ==============================================================================
# Limpieza de datos 

# Filtro de valores faltantes NA y atipicos 
casen = casen %>% 
  # eliminar registros con NA en cualquier variable
  filter(
    !is.na(ingreso) & ingreso > 0, # elimina ingresos nulos o negativos 
    !is.na(educ),
    !is.na(edad),
    !is.na(sexo),
    !is.na(tam_empresa), 
   edad >= 15, # edad minima laboral
   edad <= 65  # limite superior (jpreubilacion)
  ) %>%
  filter(ingreso < quantile(ingreso, 0.80))  # eliminamos los valores extremos 

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
  labs(
    title = "Ingreso según Nivel Educacional",
    x = "Nivel Educativo",
    y = "Ingreso"
  ) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_minimal()

# Boxplot de ingreso según sexo 
ggplot(casen, aes(x = as.factor(sexo), y = ingreso, fill = as.factor(sexo))) + 
  geom_boxplot() + 
  labs(
    title = "Distribución del Ingreso por Sexo",
    x = "Sexo",
    y = "Ingreso",
    fill = "Sexo"
  ) + 
  scale_x_discrete(labels = c("1" = "Hombre", "2" = "Mujer")) +
  scale_fill_manual(
    values = c("1" = "#F8766D", "2" = "#00BFC4"),
    labels = c("1" = "Hombre", "2" = "Mujer")
  ) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_minimal()

# ==============================================================================
# Matriz de Correlación

# Convertimos variables categóricas a numéricas para correlación
casen_cor = casen %>%
  mutate(
    sexo = as.numeric(sexo)
  )

# Calculamos la matriz de correlación
cor_matriz = cor(select(casen_cor, ingreso, educ, edad, sexo, tam_empresa))

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
modelo = lm(ingreso ~ tam_empresa +educ + edad + sexo , data = casen)

# Mostramos los resultados
summary(modelo)

# ==============================================================================
# Modelo logaritmico 

casen = casen %>%
  mutate(log_ingreso = log(ingreso))

modelo_log = lm(log_ingreso ~ tam_empresa + educ+edad + sexo, data = casen)
summary(modelo_log)
# ==============================================================================
# Diagnistico del Modelo

par(mfrow = c(2, 2))  # Disposición 2x2 para ver los cuatro gráficos juntos
plot(modelo)
par(mfrow = c(1, 1))  # Volver a disposición normal

# Diagnistico del Modelo log

par(mfrow = c(2, 2))  # Disposición 2x2 para ver los cuatro gráficos juntos
plot(modelo_log)
par(mfrow = c(1, 1))  # Volver a disposición normal
