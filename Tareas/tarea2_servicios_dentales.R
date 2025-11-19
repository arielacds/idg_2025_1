# --- LIBRERÍAS ---
library(haven)
library(ggplot2)
library(pROC)
library(corrplot)

# --- CARGA DE BASES ---
personas <- read_dta("data/datos_epf/base-personas-ix-epf-stata.dta")
gastos   <- read_dta("data/datos_epf/base-gastos-ix-epf-stata.dta")

# --- FILTRO: GRAN SANTIAGO Y PERSONAS PRINCIPALES (servicio) ---
personas_gs <- subset(personas, macrozona == 2 & sprincipal == 1)

# --- LIMPIEZA BÁSICA ---
valores_invalidos <- c(-99, -88, -77)
personas_gs <- subset(personas_gs,
                      !(edad %in% valores_invalidos) &
                        !(edue %in% valores_invalidos) &
                        ing_disp_hog_hd_ai >= 0)

# --- VARIABLES DERIVADAS ---
personas_gs$ing_pc <- personas_gs$ing_disp_hog_hd_ai / personas_gs$npersonas
personas_gs$grupo_escolaridad <- cut(personas_gs$edue,
                                     breaks = c(-Inf, 8, 12, 16, Inf),
                                     labels = c("Básica o menos", "Media-baja", "Media-alta", "Alta"),
                                     right = TRUE)

# --- GASTO DENTAL: unir por folio asegurando tipo compatible ---
codigo_dental <- "06.2.2.09.01"
gastos_dental <- subset(gastos, ccif == codigo_dental & macrozona == 2)

# Asegurar tipo character en ambas tablas
personas_gs$folio <- as.character(personas_gs$folio)
gastos_dental$folio <- as.character(gastos_dental$folio)

# Gasto total por hogar
gasto_por_folio <- aggregate(gasto ~ folio, data = gastos_dental, sum)
names(gasto_por_folio)[2] <- "gasto_dental"

# Unión por folio
gasto_por_folio$folio <- as.character(gasto_por_folio$folio)
personas_gs <- merge(personas_gs, gasto_por_folio, by = "folio", all.x = TRUE)

# Reemplazar NA por 0
personas_gs$gasto_dental[is.na(personas_gs$gasto_dental)] <- 0
personas_gs$incurre_gasto <- ifelse(personas_gs$gasto_dental > 0, 1, 0)

# --- BASE PRINCIPAL ---
df_dental <- subset(personas_gs,
                    !is.na(edad) & !is.na(grupo_escolaridad) &
                      !is.na(sexo) & !is.na(npersonas) & !is.na(ing_pc))

# --- EXPLORACIÓN INICIAL ---
hist(df_dental$gasto_dental, breaks = 30, col = "lightblue", main = "Gasto dental (sin limpieza)", xlab = "Gasto")
boxplot(gasto_dental ~ factor(sexo), data = df_dental, main = "Gasto por sexo", col = c("tomato", "lightgreen"))

plot(df_dental$edad, df_dental$gasto_dental, main = "Edad vs Gasto dental", xlab = "Edad", ylab = "Gasto", pch = 20, col = rgb(0,0,0,0.3))
lines(lowess(df_dental$edad, df_dental$gasto_dental), col = "red", lwd = 2)

plot(df_dental$ing_pc, df_dental$gasto_dental, main = "Ingreso vs Gasto dental", xlab = "Ingreso per cápita", ylab = "Gasto", pch = 20, col = rgb(0,0,1,0.3))
lines(lowess(df_dental$ing_pc, df_dental$gasto_dental), col = "blue", lwd = 2)

# --- LIMPIEZA DE OUTLIERS SOLO EN QUIENES SÍ GASTARON ---
df_dental_gastaron <- df_dental[df_dental$gasto_dental > 0, ]

limpiar_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lim_inf <- Q1 - 1.5 * IQR
  lim_sup <- Q3 + 1.5 * IQR
  return(x >= lim_inf & x <= lim_sup)
}

filtros <- with(df_dental_gastaron,
                limpiar_outliers(edad) &
                  limpiar_outliers(edue) &
                  limpiar_outliers(ing_pc) &
                  limpiar_outliers(gasto_dental) &
                  limpiar_outliers(npersonas))

df_dental_limpio <- df_dental_gastaron[filtros, ]

# --- EXPLORACIÓN LIMPIA ---
hist(df_dental_limpio$gasto_dental, breaks = 30, col = "lightgreen", main = "Gasto dental (limpio)", xlab = "Gasto")
boxplot(gasto_dental ~ factor(sexo), data = df_dental_limpio, main = "Gasto dental por sexo (limpio)", col = c("gold", "skyblue"))

plot(df_dental_limpio$edad, df_dental_limpio$gasto_dental, main = "Edad vs Gasto (limpio)", xlab = "Edad", ylab = "Gasto", pch = 20, col = rgb(0.3,0.1,0,0.3))
lines(lowess(df_dental_limpio$edad, df_dental_limpio$gasto_dental), col = "darkgreen", lwd = 2)

plot(df_dental_limpio$ing_pc, df_dental_limpio$gasto_dental, main = "Ingreso vs Gasto (limpio)", xlab = "Ingreso per cápita", ylab = "Gasto", pch = 20, col = rgb(0.2,0.2,0.5,0.3))
lines(lowess(df_dental_limpio$ing_pc, df_dental_limpio$gasto_dental), col = "blue", lwd = 2)

# --- MATRIZ DE CORRELACIÓN ---
df_cor <- data.frame(
  gasto = df_dental_limpio$gasto_dental,
  edad = df_dental_limpio$edad,
  escolaridad = df_dental_limpio$edue,
  ingreso = df_dental_limpio$ing_pc,
  sexo = df_dental_limpio$sexo,
  personas = df_dental_limpio$npersonas
)
cor_matrix <- cor(df_cor, use = "complete.obs", method = "pearson")
corrplot(cor_matrix, method = "color", tl.cex = 0.8, number.cex = 0.7)

# --- MODELO LOGIT: probabilidad de incurrir en gasto dental ---
modelo_logit <- glm(incurre_gasto ~ factor(sexo) + edad + grupo_escolaridad + ing_pc + npersonas,
                    data = df_dental, family = binomial)
summary(modelo_logit)

# --- MODELO LINEAL: monto gastado, solo con datos limpios ---
modelo_lineal <- lm(gasto_dental ~ edue + ing_pc + sexo + edad + npersonas,
                    data = df_dental_limpio)
summary(modelo_lineal)
