# ==============================================================================
# PROYECTO FINAL ESTADÍSTICA APLICADA - MIAAD
# ==============================================================================

# 1. IMPORTACIÓN DE PAQUETES Y DATOS
# ------------------------------------------------------------------------------
if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse, 
  lubridate,
  janitor 
)

# Importar el conjunto de datos CSV (está en el directorio del proyecto)
df_creditos_raw <- read_csv("Cred_por_sector.csv")

# Verificación inicial
message("Dimensiones y Estructura inicial del dataset:")
print(dim(df_creditos_raw))
print(glimpse(df_creditos_raw))


# 2. PREPROCESAMIENTO: LIMPIEZA, ORDENACIÓN Y TRANSFORMACIÓN
# ------------------------------------------------------------------------------

df_final <- df_creditos_raw %>%
  # Paso 1: Limpieza de nombres de columnas (limpia espacios, mayúsculas, etc.)
  clean_names() %>%
  
  # Paso 2: Filtrar por sector AGRICULTURA
  filter(sector_e == "AGRICULTURA") %>%
  
  # Paso 3: Conversión de la columna 'fecha' a formato Date (YYYY-MM-01)
  mutate(
    fecha = ymd(paste0(fecha, "/01")) 
  ) %>%
  
  # Paso 4: Eliminar la columna 'sector_e' 
  select(-sector_e) %>% 
  
  # Paso 5: Transponer/Pivotar a formato largo 
  # Se usan todas las columnas que no son 'fecha' o 'moneda'
  pivot_longer(
    cols = -c(fecha, moneda),
    names_to = "banco",
    values_to = "monto_credito"
  ) %>%
  
  # Paso 6: Reemplazar y fusionar nombres de bancos
  mutate(
    banco = case_when(
      banco == "gnb_fusion" ~ "gnb",
      banco == "regional" ~ "sudameris",
      banco == "vision" ~ "ueno",
      TRUE ~ banco # Mantener el resto de nombres sin cambios
    ),
    # Asegurar que el monto sea numérico
    monto_credito = as.numeric(monto_credito) 
  ) %>%
  
  # Paso 7: Agrupar la data por la fusión de bancos
  group_by(fecha, moneda, banco) %>%
  summarise(
    monto_credito = sum(monto_credito, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  
  # Paso 8: Filtrar valores extremos/inconsistentes (0.0) [Necesario para evitar sesgos]
  filter(monto_credito > 0)


# Paso Final: Verificación post-limpieza
message("\nDimensiones y Estructura del dataset FINAL (limpio y largo):")
print(dim(df_final))
print(glimpse(df_final))

# 3. EXPLORACION DE DATOS
# ------------------------------------------------------------------------------

# === ANÁLISIS DESCRIPTIVO POR MONEDA ===

# Usamos group_by y summarise para obtener estadísticas clave
df_resumen_moneda <- df_final %>%
  group_by(moneda) %>%
  summarise(
    Conteo_Total = n(),
    Monto_Total = sum(monto_credito),
    Monto_Promedio = mean(monto_credito),
    Monto_Mediana = median(monto_credito),
    Desviacion_Std = sd(monto_credito)
  )

message("\nResumen Estadístico por Moneda:")
print(df_resumen_moneda)

# Utilizacion gt para una tabla profesional
if(!require(gt)) install.packages("gt")
library(gt)

df_resumen_moneda %>%
  gt() %>%
  tab_header(
    title = "Resumen de Créditos Agrícolas por Tipo de Moneda"
  ) %>%
  fmt_number(
    columns = starts_with("Monto_"),
    decimals = 0,
    use_seps = TRUE
  )

# === RANKING DE BANCOS ===

df_top_bancos <- df_final %>%
  group_by(banco) %>%
  summarise(Monto_Total_Credito = sum(monto_credito), .groups = 'drop') %>%
  arrange(desc(Monto_Total_Credito)) %>%
  head(5)

# 
tabla_top_bancos <- df_top_bancos %>%
  gt() %>%
  tab_header(
    title = "Top 5 Bancos con Mayor Monto Total de Crédito Agrícola",
    subtitle = "Periodo Analizado"
  ) %>%
  # Formato: Monto Total de Crédito sin decimales y con separadores de miles
  fmt_number(
    columns = Monto_Total_Credito,
    decimals = 0,
    use_seps = TRUE # Activa la separación de miles
  ) %>%
  # Etiquetas de columna más claras
  cols_label(
    banco = "Banco",
    Monto_Total_Credito = "Monto Total Crédito"
  ) %>%
  # Añadir estilo de celda
  data_color(
    columns = Monto_Total_Credito,
    palette = "Greens"
  )

# Mostrar la tabla en la consola o en el visor de RStudio
print(tabla_top_bancos)

#Visualización A: Evolución del Crédito Agrícola por Moneda (Series de Tiempo)
#monto total de crédito ha evolucionado a lo largo del tiempo, separando por el tipo de moneda (MN vs. ME), 
#crucial para la justificación de la hipótesis.

# === GRÁFICO 1: EVOLUCIÓN TEMPORAL POR MONEDA ===

grafico_tiempo <- df_final %>%
  group_by(fecha, moneda) %>%
  summarise(credito_total = sum(monto_credito), .groups = 'drop') %>%
  
  ggplot(aes(x = fecha, y = credito_total, color = moneda)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::comma) + # Formato legible para el eje Y
  labs(
    title = "Evolución Mensual del Crédito Agrícola",
    subtitle = "Total por Moneda Nacional (MN) vs. Moneda Extranjera (ME)",
    x = "Fecha",
    y = "Monto Total de Crédito",
    color = "Tipo de Moneda"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

print(grafico_tiempo)


#Visualización B: Distribución de Crédito por Banco y Moneda
#Gráfico de barras apiladas o facetado 
#permite visualizar la participación de cada banco y cómo se distribuye su crédito entre MN y ME.

# === GRÁFICO 2: PARTICIPACIÓN BANCARIA POR MONEDA ===

grafico_bancos <- df_final %>%
  # Agrupamos la data para tener la participación total de cada banco
  group_by(banco, moneda) %>%
  summarise(credito_total = sum(monto_credito), .groups = 'drop') %>%
  
  # Ordenamos los bancos por el monto total para mejor lectura
  mutate(banco = fct_reorder(banco, credito_total, .fun = sum)) %>%
  
  ggplot(aes(x = banco, y = credito_total, fill = moneda)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() + # Para mejor lectura de nombres de bancos
  labs(
    title = "Participación Total de Bancos en Crédito Agrícola",
    subtitle = "Separado por Tipo de Moneda",
    x = "Banco",
    y = "Monto Total de Crédito",
    fill = "Moneda"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

print(grafico_bancos)

# ==============================================================================
# PRUEBA 1: T-TEST (COMPARANDO MONEDA NACIONAL vs. MONEDA EXTRANJERA)
# ==============================================================================

# Hipótesis: H0: No hay diferencia en el monto promedio entre MN y ME.

message("\n--- RESULTADO DEL T-TEST (MN vs. ME) ---")

# 1. Verificación de Homogeneidad de Varianzas (Test de Levene)
if(!require(car)) install.packages("car")
library(car)

levene_test_mn_me <- leveneTest(monto_credito ~ moneda, data = df_final)
# Asumimos var.equal=FALSE (Welch's t-test) si el p-valor de Levene es bajo (<0.05).
var_iguales <- levene_test_mn_me$`Pr(>F)`[1] >= 0.05
message(paste("Varianzas Iguales (Levene p-valor >= 0.05):", var_iguales))

# 2. Ejecución del T-Test (usando Welch's T-Test por defecto para robustez)
t_test_resultado <- t.test(monto_credito ~ moneda, data = df_final, var.equal = var_iguales)

print(t_test_resultado)

# 3. Interpretación Narrativa
p_val_ttest <- t_test_resultado$p.value
media_mn <- df_final %>% filter(moneda == "MN") %>% pull(monto_credito) %>% mean()
media_me <- df_final %>% filter(moneda == "ME") %>% pull(monto_credito) %>% mean()

message("\n--- INTERPRETACIÓN T-TEST ---")
if (p_val_ttest < 0.05) {
  message(paste("Conclusión: Se RECHAZA la Hipótesis Nula. Existe una diferencia estadísticamente significativa en el monto promedio de crédito entre Moneda Nacional y Moneda Extranjera (P-valor =", round(p_val_ttest, 10), ")."))
} else {
  message(paste("Conclusión: NO se puede rechazar la Hipótesis Nula. No existe una diferencia significativa (P-valor =", round(p_val_ttest, 10), ")."))
}
message(paste("Media MN:", scales::comma(media_mn, accuracy = 0.01), " | Media ME:", scales::comma(media_me, accuracy = 0.01)))


# ==============================================================================
# ️ CREACIÓN DE df_final_me (Para el análisis ANOVA)
# ==============================================================================

message("\n--- CREANDO DATAFRAME FILTRADO POR MONEDA EXTRANJERA (ME) ---")

df_final_me <- df_final %>%
  # Filtramos el dataframe completo (df_final) para quedarnos solo con ME
  filter(moneda == "ME")

# Identificamos los 5 principales bancos DENTRO de la muestra de ME
top_5_bancos_me <- df_final_me %>%
  group_by(banco) %>%
  summarise(Monto_Total_Credito = sum(monto_credito), .groups = 'drop') %>%
  arrange(desc(Monto_Total_Credito)) %>%
  head(5) %>%
  pull(banco)

# Filtramos el dataframe ME para incluir solo esos 5 bancos
df_anova_me <- df_final_me %>%
  filter(banco %in% top_5_bancos_me)

message(paste("Dimensiones del df_anova_me (Top 5 Bancos en ME):", dim(df_anova_me)[1], "filas y", dim(df_anova_me)[2], "columnas"))


# ==============================================================================
# PRUEBA 2: ANOVA (COMPARANDO MONTO PROMEDIO ENTRE TOP 5 BANCOS EN ME)
# ==============================================================================

# Hipótesis: H0: No hay diferencia en el monto promedio de crédito en ME entre el Top 5 de bancos.

message("\n--- RESULTADO DE LA PRUEBA ANOVA (en ME) ---")

# 1. VERIFICACIÓN DE HOMOGENEIDAD DE VARIANZAS (Levene)
levene_anova_me <- leveneTest(monto_credito ~ banco, data = df_anova_me)
print(levene_anova_me)

# 2. EJECUCIÓN DE LA PRUEBA ANOVA
modelo_anova_me <- aov(monto_credito ~ banco, data = df_anova_me)
resultado_anova <- summary(modelo_anova_me)

print(resultado_anova)
p_valor_anova <- resultado_anova[[1]]$`Pr(>F)`[1]

# 3. ANÁLISIS POST-HOC (Tukey HSD)
message("\n--- ANÁLISIS POST-HOC (Tukey HSD en ME) ---")

if (p_valor_anova < 0.05) {
  print(TukeyHSD(modelo_anova_me))
  message("\nConclusión ANOVA: Se RECHAZA H0. Existe diferencia significativa en el monto promedio de crédito en ME entre al menos dos de los Top 5 bancos. El análisis Post-Hoc (Tukey HSD) muestra cuáles pares son diferentes.")
} else {
  message("\nConclusión ANOVA: NO se puede rechazar H0. Las medias de los Top 5 bancos en ME son estadísticamente iguales.")
}


# === GRÁFICO 1: BOXPLOT DE DISTRIBUCIÓN POR BANCO (ME) ===

#El boxplot muestra la distribución completa de los montos de crédito para cada banco, 
#incluyendo la mediana (línea central), los cuartiles (la caja) y los valores atípicos (puntos).

#Insight: Es ideal para visualizar la variabilidad y la asimetría dentro de cada banco y si sus medianas se solapan
#(lo que indicaría que no hay diferencia significativa).

grafico_boxplot_anova <- df_anova_me %>%
  ggplot(aes(x = fct_reorder(banco, monto_credito, .fun = median, .desc = TRUE), 
             y = monto_credito)) +
  geom_boxplot(aes(fill = banco), outlier.alpha = 0.3) + # outlier.alpha para ver los puntos atípicos
  scale_y_continuous(labels = scales::comma) + 
  labs(
    title = "Distribución del Monto de Crédito Agrícola (ME)",
    subtitle = "Comparación de la Variabilidad entre los 5 Principales Bancos",
    x = "Banco",
    y = "Monto de Crédito",
    fill = "Banco"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Gira las etiquetas del eje X
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none" # La leyenda es redundante con el eje X y el color
  )

print(grafico_boxplot_anova)


# === GRÁFICO 2: BARRAS DE MEDIA CON INTERVALOS DE CONFIANZA (IC) ===

#Este gráfico se enfoca directamente en la medida central (la media), que es lo que compara la ANOVA.
#Insight: Permite ver rápidamente qué bancos tienen la media más alta y, lo más importante, 
#las barras de error (intervalos de confianza) indican la precisión de esa media.
#Si los intervalos de confianza de dos bancos no se solapan, 
#es una fuerte indicación visual de que la diferencia es estadísticamente significativa

grafico_barras_ic <- df_anova_me %>%
  # Calculamos la media y el error estándar (SE) para las barras de error
  group_by(banco) %>%
  summarise(
    media_credito = mean(monto_credito),
    se_credito = sd(monto_credito) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  # Ordenamos los bancos por su media para el gráfico
  mutate(banco = fct_reorder(banco, media_credito)) %>% 
  
  ggplot(aes(x = banco, y = media_credito)) +
  # Barras que muestran la Media
  geom_bar(stat = "identity", fill = "#0072B2") + 
  # Barras de Error (Usando el 95% IC: Media ± 1.96 * SE)
  geom_errorbar(
    aes(ymin = media_credito - 1.96 * se_credito, 
        ymax = media_credito + 1.96 * se_credito),
    width = 0.2, 
    linewidth = 1
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Monto Promedio de Crédito (ME) con Intervalos de Confianza al 95%",
    subtitle = "La no-superposición de barras sugiere diferencia significativa.",
    x = "Banco",
    y = "Monto Promedio de Crédito"
  ) +
  coord_flip() + # Para mejor lectura
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0))

print(grafico_barras_ic)
