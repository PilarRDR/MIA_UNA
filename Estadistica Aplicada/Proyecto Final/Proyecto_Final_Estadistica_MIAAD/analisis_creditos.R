# ==============================================================================
# PROYECTO FINAL ESTADÍSTICA APLICADA - MIAAD
# ==============================================================================

# 1. IMPORTACIÓN DE PAQUETES Y DATOS
# ------------------------------------------------------------------------------
if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse, 
  lubridate,
  janitor,
  gt,
  car,     # Para test de Levene
  rstatix,# Para verificación de supuestos de normalidad
  ggpubr, # Para verificación de supuestos de normalidad
  scales   # Para formato
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

# === TABLA DE ESTADÍSTICOS DESCRIPTIVOS DETALLADOS POR BANCO Y MONEDA ===

# 1. Cálculo de las métricas clave (Media, SD, Quartiles)
descriptivos_detallados <- df_final %>%
  group_by(banco, moneda) %>%
  summarise(
    N_obs = n(),
    Media = mean(monto_credito),
    DS = sd(monto_credito),
    Q1 = quantile(monto_credito, 0.25),
    Mediana = median(monto_credito),
    Q3 = quantile(monto_credito, 0.75),
    .groups = 'drop'
  )

# 2. Presentación Profesional con gt
tabla_descriptiva_completa <- descriptivos_detallados %>%
  gt() %>%
  tab_header(
    title = "Estadísticas Descriptivas Detalladas de Desembolsos por Banco",
    subtitle = "Sector: Agricultura. Valores en Guaraníes (G)"
  ) %>%
  # Redondeo y formato de números grandes (en Millones de G)
  fmt_number(
    columns = c(Media, DS, Q1, Mediana, Q3),
    decimals = 0, # Mantenemos sin decimales ya que son valores grandes
    use_seps = TRUE # Usar separador de miles
  ) %>%
  # Etiquetado y Alineación
  cols_label(
    banco = "Banco",
    moneda = "Moneda",
    N_obs = "N",
    DS = "Desv. Est.",
    Q1 = "Cuartil 1 (Q1)",
    Mediana = "Mediana (Q2)",
    Q3 = "Cuartil 3 (Q3)"
  ) %>%
  # Añadir estilo visual (opcional)
  data_color(
    columns = c(Media, DS),
    palette = "Blues"
  )

# Mostrar la tabla en el visor/consola de RStudio
print(tabla_descriptiva_completa)

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

# 1. VERIFICACIÓN DE SUPUESTOS DEL T-TEST
# ------------------------------------------------------------------------------

# 1.1 Normalidad (Test de Shapiro-Wilk para cada grupo)
# El test de Shapiro es muy sensible con muestras grandes, por lo que el QQ Plot es clave.
message("\n--- 1. Normalidad (Shapiro-Wilk por grupo) ---")
normalidad_mn_me <- df_final %>%
  group_by(moneda) %>%
  rstatix::shapiro_test(monto_credito)
print(normalidad_mn_me)

message("\n--- Inspección Visual de Normalidad (QQ Plot) ---")
# Gráfico QQ para visualizar la normalidad en ambos grupos
normalidad_qq_plot <- ggplot(df_final, aes(sample = monto_credito, color = moneda)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(~ moneda) +
  labs(title = "QQ Plot de Monto de Crédito por Moneda")
print(normalidad_qq_plot)
# INTERPRETACIÓN: Los puntos deben seguir la línea diagonal.


# 1.2 Homogeneidad de Varianzas (Test de Levene)
levene_test_mn_me <- car::leveneTest(monto_credito ~ moneda, data = df_final)
message("\n--- 2. Homogeneidad de Varianzas (Levene) ---")
print(levene_test_mn_me)
# INTERPRETACIÓN: Si p-valor < 0.05, se rechaza H0 (varianzas NO iguales).
var_iguales <- levene_test_mn_me$`Pr(>F)`[1] >= 0.05
message(paste("Decisión sobre Varianza (p-valor Levene):", if (var_iguales) "Usar T-Test Clásico (var.equal=TRUE)" else "Usar T-Test de Welch (var.equal=FALSE)"))


# 2. Ejecución del T-Test
# ------------------------------------------------------------------------------
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
#La volumetria en ME son significativamente mas elevadas

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


#ANOVA de Welch (manteniendo el enfoque paramétrico pero ajustando la varianza)

## Ejecución del ANOVA de Welch y Post-Hoc (Games-Howell)

# ==============================================================================
# 2. EJECUCIÓN FINAL DE LA PRUEBA ANOVA DE WELCH (ROBUSTA)
# ==============================================================================

# La función oneway.test() realiza el ANOVA de Welch (var.equal=FALSE)
resultado_anova_welch <- oneway.test(monto_credito ~ banco, 
                                     data = df_anova_me, 
                                     var.equal = FALSE) 

message("=====================================================")
message("  RESULTADOS DEL ANOVA DE WELCH")
message("=====================================================")
print(resultado_anova_welch) 

p_valor_anova <- resultado_anova_welch$p.value 

# 3. ANÁLISIS POST-HOC (Games-Howell)
message("\n--- ANÁLISIS POST-HOC (Games-Howell en ME) ---")

if (p_valor_anova < 0.05) {
  # ---------------------------------------------------------------
  # PREPARACIÓN DE DATOS PARA LA TABLA INTUITIVA (USANDO GT)
  # ---------------------------------------------------------------
  tabla_games_howell_gt <- df_anova_me %>%
    rstatix::games_howell_test(monto_credito ~ banco) %>%
    
    # Seleccionar las columnas esenciales
    dplyr::select(group1, group2, estimate, conf.low, conf.high, p.adj) %>%
    dplyr::rename(
      `Diferencia Media` = estimate,
      `IC Inferior` = conf.low,
      `IC Superior` = conf.high,
      `P-valor Ajustado` = p.adj
    ) %>%
    dplyr::mutate(
      `Comparación` = paste(group1, " vs ", group2) # Formato de la columna de comparación
    ) %>%
    dplyr::select(-group1, -group2) # Eliminar columnas auxiliares
  
  # ---------------------------------------------------------------
  # GENERACIÓN DE LA TABLA PROFESIONAL CON 'GT'
  # ---------------------------------------------------------------
  tabla_gt_final <- tabla_games_howell_gt %>%
    gt::gt() %>%
    gt::tab_header(
      title = gt::md("**Diferencias Significativas Post-Hoc (Games-Howell)**"),
      subtitle = "Monto Promedio de Crédito (ME) por pares de Bancos"
    ) %>%
    # Formato para los montos (sin decimales)
    gt::fmt_number(
      columns = c(`Diferencia Media`, `IC Inferior`, `IC Superior`),
      decimals = 0,
      use_seps = TRUE 
    ) %>%
    # Formato para el P-valor (4 decimales)
    gt::fmt_number(columns = `P-valor Ajustado`, decimals = 4) %>%
    
    # Formato Condicional: Resaltar en verde si es significativo (P < 0.05)
    gt::tab_style(
      style = gt::cell_fill(color = "#C3E6CB"), 
      locations = gt::cells_body(
        columns = `P-valor Ajustado`,
        rows = `P-valor Ajustado` < 0.05
      )
    ) %>%
    # Combinar los intervalos de confianza en una sola columna
    gt::cols_merge(
      columns = c(`IC Inferior`, `IC Superior`),
      pattern = "[{1}, {2}]"
    ) %>%
    gt::cols_label(
      `IC Inferior` = "Intervalo de Confianza 95%"
    ) %>%
    gt::tab_footnote(
      footnote = gt::md("**Significativo** si P-valor Ajustado < 0.05"),
      locations = gt::cells_column_labels(columns = `P-valor Ajustado`)
    )
  
  # MOSTRAR LA TABLA PROFESIONAL EN EL VISOR DE RSTUDIO
  print(tabla_gt_final)
  
  message("\n**Conclusión ANOVA:** Se **RECHAZA H0**. Existe diferencia significativa en el monto promedio de crédito en ME entre al menos dos de los Top 5 bancos. El análisis Post-Hoc (**Games-Howell**) detalla cuáles pares son diferentes.")
} else {
  message("\n**Conclusión ANOVA:** NO se puede rechazar H0. Las medias de los Top 5 bancos en ME son estadísticamente iguales.")
}

# === GRÁFICO 1: BOXPLOT DE DISTRIBUCIÓN POR BANCO (ME) ===
grafico_boxplot_anova <- df_anova_me %>%
  ggplot(aes(x = fct_reorder(banco, monto_credito, .fun = median, .desc = TRUE), 
             y = monto_credito)) +
  geom_boxplot(aes(fill = banco), outlier.alpha = 0.3) + 
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
    axis.text.x = element_text(angle = 45, hjust = 1), 
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none" 
  )

print(grafico_boxplot_anova)


# === GRÁFICO 2: BARRAS DE MEDIA CON INTERVALOS DE CONFIANZA (IC) ===
grafico_barras_ic <- df_anova_me %>%
  group_by(banco) %>%
  summarise(
    media_credito = mean(monto_credito),
    se_credito = sd(monto_credito) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  mutate(banco = fct_reorder(banco, media_credito)) %>% 
  
  ggplot(aes(x = banco, y = media_credito)) +
  geom_bar(stat = "identity", fill = "#0072B2") + 
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
  coord_flip() + 
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0))

print(grafico_barras_ic)