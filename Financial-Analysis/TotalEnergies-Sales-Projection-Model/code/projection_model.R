###############################################################################
#  regresion_totalenergies.R
#  TotalEnergies SE — Regresion OLS y proyeccion 2025-2034
#  Pasos 1-4: Dataset, Regresion, Scatter, Proyeccion 10yr
###############################################################################

# ===========================================================================
# PAQUETES
# ===========================================================================
options(repos = c(CRAN = "https://cloud.r-project.org"))
pkgs <- c("readxl", "dplyr", "tidyr", "ggplot2", "scales",
           "ggrepel", "stringr")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# ===========================================================================
# RUTAS — copiar a /tmp/ para evitar encoding macOS
# ===========================================================================
dir_origen <- getwd()
file.copy(file.path(dir_origen, 'Excel Total Energies.xlsx'),
          '/tmp/Excel_TE.xlsx', overwrite = TRUE)
file.copy(file.path(dir_origen, 'EIA_BrentCrudo_USD_Barril_1987-2024.xls'),
          '/tmp/EIA_Brent.xls', overwrite = TRUE)
file.copy(file.path(dir_origen, 'BM_TipoCambio_Oficial_LCU_USD.xls'),
          '/tmp/BM_TipoCambio.xls', overwrite = TRUE)

ruta_excel <- '/tmp/Excel_TE.xlsx'
ruta_brent <- '/tmp/EIA_Brent.xls'
ruta_tc    <- '/tmp/BM_TipoCambio.xls'

anios <- 2016:2024

# ===========================================================================
# FUNCIONES AUXILIARES
# ===========================================================================
leer_hoja <- function(sheet_name) {
  read_excel(ruta_excel, sheet = sheet_name, col_names = FALSE,
             .name_repair = 'minimal')
}

extraer_fila <- function(df, patron, cols_anio = 2:10) {
  idx <- grep(patron, as.character(df[[1]]), ignore.case = TRUE)
  if (length(idx) == 0) {
    warning(paste0("Patron '", patron, "' no encontrado"))
    return(rep(NA_real_, length(cols_anio)))
  }
  as.numeric(as.character(df[idx[1], cols_anio, drop = TRUE]))
}

# ===========================================================================
# TEMA VISUAL
# ===========================================================================
tema_tte <- theme_minimal(base_size = 11) +
  theme(
    plot.background   = element_rect(fill = 'white', color = NA),
    panel.background  = element_rect(fill = 'white', color = NA),
    panel.grid.major  = element_line(color = '#E8E8E8', linewidth = 0.4),
    panel.grid.minor  = element_blank(),
    plot.title        = element_text(face = 'bold', size = 14, color = '#1F3864'),
    plot.subtitle     = element_text(size = 10, color = '#595959'),
    plot.caption      = element_text(size = 8, color = '#888888'),
    legend.position   = 'bottom',
    legend.title      = element_blank()
  )

###############################################################################
# PASO 1 — LEER Y CONSTRUIR EL DATASET
###############################################################################
cat('\n###############################################################\n')
cat('  PASO 1 — LEER Y CONSTRUIR EL DATASET\n')
cat('###############################################################\n\n')

# --- Estado de resultados ---
er <- leer_hoja('Estado de resultados')
ventas_netas <- extraer_fila(er, 'Ventas netas')

# --- EBITDA: derivado de Ventas x Margen EBITDA (Ratios de Rentabilidad) ---
rent <- leer_hoja('Ratios de Rentabilidad')
margen_ebitda <- extraer_fila(rent, 'Margen EBITDA')
ebitda <- ventas_netas * margen_ebitda

cat('EBITDA derivado (Ventas x Margen EBITDA del Excel):\n')
for (i in seq_along(anios)) {
  cat(sprintf('  %d: Ventas=%8.0f  Margen=%.3f  EBITDA=%8.0f\n',
              anios[i], ventas_netas[i], margen_ebitda[i], ebitda[i]))
}

# --- Brent crudo (EIA) ---
brent_raw <- read_xls(ruta_brent, sheet = 'Data 1', col_names = FALSE,
                      .name_repair = 'minimal')
brent_all <- data.frame(
  serial = as.numeric(brent_raw[[1]][4:nrow(brent_raw)]),
  precio = as.numeric(brent_raw[[2]][4:nrow(brent_raw)])
)
brent_all$fecha <- as.Date(brent_all$serial, origin = '1899-12-30')
brent_all$year  <- as.integer(format(brent_all$fecha, '%Y'))
brent_anual <- brent_all |>
  filter(year >= 2016, year <= 2024) |>
  select(year, Brent = precio)

# --- Tipo de cambio EUR/USD (Banco Mundial — Francia) ---
tc_data <- read_xls(ruta_tc, sheet = 'Data', col_names = FALSE,
                    .name_repair = 'minimal')
idx_fr  <- grep('Francia|France', as.character(tc_data[[1]]), ignore.case = TRUE)
idx_fr  <- idx_fr[1]
cols_tc <- 61:69  # 2016-2024
eur_usd <- as.numeric(as.character(tc_data[idx_fr, cols_tc, drop = TRUE]))

# --- Construir dataframe combinado ---
df <- data.frame(
  Year             = anios,
  Ventas_M         = ventas_netas,
  EBITDA_M         = ebitda,
  Brent            = brent_anual$Brent,
  EUR_USD          = eur_usd,
  EBITDA_margin_pct = margen_ebitda * 100
)

# Fases historicas
df$Fase <- case_when(
  df$Year == 2016                       ~ 'F1 Colapso (2016)',
  df$Year >= 2017 & df$Year <= 2019     ~ 'F2 Adquisiciones (2017-19)',
  df$Year >= 2020 & df$Year <= 2021     ~ 'F3 COVID+Rebrand (2020-21)',
  df$Year >= 2022 & df$Year <= 2023     ~ 'F4 Ucrania (2022-23)',
  df$Year == 2024                       ~ 'F5 Transicion (2024)'
)
df$Fase <- factor(df$Fase, levels = c(
  'F1 Colapso (2016)', 'F2 Adquisiciones (2017-19)',
  'F3 COVID+Rebrand (2020-21)', 'F4 Ucrania (2022-23)',
  'F5 Transicion (2024)'
))

cat('\nDataframe completo 2016-2024:\n')
print(df, row.names = FALSE)

###############################################################################
# PASO 2 — REGRESION OLS
###############################################################################
cat('\n###############################################################\n')
cat('  PASO 2 — REGRESION OLS\n')
cat('###############################################################\n\n')

# --- Modelo A: Ventas_M ~ Brent ---
mod_a <- lm(Ventas_M ~ Brent, data = df)
cat('========== MODELO A: Ventas_M ~ Brent ==========\n')
print(summary(mod_a))

# --- Modelo B: EBITDA_margin_pct ~ Brent ---
mod_b <- lm(EBITDA_margin_pct ~ Brent, data = df)
cat('\n========== MODELO B: EBITDA_margin_pct ~ Brent ==========\n')
print(summary(mod_b))

# --- Correlaciones ---
cat('\n========== CORRELACIONES ==========\n')
mat_cor <- cor(df[, c('Brent', 'Ventas_M', 'EUR_USD')], use = 'complete.obs')
print(round(mat_cor, 4))

cat('\nNOTA: EUR/USD tiene correlacion alta con Brent (r =',
    round(mat_cor['Brent','EUR_USD'], 4),
    ')\n      Se reporta pero NO se incluye como variable adicional en la regresion.\n')



###############################################################################
# PASO 3 — GRAFICO 1: SCATTER VENTAS VS. BRENT
###############################################################################
cat('\n###############################################################\n')
cat('  PASO 3 — GRAFICO SCATTER VENTAS VS. BRENT\n')
cat('###############################################################\n\n')

coef_a   <- coef(mod_a)
r2_a     <- summary(mod_a)$r.squared
eq_label <- sprintf('Ventas = %.0f + %.0f x Brent\nR^2 = %.3f',
                     coef_a[1], coef_a[2], r2_a)

colores_fase <- c(
  'F1 Colapso (2016)'           = '#E74C3C',
  'F2 Adquisiciones (2017-19)'  = '#2ECC71',
  'F3 COVID+Rebrand (2020-21)'  = '#F39C12',
  'F4 Ucrania (2022-23)'        = '#8E44AD',
  'F5 Transicion (2024)'        = '#2980B9'
)

g1 <- ggplot(df, aes(x = Brent, y = Ventas_M)) +
  geom_smooth(method = 'lm', se = TRUE, level = 0.95,
              color = '#1F3864', fill = '#D6E4F0', linewidth = 1) +
  geom_point(aes(color = Fase), size = 4) +
  geom_text_repel(aes(label = Year, color = Fase),
                  size = 3.5, fontface = 'bold',
                  box.padding = 0.5, show.legend = FALSE) +
  scale_color_manual(values = colores_fase) +
  annotate('label', x = min(df$Brent) + 5, y = max(df$Ventas_M) - 5000,
           label = eq_label, hjust = 0, vjust = 1,
           size = 3.5, fill = '#F0F4F8', color = '#1F3864',
           fontface = 'italic', label.padding = unit(0.4, 'lines')) +
  scale_y_continuous(labels = comma) +
  labs(title    = 'Regresion: Ventas Netas vs. Precio Brent (2016-2024)',
       subtitle = 'TotalEnergies SE — Datos anuales en M USD',
       x = 'Brent (USD/barril)', y = 'Ventas Netas (M USD)',
       caption  = 'Fuente: Excel financiero + EIA Brent anual. Banda: IC 95%.') +
  tema_tte

print(g1)
ggsave('/tmp/Grafico_Regresion_Ventas_Brent.png', g1,
       width = 12, height = 6, dpi = 200)
file.copy('/tmp/Grafico_Regresion_Ventas_Brent.png',
          file.path(dir_origen, 'Grafico_Regresion_Ventas_Brent.png'),
          overwrite = TRUE)
cat('Guardado: Grafico_Regresion_Ventas_Brent.png\n')

###############################################################################
# PASO 4 — GRAFICO 2: PROYECCION 2025-2034
###############################################################################
cat('\n###############################################################\n')
cat('  PASO 4 — GRAFICO PROYECCION 2025-2034\n')
cat('###############################################################\n\n')

anios_proy <- 2025:2034

# Escenarios IEA WEO 2024
brent_steps <- c(79, 79, 79, 79, 79, 79, 78, 77, 77, 76)
brent_aps   <- c(74, 72, 70, 68, 66, 63, 62, 61, 60, 58)
eia_real_25 <- 69.14

# Proyectar ventas con coeficientes del Modelo A
ventas_steps <- coef_a[1] + coef_a[2] * brent_steps
ventas_aps   <- coef_a[1] + coef_a[2] * brent_aps
ventas_eia25 <- coef_a[1] + coef_a[2] * eia_real_25

# Dataframes para el grafico
df_hist  <- data.frame(Year = anios,      Ventas = df$Ventas_M,   Tipo = 'Historico')
df_steps <- data.frame(Year = anios_proy, Ventas = ventas_steps,  Tipo = 'STEPS (base)')
df_aps   <- data.frame(Year = anios_proy, Ventas = ventas_aps,    Tipo = 'APS (adverso)')
df_eia25 <- data.frame(Year = 2025,       Ventas = ventas_eia25,  Tipo = 'EIA real 2025')
df_banda <- data.frame(Year = anios_proy, ymin = ventas_aps, ymax = ventas_steps)

g2 <- ggplot() +
  geom_ribbon(data = df_banda, aes(x = Year, ymin = ymin, ymax = ymax),
              fill = '#D6E4F0', alpha = 0.5) +
  geom_vline(xintercept = 2024.5, linetype = 'dashed',
             color = '#555555', linewidth = 0.8) +
  annotate('text', x = 2024.3, y = max(df$Ventas_M) * 1.05,
           label = 'Historico | Proyectado', size = 3, color = '#555555',
           hjust = 0.5, fontface = 'italic') +
  geom_line(data = df_hist,  aes(x = Year, y = Ventas, color = Tipo), linewidth = 1.5) +
  geom_point(data = df_hist, aes(x = Year, y = Ventas, color = Tipo), size = 2.5) +
  geom_line(data = df_steps, aes(x = Year, y = Ventas, color = Tipo),
            linewidth = 1.2, linetype = 'dashed') +
  geom_point(data = df_steps, aes(x = Year, y = Ventas, color = Tipo), size = 2) +
  geom_line(data = df_aps,   aes(x = Year, y = Ventas, color = Tipo),
            linewidth = 1.2, linetype = 'dashed') +
  geom_point(data = df_aps,  aes(x = Year, y = Ventas, color = Tipo), size = 2) +
  geom_point(data = df_eia25, aes(x = Year, y = Ventas, color = Tipo),
             size = 5, shape = 18) +
  geom_text_repel(data = df_eia25,
                  aes(x = Year, y = Ventas,
                      label = sprintf('EIA 2025\n$%.1f/bbl\n%sM',
                                      eia_real_25,
                                      format(round(ventas_eia25), big.mark = ','))),
                  size = 3, color = '#C0392B', fontface = 'bold',
                  nudge_x = 0.8, nudge_y = 5000) +
  scale_color_manual(values = c(
    'Historico'      = '#1F3864',
    'STEPS (base)'   = '#2980B9',
    'APS (adverso)'  = '#E74C3C',
    'EIA real 2025'  = '#C0392B'
  )) +
  scale_x_continuous(breaks = seq(2016, 2034, 2)) +
  scale_y_continuous(labels = comma) +
  labs(title    = 'Proyeccion de Ventas TotalEnergies 2025-2034',
       subtitle = 'OLS: Ventas ~ Brent | Escenarios IEA WEO 2024 (STEPS vs. APS)',
       x = NULL, y = 'Ventas Netas (M USD)',
       caption  = paste0('Modelo: Ventas = ', format(round(coef_a[1]), big.mark = ','),
                          ' + ', format(round(coef_a[2]), big.mark = ','),
                          ' x Brent  |  R2 = ', round(r2_a, 3),
                          '  |  Fuente: IEA WEO 2024, EIA spot 2025')) +
  tema_tte

print(g2)
ggsave('/tmp/Grafico_Proyeccion_Ventas_10yr.png', g2,
       width = 12, height = 6, dpi = 200)
file.copy('/tmp/Grafico_Proyeccion_Ventas_10yr.png',
          file.path(dir_origen, 'Grafico_Proyeccion_Ventas_10yr.png'),
          overwrite = TRUE)
cat('Guardado: Grafico_Proyeccion_Ventas_10yr.png\n')

# Imprimir tabla de proyeccion
cat('\nTabla de proyeccion 2025-2034:\n')
df_proy <- data.frame(
  Year         = anios_proy,
  Brent_STEPS  = brent_steps,
  Ventas_STEPS = round(ventas_steps, 0),
  Brent_APS    = brent_aps,
  Ventas_APS   = round(ventas_aps, 0)
)
print(df_proy, row.names = FALSE)

cat('\n###############################################################\n')
cat('  SCRIPT COMPLETADO\n')
cat('###############################################################\n')
