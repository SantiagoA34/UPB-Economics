###############################################################################
#  graficos_totalenergies_v2.R
#  TotalEnergies SE 2016-2024 — 4 graficos seleccionados
#  G0: Brent | G1: Margenes | G3: CAPEX | G5: CCC
#  Ejecutar en RStudio con: source('graficos_totalenergies_v2.R')
###############################################################################

# ===========================================================================
# PARTE 1 — PAQUETES Y EXTRACCION DE DATOS
# ===========================================================================

options(repos = c(CRAN = "https://cloud.r-project.org"))
pkgs <- c("readxl", "dplyr", "tidyr", "ggplot2", "scales",
           "ggrepel", "stringr")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# Rutas — copia a /tmp/ para evitar problemas de encoding en macOS
dir_origen <- getwd()
file.copy(file.path(dir_origen, 'Excel_Total_Energies2.xlsx'),
          '/tmp/Excel_Total_Energies2.xlsx', overwrite = TRUE)
file.copy(file.path(dir_origen, 'EIA_BrentCrudo_USD_Barril_1987-2024.xls'),
          '/tmp/EIA_Brent.xls', overwrite = TRUE)
ruta_excel <- '/tmp/Excel_Total_Energies2.xlsx'
ruta_brent <- '/tmp/EIA_Brent.xls'

anios <- 2016:2024

# ---------------------------------------------------------------------------
# Funciones auxiliares
# ---------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------
# Ratios de Rentabilidad
# ---------------------------------------------------------------------------
rent <- leer_hoja('Ratios de Rentabilidad')
margen_ebitda <- extraer_fila(rent, 'Margen EBITDA')
margen_oper   <- extraer_fila(rent, 'Margen operativo')
margen_neto   <- extraer_fila(rent, 'Margen neto')

# ---------------------------------------------------------------------------
# Ratios de Eficiencia
# ---------------------------------------------------------------------------
efi <- leer_hoja('Ratios de Eficiencia')
dso <- extraer_fila(efi, 'cobro.*DSO')
dpo <- extraer_fila(efi, 'pago.*DPO')
dio <- extraer_fila(efi, 'inventario.*DIO')
ccc <- extraer_fila(efi, 'Ciclo neto')

# ---------------------------------------------------------------------------
# CAPEX por Segmento (URD) — Tabla III
# ---------------------------------------------------------------------------
capex_raw <- leer_hoja('CAPEX por Segmento (URD)')
fossil_pct <- as.numeric(c(capex_raw[30, 3], capex_raw[30, 5], capex_raw[30, 7]))
lng_pct    <- as.numeric(c(capex_raw[31, 3], capex_raw[31, 5], capex_raw[31, 7]))
power_pct  <- as.numeric(c(capex_raw[32, 3], capex_raw[32, 5], capex_raw[32, 7]))
corp_pct   <- as.numeric(c(capex_raw[34, 3], capex_raw[34, 5], capex_raw[34, 7]))

df_capex <- data.frame(
  year      = rep(c(2022, 2023, 2024), each = 4),
  categoria = rep(c('Fossil (E&P+R&C+M&S)', 'Transicion (LNG)',
                     'Renovable (Power)', 'Corporativo'), 3),
  pct       = c(fossil_pct[1], lng_pct[1], power_pct[1], corp_pct[1],
                fossil_pct[2], lng_pct[2], power_pct[2], corp_pct[2],
                fossil_pct[3], lng_pct[3], power_pct[3], corp_pct[3])
)
df_capex$categoria <- factor(df_capex$categoria,
  levels = c('Corporativo', 'Renovable (Power)',
             'Transicion (LNG)', 'Fossil (E&P+R&C+M&S)'))

# ---------------------------------------------------------------------------
# Brent crudo — EIA (datos anuales)
# ---------------------------------------------------------------------------
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
  select(year, brent = precio)

# ===========================================================================
# PARTE 2 — FASES, HITOS Y TEMA
# ===========================================================================

fases <- data.frame(
  xmin  = c(2015.5, 2016.5, 2019.5, 2021.5, 2023.5),
  xmax  = c(2016.5, 2019.5, 2021.5, 2023.5, 2024.5),
  fill  = c('#E74C3C','#2ECC71','#F39C12','#8E44AD','#2980B9'),
  label = c('F1 Colapso','F2 Adquisiciones','F3 COVID+Rebrand',
            'F4 Ucrania','F5 Transicion')
)

hitos <- data.frame(
  year  = c(2016, 2018, 2020, 2021, 2022),
  label = c('Brent min $27',
            'Adq. Maersk $7.45B',
            'COVID: -$7.2B',
            'Rebrand TotalEnergies',
            'Ucrania: record+writeoff')
)

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

add_fases_hitos <- function(p, ymin_val = -Inf, ymax_val = Inf) {
  p +
    geom_rect(data = fases,
              aes(xmin = xmin, xmax = xmax, ymin = ymin_val, ymax = ymax_val,
                  fill = label), alpha = 0.10, inherit.aes = FALSE) +
    scale_fill_manual(values = setNames(fases$fill, fases$label)) +
    geom_vline(data = hitos, aes(xintercept = year),
               linetype = 'dashed', color = '#555555', alpha = 0.6) +
    geom_text(data = hitos,
              aes(x = year, y = ymax_val * 0.95, label = label),
              angle = 90, vjust = -0.3, hjust = 1, size = 2.8,
              color = '#333333', inherit.aes = FALSE)
}

# ===========================================================================
# PARTE 3 — GRAFICOS
# ===========================================================================

# -------------------------------------------------------------------
# GRAFICO 0 — Precio Brent (USD/bbl) 2016-2024
# -------------------------------------------------------------------
g0 <- ggplot(brent_anual, aes(x = year, y = brent)) +
  geom_area(fill = '#E67E22', alpha = 0.15) +
  geom_line(color = '#E67E22', linewidth = 2.5) +
  geom_point(color = '#E67E22', size = 3) +
  geom_hline(yintercept = 40, linetype = 'dotted',
             color = 'red', linewidth = 0.9) +
  annotate('text', x = 2016.2, y = 42, label = 'Umbral crisis',
           color = 'red', size = 3.2, hjust = 0) +
  geom_text(aes(label = paste0('$', round(brent, 1))),
            vjust = -1.2, size = 3, color = '#333333') +
  labs(title    = 'Precio Brent 2016-2024',
       subtitle = 'Contexto macroeconomico que mueve todos los ratios',
       x = NULL, y = 'USD por barril') +
  scale_x_continuous(breaks = 2016:2024) +
  scale_y_continuous(limits = c(0, 120)) +
  tema_tte
g0 <- add_fases_hitos(g0, ymin_val = 0, ymax_val = 120)

print(g0)  # Mostrar en RStudio
ggsave('grafico_0_brent.png', g0, width = 12, height = 6, dpi = 200)

# -------------------------------------------------------------------
# GRAFICO 1 — Margenes de Rentabilidad 2016-2024
# -------------------------------------------------------------------
df_margenes <- data.frame(
  year = rep(anios, 3),
  indicador = rep(c('Margen EBITDA', 'Margen Operativo', 'Margen Neto'),
                  each = 9),
  valor = c(margen_ebitda, margen_oper, margen_neto)
)

colores_margenes <- c(
  'Margen EBITDA'    = '#2980B9',
  'Margen Operativo' = '#27AE60',
  'Margen Neto'      = '#8E44AD'
)
formas_margenes <- c(
  'Margen EBITDA' = 21, 'Margen Operativo' = 22, 'Margen Neto' = 24
)

g1 <- ggplot(df_margenes, aes(x = year, y = valor * 100,
                              color = indicador, shape = indicador)) +
  geom_hline(yintercept = 0, color = 'red', alpha = 0.6) +
  geom_line(linewidth = 2) +
  geom_point(size = 3.5, fill = 'white', stroke = 1.5) +
  annotate('label', x = 2016, y = margen_ebitda[1] * 100 + 3,
           label = paste0('F1: EBITDA ', round(margen_ebitda[1]*100, 1),
                          '%\nBrent $', round(brent_anual$brent[1], 0), '/bbl'),
           size = 2.8, fill = '#FDECEA', color = '#C0392B') +
  annotate('label', x = 2020, y = margen_neto[5] * 100 - 3,
           label = paste0('COVID: Margen neto\n',
                          round(margen_neto[5]*100, 1), '% (-$7.2B)'),
           size = 2.8, fill = '#FFF3E0', color = '#E67E22') +
  annotate('label', x = 2022, y = margen_ebitda[7] * 100 + 3,
           label = paste0('Record: EBITDA ',
                          round(margen_ebitda[7]*100, 1),
                          '%\nUcrania + precios max'),
           size = 2.8, fill = '#EAF4E8', color = '#27AE60') +
  scale_color_manual(values = colores_margenes) +
  scale_shape_manual(values = formas_margenes) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  scale_x_continuous(breaks = 2016:2024) +
  labs(title    = 'Margenes de Rentabilidad TotalEnergies 2016-2024',
       subtitle = 'Efecto del colapso del petroleo, COVID y guerra Ucrania en el margen EBITDA',
       x = NULL, y = 'Margen (%)') +
  tema_tte
g1 <- add_fases_hitos(g1, ymin_val = -10, ymax_val = 35)

print(g1)  # Mostrar en RStudio
ggsave('grafico_1_margenes.png', g1, width = 12, height = 6, dpi = 200)

# -------------------------------------------------------------------
# GRAFICO 3 — CAPEX Fossil vs. Renovable 2022-2024 (barras 100%)
# -------------------------------------------------------------------
colores_capex <- c(
  'Fossil (E&P+R&C+M&S)' = '#E74C3C',
  'Transicion (LNG)'      = '#F39C12',
  'Renovable (Power)'     = '#2ECC71',
  'Corporativo'           = '#95A5A6'
)

g3 <- ggplot(df_capex, aes(x = factor(year), y = pct * 100,
                            fill = categoria)) +
  geom_col(position = 'stack', width = 0.55) +
  geom_text(aes(label = paste0(round(pct * 100, 1), '%')),
            position = position_stack(vjust = 0.5),
            color = 'white', size = 3.5, fontface = 'bold') +
  scale_fill_manual(values = colores_capex) +
  scale_y_continuous(labels = function(x) paste0(x, '%'),
                     limits = c(0, 105)) +
  labs(title    = 'Composicion del CAPEX por Tipo de Energia 2022-2024',
       subtitle = 'Esta TotalEnergies cambiando realmente su modelo de negocio?',
       x = NULL, y = '% del CAPEX total (Gross Investments)',
       caption  = 'Fuente: URD 2024, inversiones brutas (Gross = Organico + Adquisiciones + Equity)') +
  tema_tte +
  theme(panel.grid.major.x = element_blank())

print(g3)  # Mostrar en RStudio
ggsave('grafico_3_capex_dual.png', g3, width = 10, height = 6, dpi = 200)

# -------------------------------------------------------------------
# GRAFICO 5 — Ciclo de Conversion de Efectivo 2016-2024
# -------------------------------------------------------------------
df_ccc <- data.frame(
  year = anios, DSO = dso, DIO = dio, DPO = dpo, CCC = ccc
)

df_ccc_largo <- data.frame(
  year      = rep(anios, 3),
  categoria = rep(c('DSO (cobro)', 'DIO (inventario)', 'DPO (pago)'), each = 9),
  dias      = c(dso, dio, -dpo)
)
df_ccc_largo$categoria <- factor(df_ccc_largo$categoria,
  levels = c('DPO (pago)', 'DIO (inventario)', 'DSO (cobro)'))

g5 <- ggplot(df_ccc_largo, aes(x = year, y = dias, fill = categoria)) +
  geom_col(position = 'stack', width = 0.55, alpha = 0.85) +
  geom_line(data = df_ccc, aes(x = year, y = CCC),
            inherit.aes = FALSE, color = 'black',
            linewidth = 2, linetype = 'solid') +
  geom_point(data = df_ccc, aes(x = year, y = CCC),
             inherit.aes = FALSE, color = 'black',
             shape = 18, size = 4) +
  geom_text(data = df_ccc, aes(x = year, y = CCC,
                                label = paste0(round(CCC, 1), 'd')),
            inherit.aes = FALSE, vjust = -1, size = 2.8, color = 'black') +
  scale_fill_manual(values = c(
    'DSO (cobro)'       = '#2980B9',
    'DIO (inventario)'  = '#27AE60',
    'DPO (pago)'        = '#E74C3C'
  )) +
  scale_x_continuous(breaks = 2016:2024) +
  labs(title    = 'Ciclo de Conversion de Efectivo (dias) 2016-2024',
       subtitle = 'Cuantos dias tarda TotalEnergies en convertir inventario en caja?',
       x = NULL, y = 'Dias',
       caption  = 'Linea negra: Ciclo Neto (CCC = DSO + DIO - DPO). Barras rojas negativas = DPO.') +
  tema_tte

print(g5)  # Mostrar en RStudio
ggsave('grafico_5_ccc.png', g5, width = 12, height = 6, dpi = 200)

# ===========================================================================
# RESUMEN
# ===========================================================================
cat('\n============================================================\n')
cat('  4 GRAFICOS GENERADOS\n')
cat('============================================================\n')
cat('  grafico_0_brent.png        -- Precio Brent 2016-2024\n')
cat('  grafico_1_margenes.png     -- Margenes de Rentabilidad\n')
cat('  grafico_3_capex_dual.png   -- CAPEX Fossil vs. Renovable\n')
cat('  grafico_5_ccc.png          -- Ciclo Conversion Efectivo\n')
cat('============================================================\n')
cat('  Cada grafico se muestra con print() para vista en RStudio.\n')
cat('============================================================\n')
