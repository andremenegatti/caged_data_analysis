library(cagedExplorer)

# READING DATA ----------------------------------------------------------------
df_caged <- read_caged_rds(data_folder = 'data',
                           start = '2015-11',
                           end = '2019-10',
                           nested = FALSE,
                           stringAsFactors = TRUE,
                           periodos_12meses = TRUE)


# EVOLUCAO MENSAL -------------------------------------------------------------
barplot_mes <- barplot_monthly(df_caged, inicio = '2019-01',
                               fim = '2019-10', incluir_ano_x = FALSE) +
  theme(panel.grid.major.x = element_blank())

# saveRDS(barplot_mes, 'Desktop/barplot_setor_sp_mensal.rds')
# ggsave(barplot_mes, filename = 'barplot_setor_sp_mensal.png', width = 8, height = 7)

# EVOLUCAO SALDO ACUMULADO 12 MESES -------------------------------------------
barplot_sp <- barplot_12months(df = df_caged) +
  theme(panel.grid.major.x = element_blank())

# saveRDS(barplot_sp, 'barplot_setor_setor_sp_12meses.rds')
# ggsave(barplot_sp, filename = 'barplot_setor_sp_12meses.png', width = 8, height = 7)