library(cagedExplorer)

barplot_mes <- barplot_monthly(df_caged, inicio = '2019-01',
                               fim = '2019-12', incluir_ano_x = FALSE) +
  theme(panel.grid.major.x = element_blank())

saveRDS(barplot_mes, 'boletim-mar2020/data/barplot_setor_sp_mensal.rds')

ggsave(barplot_mes,
       filename = 'boletim-mar2020/plots/barplot_setor_sp_mensal.png',
       width = 8, height = 7)