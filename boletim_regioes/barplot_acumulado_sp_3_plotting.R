library(tidyverse)

# Importando dados ------------------------------------------------------------
acumulado_12meses_estado <-
  readRDS('boletim-mar2020/data/acumulado_12meses_estado.rds')

acumulado_12meses_estado_setor <-
  readRDS('boletim-mar2020/data/acumulado_12_meses_estado_setor.rds')

# Gerando gráfico -------------------------------------------------------------
plot_acumulado <-
  acumulado_12meses_estado_setor %>%
  ggplot() +
  geom_col(aes(x = periodo, y = saldo/1000, fill = setor),
           position = 'stack', alpha = 0.7) +
  custom_theme() +
  geom_hline(yintercept = 0, col = 'darkred') +
  labs(
    x = 'Período',
    y =  'Postos de trabalho (milhares)',
    subtitle = 'Contribuição de cada setor',
    caption = 'Fonte: Elaboração própria a partir de dados do CAGED.',
    title = 
      'Empregos no Estado de SP - Evolução do Saldo Acumulado - Jan/2019 a Dez/2019'
    ) +
  scale_fill_brewer(palette = "Accent", name = "Setor") +
  geom_line(data = acumulado_12meses_estado,
            aes(x = periodo, y = saldo/1000, group = 1),
            col = 'black', linetype = 'dotted') +
  geom_label(data = acumulado_12meses_estado,
             aes(x = periodo, y = saldo/1000,
                 label = str_replace(saldo/1000, '\\.', ',')),
             family = 'serif', size = 3) +
  theme(panel.grid = element_blank()) ; plot_acumulado

# Salvando --------------------------------------------------------------------
ggsave(plot_acumulado, width = 8, height = 7,
       filename = 'boletim-mar2020/plots/barplot_setor_sp_12meses.png')