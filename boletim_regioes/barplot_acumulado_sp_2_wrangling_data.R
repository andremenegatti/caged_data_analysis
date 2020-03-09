library(tidyverse)

df_full <- readRDS('boletim_regiones/boletim-mar2020/data/acumulado_2016_a_2019.rds')

acumulado_12meses_estado <- df_full %>%
  filter(!is.na(periodo)) %>%
  compute_job_creation(periodo)

saveRDS(acumulado_12meses_estado,
        'boletim_regioes/boletim-mar2020/data/acumulado_12meses_estado.rds')

acumulado_12meses_estado_setor <- df_full %>%
  filter(!is.na(periodo)) %>%
  compute_job_creation(periodo, setor) %>%
  drop_na()

saveRDS(acumulado_12meses_estado_setor,
        'boletim_regioes/boletim-mar2020/data/acumulado_12_meses_estado_setor.rds')