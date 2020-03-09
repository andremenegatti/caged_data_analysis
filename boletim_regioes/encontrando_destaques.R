library(cagedExplorer)

# IMPORTANDO DADOS ------------------------------------------------------------
df_caged <- read_caged_rds(data_folder = 'data',
                           start = '2019-01',
                           end = '2019-12',
                           nested = FALSE,
                           stringAsFactors = TRUE,
                           periodos_12meses = FALSE)

# Destaques municipios --------------------------------------------------------
df_destaques_mun <- df_caged %>%
  compute_job_creation(municipio) %>%
  arrange(desc(saldo)) %>%
  slice(1:5, 641:645)

df_destaques_setor_mun <- df_caged %>%
  compute_job_creation(municipio, setor) %>%
  group_by(municipio) %>%
  mutate(saldo_total = sum(saldo)) %>%
  ungroup() %>%
  arrange(desc(saldo_total)) %>%
  filter(municipio %in% df_destaques_mun$municipio)

# Salvando
write.csv(df_destaques_setor_mun,
          'boletim-mar2020/data/destaques_municipios.csv', row.names = FALSE)

# Destaques regiões -----------------------------------------------------------
df_destaques_regioes <- df_caged %>%
  compute_job_creation(regiao_governo) %>%
  arrange(desc(saldo)) %>%
  slice(1:5, 39:43)

df_destaques_regioes_setor <- df_caged %>%
  compute_job_creation(regiao_governo, setor) %>%
  group_by(regiao_governo) %>%
  mutate(saldo_total = sum(saldo)) %>%
  ungroup() %>%
  arrange(desc(saldo_total)) %>%
  semi_join(df_destaques_regioes, by = 'regiao_governo')

# Sedes -----------------------------------------------------------------------
df_saldo_sede_setor <- df_caged %>%
  compute_job_creation(municipio, regiao_governo, setor) %>%
  filter(as.character(municipio) == as.character(regiao_governo)) %>%
  group_by(municipio) %>%
  mutate(saldo_total_sede = sum(saldo)) %>%
  ungroup() %>%
  rename(vagas_criadas_setor_sede = vagas_criadas,
         vagas_destruidas_setor_sede = vagas_destruidas,
         saldo_setor_sede = saldo) %>%
  select(-municipio)

# Juntando dados de sedes aos dados de regiões
df_destaques_regioes_setor <- df_destaques_regioes_setor %>%
  rename(vagas_criadas_setor_regiao = vagas_criadas,
         vagas_destruidas_setor_regiao = vagas_destruidas,
         saldo_setor_regiao = saldo,
         saldo_total_regiao = saldo_total) %>%
  left_join(df_saldo_sede_setor, by = c('regiao_governo', 'setor'))

# Salvando
# write.csv(df_destaques_regioes_setor,
#           'boletim-mar2020/data/destaques_regioes.csv', row.names = FALSE)
