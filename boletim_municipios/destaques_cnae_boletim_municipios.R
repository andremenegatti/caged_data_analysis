library(cagedExplorer)

# Carregando bases para acumulado 12 meses
df_ano_atual <- read_caged_rds(data_folder = 'data',
                               start = '2019-01', end = '2019-12',
                               periodos_12meses = FALSE)

df_ano_anterior <- read_caged_rds(data_folder = 'data',
                                  start = '2018-01', end = '2018-12',
                                  periodos_12meses = FALSE)

# Definindo variaveis
mes_atual <- 12
ano_atual <- 2019
ano_anterior <- 2018
output_folder <- 'dados_boletim'

# Separando dados do mes
df_mes_ano_atual <- filter(df_ano_atual, mes == mes_atual)
df_mes_ano_anterior <- filter(df_ano_anterior, mes == mes_atual)

# MUNICIPIOS ------------------------------------------------------------------
for (mun in c('RIBEIRAO PRETO', 'FRANCA', 'CAMPINAS',
              'SAO JOSE DO RIO PRETO', 'SERTAOZINHO')) {
  
  print(mun)
  
  # Mes, ano atual
  saldo_mes_ano_atual <- df_mes_ano_atual %>%
    filter(municipio_clean == mun) %>%
    compute_job_creation(setor, classe_cnae, .drop = FALSE) %>%
    select(setor, classe_cnae, saldo)
  
  destaques_positivos_mes_ano_atual <- 
    find_cnae_highlights(df = saldo_mes_ano_atual)
  
  destaques_negativos_mes_ano_atual <- 
    find_cnae_highlights(df = saldo_mes_ano_atual, positivos = FALSE)
  
  write.csv(destaques_positivos_mes_ano_atual,
            str_c(output_folder, '/destaques_cnae/', mun,
                  '/destaques_positivos_', mun, '_',
                  ano_atual, '_', mes_atual, '.csv'))
  
  write.csv(destaques_negativos_mes_ano_atual,
            str_c(output_folder, '/destaques_cnae/', mun,
                  '/destaques_negativos_', mun, '_',
                  ano_atual, '_', mes_atual, '.csv'))
  
  # Mes, ano anterior
  saldo_mes_ano_anterior <- df_mes_ano_anterior %>%
    filter(municipio_clean == mun) %>%
    compute_job_creation(setor, classe_cnae, .drop = FALSE) %>%
    select(setor, classe_cnae, saldo)
  
  destaques_positivos_mes_ano_anterior <- 
    find_cnae_highlights(df = saldo_mes_ano_anterior)
  
  destaques_negativos_mes_ano_anterior <- 
    find_cnae_highlights(df = saldo_mes_ano_anterior, positivos = FALSE)
  
  write.csv(destaques_positivos_mes_ano_anterior,
            str_c(output_folder, '/destaques_cnae/', mun,
                  '/destaques_positivos_', mun, '_',
                  ano_anterior, '_', mes_atual, '.csv'))
  
  write.csv(destaques_negativos_mes_ano_anterior,
            str_c(output_folder, '/destaques_cnae/', mun,
                  '/destaques_negativos_', mun, '_',
                  ano_anterior, '_', mes_atual, '.csv'))
  
  # Acumulado 12 meses, ano atual
  saldo_acum_ano_atual <- df_ano_atual %>%
    filter(municipio_clean == mun) %>%
    compute_job_creation(setor, classe_cnae, .drop = FALSE) %>%
    select(setor, classe_cnae, saldo)
  
  destaques_positivos_acum_ano_atual <- 
    find_cnae_highlights(df = saldo_acum_ano_atual)
  
  destaques_negativos_acum_ano_atual <-
    find_cnae_highlights(df = saldo_acum_ano_atual, positivos = FALSE)
  
  write.csv(destaques_positivos_acum_ano_atual,
            str_c(output_folder, '/destaques_cnae/', mun,
                  '/destaques_positivos_acumulado_', mun,
                  '_', ano_atual, '.csv'))
  
  write.csv(destaques_negativos_acum_ano_atual,
            str_c(output_folder, '/destaques_cnae/', mun,
                  '/destaques_negativos_acumulado_', mun,
                  '_', ano_atual, '.csv'))
  
  # Acumulado 12 meses, ano anterior
  saldo_acum_ano_anterior <- df_ano_anterior %>%
    filter(municipio_clean == mun) %>%
    compute_job_creation(setor, classe_cnae, .drop = FALSE) %>%
    select(setor, classe_cnae, saldo)
  
  destaques_positivos_acum_ano_anterior <- 
    find_cnae_highlights(df = saldo_acum_ano_anterior)
  
  destaques_negativos_acum_ano_anterior <- 
    find_cnae_highlights(df = saldo_acum_ano_anterior, positivos = FALSE)
  
  write.csv(destaques_positivos_acum_ano_atual,
            str_c(output_folder, '/destaques_cnae/', mun,
                  '/destaques_positivos_acumulado_', mun,
                  '_', ano_anterior, '.csv'))
  
  write.csv(destaques_negativos_acum_ano_atual,
            str_c(output_folder, '/destaques_cnae/', mun,
                  '/destaques_negativos_acumulado_', mun,
                  '_', ano_anterior, '.csv'))
  
}

# REGIAO ADM RIBEIRAO ---------------------------------------------------------
# Mes, ano atual
saldo_mes_ano_atual <- df_mes_ano_atual %>%
  filter(regiao_administrativa == 'Ribeirão Preto') %>%
  compute_job_creation(setor, classe_cnae, .drop = FALSE) %>%
  select(setor, classe_cnae, saldo)

destaques_positivos_mes_ano_atual <-
  find_cnae_highlights(df = saldo_mes_ano_atual)

destaques_negativos_mes_ano_atual <-
  find_cnae_highlights(df = saldo_mes_ano_atual, positivos = FALSE)

write.csv(destaques_positivos_mes_ano_atual,
          str_c(output_folder, '/destaques_cnae/REGIAO ADM RIBEIRAO',
                '/destaques_positivos_REGIAO ADM RIBEIRAO_', ano_atual,
                '_', mes_atual, '.csv'))

write.csv(destaques_negativos_mes_ano_atual,
          str_c(output_folder, '/destaques_cnae/REGIAO ADM RIBEIRAO',
                '/destaques_negativos_REGIAO ADM RIBEIRAO_', ano_atual,
                '_', mes_atual, '.csv'))

# Mes, ano anterior
saldo_mes_ano_anterior <- df_mes_ano_anterior %>%
  filter(regiao_administrativa == 'Ribeirão Preto') %>%
  compute_job_creation(setor, classe_cnae, .drop = FALSE) %>%
  select(setor, classe_cnae, saldo)

destaques_positivos_mes_ano_anterior <-
  find_cnae_highlights(df = saldo_mes_ano_anterior)

destaques_negativos_mes_ano_anterior <-
  find_cnae_highlights(df = saldo_mes_ano_anterior, positivos = FALSE)

write.csv(destaques_positivos_mes_ano_anterior,
          str_c(output_folder, '/destaques_cnae/REGIAO ADM RIBEIRAO',
                '/destaques_positivos_REGIAO ADM RIBEIRAO_', ano_anterior,
                '_', mes_atual, '.csv'))

write.csv(destaques_negativos_mes_ano_anterior,
          str_c(output_folder, '/destaques_cnae/REGIAO ADM RIBEIRAO',
                '/destaques_negativos_REGIAO ADM RIBEIRAO_', ano_anterior,
                '_', mes_atual, '.csv'))

# Acumulado 12 meses, ano atual
saldo_acum_ano_atual <- df_ano_atual %>%
  filter(regiao_administrativa == 'Ribeirão Preto') %>%
  compute_job_creation(setor, classe_cnae, .drop = FALSE) %>%
  select(setor, classe_cnae, saldo)

destaques_positivos_acum_ano_atual <-
  find_cnae_highlights(df = saldo_acum_ano_atual)

destaques_negativos_acum_ano_atual <-
  find_cnae_highlights(df = saldo_acum_ano_atual, positivos = FALSE)

write.csv(destaques_positivos_acum_ano_atual,
          str_c(output_folder, '/destaques_cnae/REGIAO ADM RIBEIRAO',
                '/destaques_positivos_acumulado_REGIAO ADM RIBEIRAO_',
                ano_atual, '.csv'))

write.csv(destaques_negativos_acum_ano_atual,
          str_c(output_folder, '/destaques_cnae/REGIAO ADM RIBEIRAO',
                '/destaques_negativos_acumulado_REGIAO ADM RIBEIRAO_',
                ano_atual, '.csv'))

# Acumulado 12 meses, ano anterior
saldo_acum_ano_anterior <- df_ano_anterior %>%
  filter(regiao_administrativa == 'Ribeirão Preto') %>%
  compute_job_creation(setor, classe_cnae, .drop = FALSE) %>%
  select(setor, classe_cnae, saldo)

destaques_positivos_acum_ano_anterior <-
  find_cnae_highlights(df = saldo_acum_ano_anterior)

destaques_negativos_acum_ano_anterior <-
  find_cnae_highlights(df = saldo_acum_ano_anterior, positivos = FALSE)

write.csv(destaques_positivos_acum_ano_atual,
          str_c(output_folder, '/destaques_cnae/REGIAO ADM RIBEIRAO',
                '/destaques_positivos_acumulado_REGIAO ADM RIBEIRAO_',
                ano_anterior, '.csv'))

write.csv(destaques_negativos_acum_ano_atual,
          str_c(output_folder, '/destaques_cnae/REGIAO ADM RIBEIRAO',
                '/destaques_negativos_acumulado_REGIAO ADM RIBEIRAO_',
                ano_anterior, '.csv'))

# ESTADO SP -------------------------------------------------------------------
# Mes, ano atual
saldo_mes_ano_atual <- df_mes_ano_atual %>%
  compute_job_creation(setor, classe_cnae, .drop = FALSE) %>%
  select(setor, classe_cnae, saldo)

destaques_positivos_mes_ano_atual <-
  find_cnae_highlights(df = saldo_mes_ano_atual)

destaques_negativos_mes_ano_atual <-
  find_cnae_highlights(df = saldo_mes_ano_atual, positivos = FALSE)

write.csv(destaques_positivos_mes_ano_atual,
          str_c(output_folder, '/destaques_cnae/ESTADO SP',
                '/destaques_positivos_ESTADO SP_', ano_atual, '_',
                mes_atual, '.csv'))

write.csv(destaques_negativos_mes_ano_atual,
          str_c(output_folder, '/destaques_cnae/ESTADO SP',
                '/destaques_negativos_ESTADO SP_', ano_atual, '_',
                mes_atual, '.csv'))

# Mes, ano anterior
saldo_mes_ano_anterior <- df_mes_ano_anterior %>%
  compute_job_creation(setor, classe_cnae, .drop = FALSE) %>%
  select(setor, classe_cnae, saldo)

destaques_positivos_mes_ano_anterior <-
  find_cnae_highlights(df = saldo_mes_ano_anterior)

destaques_negativos_mes_ano_anterior <-
  find_cnae_highlights(df = saldo_mes_ano_anterior, positivos = FALSE)

write.csv(destaques_positivos_mes_ano_anterior,
          str_c(output_folder, '/destaques_cnae/ESTADO SP',
                '/destaques_positivos_ESTADO SP_', ano_anterior, '_',
                mes_atual, '.csv'))

write.csv(destaques_negativos_mes_ano_anterior,
          str_c(output_folder, '/destaques_cnae/ESTADO SP',
                '/destaques_negativos_ESTADO SP_', ano_anterior, '_',
                mes_atual, '.csv'))

# Acumulado 12 meses, ano atual
saldo_acum_ano_atual <- df_ano_atual %>%
  compute_job_creation(setor, classe_cnae, .drop = FALSE) %>%
  select(setor, classe_cnae, saldo)

destaques_positivos_acum_ano_atual <- 
  find_cnae_highlights(df = saldo_acum_ano_atual)

destaques_negativos_acum_ano_atual <- 
  find_cnae_highlights(df = saldo_acum_ano_atual, positivos = FALSE)

write.csv(destaques_positivos_acum_ano_atual,
          str_c(output_folder, '/destaques_cnae/ESTADO SP',
                '/destaques_positivos_acumulado_ESTADO SP_', ano_atual, '.csv'))

write.csv(destaques_negativos_acum_ano_atual,
          str_c(output_folder, '/destaques_cnae/ESTADO SP',
                '/destaques_negativos_acumulado_ESTADO SP_', ano_atual, '.csv'))

# Acumulado 12 meses, ano anterior
saldo_acum_ano_anterior <- df_ano_anterior %>%
  compute_job_creation(setor, classe_cnae, .drop = FALSE) %>%
  select(setor, classe_cnae, saldo)

destaques_positivos_acum_ano_anterior <- 
  find_cnae_highlights(df = saldo_acum_ano_anterior)

destaques_negativos_acum_ano_anterior <- 
  find_cnae_highlights(df = saldo_acum_ano_anterior, positivos = FALSE)

write.csv(destaques_positivos_acum_ano_atual,
          str_c(output_folder, '/destaques_cnae/ESTADO SP',
                '/destaques_positivos_acumulado_ESTADO SP_',
                ano_anterior, '.csv'))

write.csv(destaques_negativos_acum_ano_atual,
          str_c(output_folder, '/destaques_cnae/ESTADO SP',
                '/destaques_negativos_acumulado_ESTADO SP_',
                ano_anterior, '.csv'))