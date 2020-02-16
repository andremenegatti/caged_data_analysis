library(cagedExplorer)

# Carregando dados (atencao para inicio e fim)
df_acum_ano_atual <- read_caged_rds(data_folder = 'data', start = '2018-12', end = '2019-11')
df_acum_ano_anterior <- read_caged_rds(data_folder = 'data', start = '2017-12', end = '2018-11')

# Mes de referencia
mes_atual <- 11

# Vetor com nomes das colunas das tabelas finais
colunas <- c('Setores',
             'Nov./2018',
             'Acumulado Dez./2017 a Nov./2018',
             'Nov./2019',
             'Acumulado Dez./2018 a Nov./2019')

# Pasta onde as tabelas serao salvas
output_folder <- 'dados_boletim/tabelas'

#### MUNICIPIOS ####
for (mun in c('RIBEIRAO PRETO', 'SERTAOZINHO', 'CAMPINAS', 'FRANCA', 'SAO JOSE DO RIO PRETO')) {

  criar_tabela_municipio(df_ano_atual = df_acum_ano_atual,
                         df_ano_anterior = df_acum_ano_anterior,
                         mes_atual = mes_atual,
                         municipio = mun,
                         colunas = colunas,
                         csv_output = str_c(output_folder, '/', mun, '.csv'))
}

#### REGIAO ADM RIBEIRAO ####
criar_tabela_regiao_adm(df_ano_atual = df_acum_ano_atual,
                        df_ano_anterior = df_acum_ano_anterior,
                        mes_atual = mes_atual, colunas = colunas,
                        csv_output = str_c(output_folder, '/REGIAO_ADM_RIBEIRAO.csv'))

#### ESTADO SP ####
criar_tabela_estado(df_ano_atual = df_acum_ano_atual,
                    df_ano_anterior = df_acum_ano_anterior,
                    mes_atual = mes_atual, colunas = colunas,
                    csv_output = str_c(output_folder, '/ESTADO_SP.csv'))
