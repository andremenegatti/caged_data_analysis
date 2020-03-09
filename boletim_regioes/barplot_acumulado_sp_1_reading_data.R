library(cagedExplorer)

# READING DATA ----------------------------------------------------------------
df_2019 <- read_caged_rds(data_folder = 'data',
                          start = '2019-01',
                          end = '2019-12',
                          nested = FALSE,
                          stringAsFactors = TRUE,
                          periodos_12meses = FALSE) %>% 
  mutate(periodo = '2019')

df_2018 <- read_caged_rds(data_folder = 'data',
                          start = '2018-01',
                          end = '2018-12',
                          nested = FALSE,
                          stringAsFactors = TRUE,
                          periodos_12meses = FALSE) %>% 
  mutate(periodo = '2018')

df_2017 <- read_caged_rds(data_folder = 'data',
                          start = '2017-01',
                          end = '2017-12',
                          nested = FALSE,
                          stringAsFactors = TRUE,
                          periodos_12meses = FALSE) %>% 
  mutate(periodo = '2017')

df_2016 <-  read_caged_rds(data_folder = 'data',
                           start = '2016-01',
                           end = '2016-12',
                           nested = FALSE,
                           stringAsFactors = TRUE,
                           periodos_12meses = FALSE) %>% 
  mutate(periodo = '2016')

# Joining
df_full <- bind_rows(df_2016, df_2017, df_2018, df_2019)

# Saving - rds format
saveRDS(df_full, 'boletim_regioes/boletim-mar2020/data/df_full.rds')