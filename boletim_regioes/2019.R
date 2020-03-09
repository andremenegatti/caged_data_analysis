library(cagedExplorer)

# READING DATA ----------------------------------------------------------------
df_caged <- read_caged_rds(data_folder = 'data',
                           start = '2019-01',
                           end = '2019-10',
                           nested = FALSE,
                           stringAsFactors = TRUE,
                           periodos_12meses = FALSE)

# AGREGANDO E CALCULANDO VARIAVEIS DE INTERESSE -------------------------------
# Saldo por municipio
municipios_acumulado_12_meses <- df_caged %>%
  compute_job_creation(municipio, Codmun7, regiao_governo, pop) %>%
  arrange(desc(saldo))

# Agregando por regiao de governo
sp_sf_gov <- municipios_acumulado_12_meses %>%
  group_by(regiao_governo) %>%
  summarise(`Saldo de empregos` = sum(saldo),
            `Empregos gerados/100mil hab.` = sum(saldo) / sum(pop) * 100e+3,
            pop = sum(pop)) %>%
  add_geometry_regioes_gov()

# Dados de salario, por regiao
reg_gov_salario <- df_caged %>% 
  group_by(regiao_governo, movimento) %>% 
  summarise(vagas = sum(abs(movimento)),
            massa_salarial = sum(salario),
            salario_medio = massa_salarial / vagas) %>% 
  ungroup() %>% 
  mutate(movimento = ifelse(movimento == 1, 'criadas', 'destruidas')) %>% 
  pivot_wider(names_from = movimento,
              values_from = c(vagas, massa_salarial, salario_medio)) %>%
  mutate(saldo_vagas = vagas_criadas - vagas_destruidas,
         saldo_massa_salarial = massa_salarial_criadas - massa_salarial_destruidas,
         diferenca_salario_medio = salario_medio_criadas - salario_medio_destruidas) %>%
  inner_join(sp_sf_gov %>% as.data.frame() %>%  select(regiao_governo, pop),
             by = 'regiao_governo') %>%
  mutate(var_perc_massa_salarial = saldo_massa_salarial / massa_salarial_destruidas * 100) %>%
  mutate(sentido_saldo = ifelse(saldo_vagas > 0, 'Positivo', 'Negativo') %>%
           as.factor())

glimpse(reg_gov_salario)

# DOTPLOTS --------------------------------------------------------------------
# Variacao massa salarial
reg_gov_salario %>%
  mutate(regiao_governo = fct_reorder(regiao_governo, var_perc_massa_salarial)) %>%
  ggplot() +
  geom_point(aes(x = var_perc_massa_salarial,
                 y = regiao_governo,
                 size = massa_salarial_criadas/1e+6,
                 col = sentido_saldo),
             alpha = 0.8) +
  custom_theme() +
  geom_vline(xintercept = 0, color = 'darkred', alpha = 0.5) +
  labs(x = '',
       y = 'Região de governo',
       title = 'Soma dos salários das vagas criadas como \npercentual da soma dos salários das vagas destruídas',
       subtitle = 'Comparação entre Regiões de Governo de SP - Jan/2019 a Out/2019') +
  scale_size_continuous(name = 'Total de salários criados',
                        breaks = c(500, 1000, 3000, 5000),
                        labels = c('R$ 500 milhões', 'R$ 1 bilhão', 'R$ 3 bilhões', 'R$ 5 bilhões')) +
  scale_color_manual(name = 'Saldo de vagas',
                     # values = c("#D6604D", "#2171B5"),
                     values = c('red', 'steelblue')) +
  scale_x_continuous(labels = function(x) str_c(x, '%'))
# ggsave('variacao_massa_salarial.png', height = 8, width = 6)

# Empregos gerados 100 mil hab
saldo_100mil_breaks = c(-540, -168, -45, 0, 401.42545, 816.63611, 1650, 2201.27665)
saldo_100mil_palette = c("#B2182B", "#F4A582", "#FDDBC7",  "#C6DBEF", "#6BAED6", "#2171B5", "#08306B")

dotplot_regions(sp_sf_gov, var_plot = `Empregos gerados/100mil hab.`,
                breaks = saldo_100mil_breaks, palette = saldo_100mil_palette,
                labs_y = 'Região de Governo',
                labs_title = 'Geração de Empregos por 100 mil habitantes',
                labs_subtitle = 'Comparação entre Regiões de Governo de SP - Jan/2019 a Set/2019')
# ggsave('dotplot_empregos_100mil_hab_2019.png', height = 8, width = 6)

# Saldo vagas
saldo_breaks = c(-1417, -850, -570, 0, 1391, 2530, 4600, 18500, 100058)
saldo_palette = c("#D6604D", "#F4A582", "#FDDBC7", "#DEEBF7", "#9ECAE1", "#4292C6", "#2171B5", "#08306B")

sp_sf_gov %>% 
  filter(regiao_governo != 'São Paulo') %>% 
  mutate(saldo_milhares = `Saldo de empregos` / 1e+3) %>% 
  dotplot_regions(var_plot = saldo_milhares,
                  breaks = saldo_breaks / 1e+3, palette = saldo_palette,
                  labs_x = 'Saldo de empregos (milhares de vagas)',
                  labs_y = 'Região de Governo',
                  labs_title = 'Geração de Empregos em São Paulo em 2019',
                  labs_subtitle = 'Comparação entre Regiões de Governo (exceto Capital)')
# ggsave('dotplot_saldo_vagas_2019.png', height = 8, width = 6)

#### MAPAS ####
# Modo estático
tmap::tmap_mode("plot")

# Saldo de salários
map1 <- reg_gov_salario %>%
  mutate(`Saldo (milhares de R$)` = saldo_massa_salarial / 1e+3) %>%
  add_geometry_regioes_gov() %>%
  map_regions_divergent(
    var_plot = 'Saldo (milhares de R$)',
    map_title = 'Regiões Gov. SP - Saldo de Salários - Acumulado Jan/2019 a Set/2019',
    map_palette = c("#67001F", "#D6604D", "#F4A582", "#FDDBC7", "#DEEBF7", "#9ECAE1", "#2171B5", "#08306B"))
# tmap::tmap_save(map1, filename = 'mapa_massa_salarial_absoluta_2019.png', width = 7, height = 5.5)

# Saldo de vagas
map2 <- 
  map_regions_divergent(
    sp_sf_gov,
    var_plot = "Saldo de empregos",
    map_title = 'Regiões Gov. SP - Geração de empregos - Acumulado Jan/2019 a Set/2019',
    map_breaks = saldo_breaks,
    map_palette = saldo_palette)
# tmap::tmap_save(map2, filename = 'mapa_saldo_2019.png', width = 7, height = 5.5)

# Saldo de vagas/100 mil hab
map3 <- 
  map_regions_divergent(
    sp_sf_gov,
    var_plot = "Empregos gerados/100mil hab.",
    map_title = 'Regiões Gov. SP - Empregos gerados/100mil hab. - Jan/2019 a Set/2019',
    map_breaks = saldo_100mil_breaks,
    map_palette = saldo_100mil_palette)
# tmap::tmap_save(map3, filename = 'mapa_saldo_100mil_hab_2019.png', width = 7, height = 5.5)

# Total salários vagas criadas
map4 <- reg_gov_salario %>%
  mutate(`Salários criados (milhões de R$)` = massa_salarial_criadas / 1e+6) %>%
  add_geometry_regioes_gov() %>%
  map_regions_sequential(
    var_plot = 'Salários criados (milhões de R$)',
    map_title = 'Regiões Gov. SP - Soma dos salários das vagas criadas - Jan/2019 a Set/2019',
    map_palette = c(RColorBrewer::brewer.pal(5, 'Blues'), "#08306B"))
# tmap::tmap_save(map4, filename = 'mapa_salarios_criados_2019.png', width = 7, height = 5.5)