library(cagedExplorer)
library(tmap)

# IMPORTANDO DADOS ------------------------------------------------------------
df_caged <- read_caged_rds(data_folder = 'data',
                           start = '2019-01',
                           end = '2019-12',
                           nested = FALSE,
                           stringAsFactors = TRUE,
                           periodos_12meses = FALSE)

# AGREGANDO E CALCULANDO VARIAVEIS DE INTERESSE -------------------------------
# Saldo por município
municipios_acumulado_12_meses <- df_caged %>%
  compute_job_creation(municipio, Codmun7, regiao_governo, pop) %>%
  arrange(desc(saldo))

# Encontrando destaques: 5 melhores e 5 piores
df_destaques <- df_caged %>%
  compute_job_creation(municipio) %>%
  arrange(desc(saldo)) %>%
  slice(1:5, 641:645)

# Tabela de destaques com dados de setor
df_destaques_setor <- df_caged %>%
  compute_job_creation(municipio, setor) %>%
  group_by(municipio) %>%
  mutate(saldo_total = sum(saldo)) %>%
  ungroup() %>%
  arrange(desc(saldo_total)) %>%
  filter(municipio %in% df_destaques$municipio)

# Salvando dados para usar na redação do boletim
df_destaques_setor %>%
  write.csv('boletim_regioes/boletim-mar2020/data/destaques_municipios.csv',
            row.names = FALSE)

# MAPA ------------------------------------------------------------------------
# Adicionando polígonos para desenhar o mapa
municipios_acumulado_12_meses <- municipios_acumulado_12_meses %>%
  add_geometry_municipios()

# Preparando dados e definindo títulos
df <- municipios_acumulado_12_meses
df$`Saldo de Empregos` <- df$saldo
var_saldo <-  "Saldo de Empregos"
map_title <- "Geração de Emprego nos Municípios Paulistas - Jan/2019 a Dez/2019"

# Definindo quebras e cores
map_breaks <- c(min(df$`Saldo de Empregos`),
                -500, -100, 0,
                100, 500, 2000, 4000, 8000,
                max(df$`Saldo de Empregos`))

map_palette <- c(reds_full[c(2, 4, 5)],
                 blues_full[4:9])

# Desenhando mapa
map_mun <- tm_shape(df) +
  tm_style("beaver",
           legend.format = list(fun = format_number,
                                text.separator = " a ")) +
  tm_fill(var_saldo,
          palette = map_palette,
          style = 'fixed',
          breaks = map_breaks,
          alpha = 1,
          id = "regiao_governo") +
  tm_layout(main.title.size = 1.2,
            fontfamily = 'serif',
            main.title.fontface = 'bold',
            scale = 1.1,
            bg.color = "white",
            inner.margins = c(.1, .1, .1, .1),
            main.title = map_title) +
  tm_compass(north = 0,
             type = "8star",
             size = 2,
             position = c("right", "bottom")) +
  tm_scale_bar(text.size = 0.6,
               text.color = NA,
               lwd = 1,
               color.dark = "black",
               color.light = "white") +
  tm_legend(legend.position = c(0.01,0.08)) +
  tm_borders(col = "black", lwd = 0.3) ; map_mun

# Salvando mapa
tmap_save(map_mun,
          filename = 'boletim_regioes/boletim-mar2020/plots/mapa_saldo_vagas_municipios.png',
          width = 7, height = 5.5)
