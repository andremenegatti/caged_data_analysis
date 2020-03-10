library(cagedExplorer)
library(magrittr)
library(tmap)

# READING DATA ----------------------------------------------------------------
df_caged <- read_caged_rds(data_folder = 'data',
                           start = '2019-01',
                           end = '2019-12',
                           nested = FALSE,
                           stringAsFactors = TRUE,
                           periodos_12meses = FALSE) %>% 
  filter(regiao_administrativa == 'Ribeirão Preto') # <<< Filtrando Ribeirão

# AGREGANDO E CALCULANDO VARIAVEIS DE INTERESSE -------------------------------
# Saldo por municipio
df_reg_adm_rp <- df_caged %>%
  compute_job_creation(municipio, Codmun7, pop) %>%
  arrange(desc(saldo)) %T>%
  write.csv('dados_caged_municipios_regiao_adm_ribeirao.csv',
            row.names = FALSE) %>% 
  add_geometry_municipios()

# DOTPLOT ---------------------------------------------------------------------
classInt::classIntervals(df_reg_adm_rp$saldo, n = 7, style = 'quantile')
intervals <- c(-50, -40, -20, 0, 20, 40, 100, 200, 300, 500, 1043.4, 2820)
palette <- c(reds_full[4:6], blues_full[2:9])

df_reg_adm_rp %>% 
  mutate(municipio = fct_reorder(municipio, saldo)) %>% 
  mutate(grupo_saldo = group_by_breaks(saldo, breaks = intervals) %>%
           as.factor() %>% fct_reorder(saldo)) %>% 
  ggplot() +
  geom_point(aes(x = saldo, y = municipio, fill = grupo_saldo),
             size = 5, shape = 21, col = 'gray') +
  geom_vline(xintercept = 0, col = 'darkred', alpha = 0.5) +
  geom_text(aes(x = saldo + 170, y = municipio, label = saldo),
            col = 'gray25', size = 3.5,
            family = 'serif') +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) +
  custom_theme() +
  theme(legend.position = 'none',
        axis.title.y = element_blank()) +
  labs(
    x =  'Saldo de vagas',
    y = 'Município',
    title = 'Geração de Empregos na Região Adm. de Ribeirão Preto',
    subtitle = 'Saldo acumulado dos municípios, jan/2019 a dez/2019'
    )

# Salvando
ggsave('dotplot_saldo_empregos_2019_reg_adm_ribeirao.png',
       height = 7, width = 6.5)

# MAPAS -----------------------------------------------------------------------
# Modo estático
tmap_mode("plot")

# Desenhando mapa
mapa_saldo <- 
  df_reg_adm_rp %>% 
  mutate(municipio = as.character(municipio)) %>% 
  mutate(`Saldo` = saldo,
         municipio_short = 
           case_when(municipio == 'Santo Antônio da Alegria' ~ 'Sto. Ant. da Alegria',
                     municipio == 'Santa Cruz da Esperança' ~ 'Sta. C. da Esp.',
                     municipio == 'Santa Rosa de Viterbo' ~ 'Sta. R. de Viterbo',
                     municipio == 'Cássia dos Coqueiros' ~ 'C. dos Coqueiros',
                     TRUE ~ municipio)) %>% 
  tm_shape() +
  tm_style("beaver",
           legend.format = list(fun = format_number, text.separator = " a ")) +
  tm_fill('Saldo',
          style = 'fixed',
          breaks = intervals,
          palette = palette,
          alpha = 1,
          id = "municipio") +
  tm_layout(main.title.size = 1.2,
            fontfamily = 'serif',
            main.title.fontface = 'bold',
            scale = 1.1,
            bg.color = "white",
            inner.margins = c(.1, .1, .1, .1),
            main.title = 'Região Administrativa de Ribeirão Preto - Geração de Emprego em 2019') +
  tm_compass(north = 0,
            type = "8star",
            size = 2,
            position = c("right", "bottom")) +
  tm_scale_bar(text.size = 0.6,
            text.color = NA,
            lwd = 1,
            color.dark = "black",
            color.light = "white") +
  tm_text("municipio_short", size = 0.5) +
  tm_legend(legend.position = c(0.01,0.08)) +
  tm_borders(col = "black", lwd = 0.3) ; mapa_saldo

# Salvando
tmap_save(mapa_saldo, width = 7, height = 5.5,
          filename = 'mapa_regiao_adm_ribeirao.png')
