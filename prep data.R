source('source.R')
library(arrow)

# load files -----
distritos <- read_sf('dados/distritos.geojson')
zonas <- read_sf('dados/zonas.geojson')
zonas$pop_22 <- zonas$`População 2022` |> gsub(',', '', x = _) |> as.numeric()

hospitais.full <- read_csv('dados/hospitais.csv') |> as_geodf()
hospitais.full$eq_leitos <- as.numeric(hospitais.full$eq_leitos)
hospitais.full$eq_esfera <- ifelse(hospitais.full$eq_esfera != 'Privado', 'Público', hospitais.full$eq_esfera)
hospitais <- hospitais.full |>  filter(eq_esfera == 'Público')

censo <- read_sf('dados/censo_22.geojson')
censo$area_ori <- as.numeric(st_area(censo))

municipio <- st_union(zonas)

# areas de influencia -----
bb <- st_bbox(distritos)

p <- matrix(
  c(bb["xmin"], bb["ymin"], 
    bb["xmin"], bb["ymax"],
    bb["xmax"], bb["ymax"], 
    bb["xmax"], bb["ymin"], 
    bb["xmin"], bb["ymin"]),
  ncol = 2, byrow = T
)

voronoi <- 
  sf::st_voronoi(hospitais |> st_cast('POINT') |> st_union(), 
                 sf::st_polygon(list(p))) |>
  st_collection_extract()

voronoi <- st_intersection(voronoi, municipio) |> st_as_sf()
voronoi <- st_join(voronoi, hospitais |> filter(eq_esfera == 'Público'))

buff.2k <-
  st_buffer(hospitais |> st_cast('POINT'), 2000/111000)

# merge censo -----
voronoi.censo <- st_intersection(voronoi, censo)
voronoi.censo$prop_area <- as.numeric(st_area(voronoi.censo)) / voronoi.censo$area_ori
voronoi.censo$`População 2022` <- round(voronoi.censo$prop_area * voronoi.censo$v0001)
voronoi.censo <-
  voronoi.censo |>
  as.data.frame() |>
  group_by(eq_cnes) |>
  summarise(`População 2022` = sum(`População 2022`)) |>
  ungroup() |>
  merge(voronoi, by = 'eq_cnes')

st_geometry(voronoi.censo) <- 'x'

buff.2k.censo <- st_intersection(buff.2k, censo)
buff.2k.censo$prop_area <- as.numeric(st_area(buff.2k.censo)) / buff.2k.censo$area_ori
buff.2k.censo$`População 2022` <- round(buff.2k.censo$prop_area * buff.2k.censo$v0001)
buff.2k.censo <-
  buff.2k.censo |>
  as.data.frame() |>
  group_by(eq_cnes) |>
  summarise(`População 2022` = sum(`População 2022`)) |>
  ungroup() |>
  merge(buff.2k, by = 'eq_cnes')

st_geometry(buff.2k.censo) <- 'geometry'


# basemap -----
n.hosp.distr <- 
  st_intersection(distritos, hospitais.full) |>
  as.data.frame() |>
  group_by(ds_nome, eq_esfera) |> 
  summarise(qtde = n()) |> 
  pivot_wider(names_from = c(eq_esfera), values_from = c(qtde)) |>
  mutate(Total = sum(c(Público, Privado), na.rm = T))

distritos <- merge(distritos, n.hosp.distr, by = 'ds_nome', all.x = T)
distritos$Total <- ifelse(is.na(distritos$Total), 0, distritos$Total)

ggplot() +
  geom_sf(data = distritos, aes(color = 'Distritos'), fill = '#d6d9c9') +
  geom_sf(data = st_boundary(zonas), aes(color = 'Zonas'), linewidth = .5) +
  geom_sf(data = hospitais.full, aes(color = eq_esfera), size = 2) +
  scale_color_manual(values = c('white', '#dd4b56', '#4b9fdd', '#3f464c')) +
  labs(title = 'Distribuição dos hospitais no território', color = '') +
  annotation_scale() +
  annotation_north_arrow(pad_x = unit(7, "cm"),  width = unit(.5, "cm"), height = unit(.5, "cm"), pad_y = unit(1, "cm")) +
  tema.mapa

ggsave('plots/mapa_hospitais.png', width = 6, height = 6)



# mapa acessbilidade teorica - censo ----
ggplot() +
  geom_sf(data = censo, aes(fill = v0001), linewidth = 0) +
  geom_sf(data = st_boundary(voronoi), linetype = 'dashed', aes(color = 'Voronoi')) +
  geom_sf(data = hospitais, aes(color = 'Hospitais'), size = 2, shape = 20) +
  scale_color_manual(values = c('#176275', '#31243e')) +
  scale_fill_paletteer_c('harrypotter::gryffindor', direction = -1) +
  labs(fill = 'População\nCenso 2022',
       color = '', 
       title = '\nAcessibilidade Teórica',
       caption = 'IBGE Censo 2022\nDataSUS') + 
  annotation_scale() +
  annotation_north_arrow(pad_x = unit(7, "cm"),  width = unit(.5, "cm"), height = unit(.5, "cm"), pad_y = unit(1, "cm")) +
  tema.mapa
ggsave('plots/acessibilidade_teorica.png', width = 6, height = 6)


# mapa acessbilidade teorica leitos p mil ----

voronoi.censo |>
  mutate(`Qtde. de Leitos` = eq_leitos,
         leitos_mil_hab = (eq_leitos / `População 2022`)*1e3) |>
  mutate(lbl_leitos = cut(leitos_mil_hab,
                          breaks = c(min(leitos_mil_hab),
                                     quantile(leitos_mil_hab, .25),
                                     mean(leitos_mil_hab),
                                     median(leitos_mil_hab),
                                     quantile(leitos_mil_hab, .75),
                                     max(leitos_mil_hab)) )) |>
  ggplot() +
  geom_sf(aes(fill = lbl_leitos), linewidth = 0) +
  geom_sf(data = st_boundary(zonas), aes(color = 'Zonas')) +
  scale_color_manual(values = c('black')) +
  # scale_fill_paletteer_c('harrypotter::gryffindor', direction = -1) +
  labs(fill = 'Leitos por Mil hab',
       title = '\nAcessibilidade Teórica - Leitos',
       caption = 'IBGE Censo 2022\nDataSUS', color = '') + 
  scale_fill_paletteer_d('ggsci::deep_purple_material') +
  annotation_scale() +
  annotation_north_arrow(pad_x = unit(7, "cm"),  width = unit(.5, "cm"), height = unit(.5, "cm"), pad_y = unit(1, "cm")) +
  tema.mapa
ggsave('plots/acessibilidade_teorica_leitos.png', width = 6, height = 6)


# correlacao leitos x pop (teorica) -----
ggplot(voronoi.censo, 
       aes(x = `População 2022`/1000, y = eq_leitos)) +
  geom_point() +
  geom_smooth(method = "gam",
              se = FALSE,
              color = cores$chart.1) +
  labs(title = '', 
       x = TeX('\\frac{População 2022}{1000}'),
       y = 'Qtde. de leitos') +
  tema.grafico
ggsave('plots/corr_teorica.png', width = 4, height = 4)


# mapa acessbilidade real - censo ----
ggplot() +
  geom_sf(data = censo, aes(fill = v0001), linewidth = 0) +
  geom_sf(data = st_boundary(buff.2k), linetype = 'dashed', aes(color = 'Raio de 2Km')) +
  geom_sf(data = hospitais, aes(color = 'Hospitais'), size = 2, shape = 20) +
  scale_color_manual(values = c('#176275', '#31243e')) +
  scale_fill_paletteer_c('harrypotter::gryffindor', direction = -1) +
  labs(fill = 'População\nCenso 2022',
       color = '', 
       title = '\nAcessibilidade Real',
       caption = 'IBGE Censo 2022\nDataSUS') + 
  annotation_scale() +
  annotation_north_arrow(pad_x = unit(7, "cm"),  width = unit(.5, "cm"), height = unit(.5, "cm"), pad_y = unit(1, "cm")) +
  tema.mapa
ggsave('plots/acessibilidade_real.png', width = 6, height = 6)


# mapa acessbilidade real leitos p mil ----

buff.2k.censo |>
  mutate(`Qtde. de Leitos` = eq_leitos,
         leitos_mil_hab = (eq_leitos / `População 2022`)*1e3) |>
  mutate(lbl_leitos = cut(leitos_mil_hab,
                          breaks = c(min(leitos_mil_hab),
                                     quantile(leitos_mil_hab, .25),
                                     mean(leitos_mil_hab),
                                     median(leitos_mil_hab),
                                     quantile(leitos_mil_hab, .75),
                                     max(leitos_mil_hab)) )) |>
  ggplot() +
  geom_sf(data = st_boundary(zonas), aes(color = 'Zonas')) +
  scale_color_manual(values = c('black')) +
  geom_sf(aes(fill = lbl_leitos), linewidth = 0) +
  # scale_fill_paletteer_c('harrypotter::gryffindor', direction = -1) +
  labs(fill = 'Leitos por Mil hab',
       title = '\nAcessibilidade Real - Leitos',
       caption = 'IBGE Censo 2022\nDataSUS', 
       color = '') + 
  scale_fill_paletteer_d('ggsci::deep_purple_material') +
  annotation_scale() +
  annotation_north_arrow(pad_x = unit(7, "cm"),  width = unit(.5, "cm"), height = unit(.5, "cm"), pad_y = unit(1, "cm")) +
  tema.mapa
ggsave('plots/acessibilidade_real_leitos.png', width = 6, height = 6)


# correlacao leitos x pop (real) -----
ggplot(buff.2k.censo, 
       aes(x = `População 2022`/1000, y = eq_leitos)) +
  geom_point() +
  geom_smooth(method = "gam",
              se = FALSE,
              color = cores$chart.1) +
  labs(title = '', 
       x = TeX('\\frac{População 2022}{1000}'),
       y = 'Qtde. de leitos') +
  tema.grafico
ggsave('plots/corr_real.png', width = 4, height = 4)


# zoom ZL acessibilidade real -----
zoom_zl <- read_sf('dados/zoom_zl.geojson')

ploc <-
  ggplot() +
  geom_sf(data = zonas) +
  geom_sf(data = st_boundary(zoom_zl), color = 'red') +
  theme_void()

ggplot() +
  geom_sf(data = st_crop(censo, zoom_zl), aes(fill = v0001), linewidth = 0) +
  geom_sf(data = st_boundary(st_crop(buff.2k, zoom_zl)), linewidth = 2, linetype = 'dashed', aes(color = 'Buffer 2Km')) +
  geom_sf(data = st_crop(hospitais, zoom_zl), aes(color = 'Hospitais'), size = 4, shape = 20) +
  scale_color_manual(values = c('black', '#176275')) +
  scale_fill_paletteer_c('harrypotter::gryffindor', direction = -1) +
  labs(fill = 'População\nCenso 2022',
       color = '', 
       title = '\nAcessibilidade Real Zona Leste',
       subtitle = 'Area de abrandência de 2Km em torno de cada hospital',
       caption = 'IBGE Censo 2022\nDataSUS') + 
  annotation_scale() +
  annotation_north_arrow(pad_x = unit(8, "cm")) +
  tema.mapa + 
  inset_element(ploc, left = 1, right = 1.3, bottom = 0, top = .2)

ggsave('plots/acessibilidade_real_zl.png', width = 6, height = 6)


# renda -----
censo.renda <- readRDS('dados/censo_10.rds')
censo.renda$renda_media_domiciliar <- censo.renda$renda_media_domiciliar * 1.1455

salario_minimo <- 1320
faixas <- c(0, 1, 2, 3, 5, Inf) * salario_minimo

rotulos <- c(
  "Até 1 SM",
  "De 1 a 2 SM",
  "De 2 a 3 SM",
  "De 3 a 5 SM",
  "Mais de 5 SM"
)

censo.renda$renda_categoria <- cut(
  censo.renda$renda_media_domiciliar,
  breaks = faixas,
  labels = rotulos,
  include.lowest = TRUE
)

ggplot() +
  geom_sf(data = censo.renda, aes(fill = renda_categoria), linewidth = 0) +
  geom_sf(data = st_boundary(buff.2k), linetype = 'dashed', aes(color = 'Raio de 2Km')) +
  geom_sf(data = hospitais, aes(color = 'Hospitais'), size = 2, shape = 20) +
  scale_color_manual(values = c('tomato', '#31243e')) +
  scale_fill_paletteer_d('ggsci::blue_material', direction = 1) +
  labs(fill = 'Renda média domiciliar* \nCenso 2010\n',
       color = '', 
       title = '\nAcessibilidade Real e Renda',
       caption = 'IBGE Censo 2010\nDataSUS\n*Valores corrigidos com IPCA para 2022') + 
  annotation_scale() +
  annotation_north_arrow(pad_x = unit(7, "cm"),  width = unit(.5, "cm"), height = unit(.5, "cm"), pad_y = unit(1, "cm")) +
  tema.mapa

ggsave('plots/acessibilidade_real_renda.png', width = 6, height = 6)


# mapa leitos + corr -----------
p.leitos <-
  voronoi.censo |>
  mutate(`Qtde. de Leitos` = eq_leitos,
         leitos_mil_hab = (eq_leitos / `População 2022`)*1e3) |>
  mutate(lbl_leitos = cut(leitos_mil_hab,
                          breaks = c(min(leitos_mil_hab),
                                     quantile(leitos_mil_hab, .25),
                                     mean(leitos_mil_hab),
                                     median(leitos_mil_hab),
                                     quantile(leitos_mil_hab, .75),
                                     max(leitos_mil_hab)) )) |>
  ggplot() +
  geom_sf(aes(fill = lbl_leitos), linewidth = 0) +
  geom_sf(data = st_boundary(zonas), aes(color = 'Zonas')) +
  scale_color_manual(values = c('black')) +
  # scale_fill_paletteer_c('harrypotter::gryffindor', direction = -1) +
  labs(fill = 'Leitos por Mil hab',
       title = '\nAcessibilidade Teórica - Leitos',
       caption = 'IBGE Censo 2022\nDataSUS', color = '') + 
  scale_fill_paletteer_d('ggsci::deep_purple_material') +
  annotation_scale() +
  annotation_north_arrow(pad_x = unit(7, "cm"),  width = unit(.5, "cm"), height = unit(.5, "cm"), pad_y = unit(1, "cm")) +
  tema.mapa


p.corr <-
  ggplot(voronoi.censo, 
         aes(x = `População 2022`/1000, y = eq_leitos)) +
  geom_point(size = .5) +
  geom_smooth(method = "gam",
              se = FALSE,
              color = cores$chart.1,
              linewidth = .5) +
  labs(title = '', 
       x = 'Habitantes',
       y = 'Leitos') +
  tema.grafico +
  theme(plot.background = element_rect(fill = 'transparent'),
        panel.background  = element_rect(fill = 'transparent'),
        axis.text = element_text(size =  6),
        axis.title = element_text(size =  6))


p.leitos + inset_element(p.corr, left = .8, right = 1.4, bottom = .8, top = 1)
ggsave('plots/acessibilidade_teorica_leitos_mpcorr.png', width = 6, height = 8)


###

p.leitos <-
  buff.2k.censo |>
  mutate(`Qtde. de Leitos` = eq_leitos,
         leitos_mil_hab = (eq_leitos / `População 2022`)*1e3) |>
  mutate(lbl_leitos = cut(leitos_mil_hab,
                          breaks = c(min(leitos_mil_hab),
                                     quantile(leitos_mil_hab, .25),
                                     mean(leitos_mil_hab),
                                     median(leitos_mil_hab),
                                     quantile(leitos_mil_hab, .75),
                                     max(leitos_mil_hab)) )) |>
  ggplot() +
  geom_sf(aes(fill = lbl_leitos), linewidth = 0) +
  geom_sf(data = st_boundary(zonas), aes(color = 'Zonas')) +
  scale_color_manual(values = c('black')) +
  # scale_fill_paletteer_c('harrypotter::gryffindor', direction = -1) +
  labs(fill = 'Leitos por Mil hab',
       title = '\nAcessibilidade Real - Leitos',
       caption = 'IBGE Censo 2022\nDataSUS', color = '') + 
  scale_fill_paletteer_d('ggsci::deep_purple_material') +
  annotation_scale() +
  annotation_north_arrow(pad_x = unit(7, "cm"),  width = unit(.5, "cm"), height = unit(.5, "cm"), pad_y = unit(1, "cm")) +
  tema.mapa


p.corr <-
  ggplot(buff.2k.censo, 
         aes(x = `População 2022`/1000, y = eq_leitos)) +
  geom_point(size = .5) +
  geom_smooth(method = "gam",
              se = FALSE,
              color = cores$chart.1,
              linewidth = .5) +
  labs(title = '', 
       x = 'Habitantes',
       y = 'Leitos') +
  tema.grafico +
  theme(plot.background = element_rect(fill = 'transparent'),
        panel.background  = element_rect(fill = 'transparent'),
        axis.text = element_text(size =  6),
        axis.title = element_text(size =  6))

p.leitos + inset_element(p.corr, left = .8, right = 1.4, bottom = .8, top = 1)
ggsave('plots/acessibilidade_real_leitos_mpcorr.png', width = 6, height = 8)

