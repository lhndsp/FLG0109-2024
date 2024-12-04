source('R/source.R')

# load files ----

## zonas ------
zonas <- read_sf('dados/zonas.geojson')
zonas$pop_22 <- zonas$`População 2022` |> gsub(',', '', x = _) |> as.numeric()

##  hospitais ------
hospitais <- read_geo('select * from sao_paulo.hospitais') # |> filter(eq_esfera != 'Privado')
hospitais$eq_leitos <- as.numeric(hospitais$eq_leitos)

hospitais$eq_esfera |> table()

hospitais$eq_esfera <- ifelse(hospitais$eq_esfera != 'Privado', 'Público', hospitais$eq_esfera)

# basemaps -----

distritos <- read_geo('select * from sao_paulo.geosampa_distritos')

n.hosp.distr <- 
  st_intersection(distritos, hospitais) |>
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
  geom_sf(data = hospitais, aes(color = eq_esfera), size = 2) +
  scale_color_manual(values = c('white', '#dd4b56', '#4b9fdd', '#3f464c')) +
  labs(title = 'Distribuição dos hospitais no território', color = '') +
  annotation_scale() +
  annotation_north_arrow(pad_x = unit(7, "cm"),  width = unit(.5, "cm"), height = unit(.5, "cm"), pad_y = unit(1, "cm")) +
  tema.mapa

ggsave('R/mapas/mapa_hospitais.png', width = 6, height = 6)

## censo ----
censo <- read_sf('dados/censo_22.geojson')
censo$area_ori <- as.numeric(st_area(censo))

## area de influencia  -------

### voronoi ------
voronoi <- read_sf('dados/voronoi.geojson')
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

st_geometry(voronoi.censo) <- 'geometry'

### buffer ------
buff.2k <- read_sf('dados/buffer_2km.geojson')
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

# Estatíticas -----

## acessbilidade teorica -----

### Mapa com censo ----
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
ggsave('R/mapas/acessibilidade_teorica.png', width = 6, height = 6)


### Mapa com leitos p mil ----

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
ggsave('R/mapas/acessibilidade_teorica_leitos.png', width = 6, height = 6)


### Correlaçao -----
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
ggsave('R/mapas/corr_teorica.png', width = 4, height = 4)


## acessbilidade real -----

### Mapa com censo ----
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
ggsave('R/mapas/acessibilidade_real.png', width = 6, height = 6)


### Mapa com leitos p mil ----

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
ggsave('R/mapas/acessibilidade_real_leitos.png', width = 6, height = 6)


### Correlaçao -----
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
ggsave('R/mapas/corr_real.png', width = 4, height = 4)


### ZL -------

zoom_zl <- read_sf('dados/zoom_zl.geojson')

ploc <-
  ggplot() +
  geom_sf(data = zonas) +
  geom_sf(data = st_boundary(zoom_zl), color = 'red') +
  theme_void()

ggplot() +
  geom_sf(data = st_crop(censo, zoom_zl), aes(fill = v0001), linewidth = 0) +
  geom_sf(data = st_boundary(st_crop(buff.2k, zoom_zl)), linewidth = 1, linetype = 'dashed', aes(color = 'Buffer 2Km')) +
  geom_sf(data = st_crop(hospitais, zoom_zl), aes(color = 'Hospitais'), size = 2, shape = 20) +
  scale_color_manual(values = c('#176275', '#31243e')) +
  scale_fill_paletteer_c('harrypotter::gryffindor', direction = -1) +
  labs(fill = 'População\nCenso 2022',
       color = '', 
       title = '\nAcessibilidade Real Zona Leste',
       subtitle = 'Area de abrandência de 2Km em torno de cada hospital',
       caption = 'IBGE Censo 2022\nDataSUS') + 
  annotation_scale() +
  annotation_north_arrow(pad_x = unit(10, "cm")) +
  tema.mapa + 
  inset_element(ploc, left = .8, right = 1, bottom = 0, top = 0.3)

ggsave('R/mapas/acessibilidade_real_zl.png', width = 6, height = 6)

  

# renda -----
censo.renda <- 
    read_geo("select code_tract,
       renda_mensal_domicilio::numeric / n_dom_tot::numeric as renda_media_domiciliar,
       geom
from ibge.ibge_censo2010_dados_corr censo
join ibge.ibge_setores_censitarios_brasil setor on setor.cd_geocodi::numeric = censo.code_tract::numeric
where censo.code_municipio::numeric = 3550308 and
      n_dom_tot::numeric > 0")


censo.renda$renda_media_domiciliar <- censo.renda$renda_media_domiciliar * 1.1455

salario_minimo <- 1320
faixas <- c(0, 1, 2, 3, 5, Inf) * salario_minimo

# Cria os rótulos das faixas
rotulos <- c(
  "Até 1 SM",
  "De 1 a 2 SM",
  "De 2 a 3 SM",
  "De 3 a 5 SM",
  "Mais de 5 SM"
)

# Categoriza os dados com base nas faixas
censo.renda$censo.renda <- cut(
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

ggsave('R/mapas/acessibilidade_real_renda.png', width = 6, height = 6)




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
ggsave('R/mapas/acessibilidade_teorica_leitos_mpcorr.png', width = 6, height = 8)


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
ggsave('R/mapas/acessibilidade_real_leitos_mpcorr.png', width = 6, height = 8)

  
# 
# #voronoi$area_ori <- as.numeric(st_area(voronoi))
# 
# 
# #buff.2k$area_ori <- as.numeric(st_area(buff.2k))
# buff.2k.censo <- st_intersection(buff.2k, censo)
# buff.2k.censo$prop_area <- as.numeric(st_area(buff.2k.censo)) / buff.2k.censo$area_ori
# buff.2k.censo$`População 2022` <- round(buff.2k.censo$prop_area * buff.2k.censo$v0001)
# buff.2k.censo <-
#   buff.2k.censo |>
#   group_by(eq_cnes) |>
#   summarise(`População 2022` = sum(`População 2022`),
#             `Qtde Leitos` = sum(eq_leitos),
#             `Leitos por Mil Hab` = sum(`População 2022`)/sum(eq_leitos)* 1e3)
# 
# (( hospitais$eq_leitos |> sum(na.rm = T) ) / ( censo$v0001 |> sum() )) * 1e3
# (( hospitais[hospitais$eq_esfera != 'Privado',]$eq_leitos |> sum(na.rm = T) ) / ( censo$v0001 |> sum() )) * 1e3
# 
# hospitais.desc <-
#   hospitais |>
#   as.data.frame() |>
#   mutate(eq_esfera = ifelse(eq_esfera != 'Privado', 'Publico', eq_esfera)) |>
#   group_by(eq_regiao5, eq_esfera) |> 
#   summarise(qtd_hosp = n(),
#             qtd_leitos = sum(eq_leitos, na.rm = T)) |>
#   pivot_wider(names_from = eq_esfera, values_from = c(qtd_hosp, qtd_leitos)) |>
#   mutate(qtd_hosp_tot = qtd_hosp_Privado + qtd_hosp_Publico,
#          qtd_leitos_tot = qtd_leitos_Privado + qtd_leitos_Publico) |>
#   ungroup() 
# 
# hospitais.desc <-
#   merge(hospitais.desc, 
#         zonas |> as.data.frame() |> select(pop_22, NOME), 
#         by.x = 'eq_regiao5', by.y = 'NOME')
# 
# hospitais.desc <- 
#   bind_rows(hospitais.desc, 
#             hospitais.desc |> select(-eq_regiao5) |> colSums())
# 
# 
# 
# hospitais.desc$eq_regiao5[6] <- 'Município'
# 
# hospitais.desc <-
#   hospitais.desc |>
#   mutate(leito_tot_pmil = (qtd_leitos_tot/pop_22)*1e3,
#          leito_pub_pmil = (qtd_leitos_Publico/pop_22)*1e3)
#   
# hospitais.desc |>
#   select(pop_22, qtd_hosp_tot, qtd_hosp_Publico) |>
#   rename(`População\n2022` = pop_22,
#          `Qtde. hospitais\ntotal` = qtd_hosp_tot,
#          `Qtde. hospitais\npublicos` = qtd_hosp_Publico) |>
#   round(2) |>
#   mutate(`População\n2022` = format(`População\n2022`, big.mark = ',')) |>
#   plot_tab(rows = hospitais.desc$eq_regiao5, col.size = .7, row.size =.7, txt.size = .7, fg_c = c('black'))
# 
# ggsave('R/mapas/tabela_zonas_1.png', width = 4, height = 2)
# 
# 
# hospitais.desc |>
#   select(qtd_leitos_Publico, qtd_leitos_tot, leito_tot_pmil, leito_pub_pmil) |>
#   rename(`Qtde. leitos\ntotal` = qtd_leitos_tot,
#          `Qtde. leitos\npublicos` = qtd_leitos_Publico,
#          `Leitos\npor mil hab\ntotal` = leito_tot_pmil,
#          `Leitos\npor mil hab\npublicos` = leito_pub_pmil) |>
#   round(2) |>
#   mutate(`Qtde. leitos\ntotal` = format(`Qtde. leitos\ntotal`, big.mark = ','),
#          `Qtde. leitos\npublicos` = format(`Qtde. leitos\npublicos`, big.mark = ',')) |>
#   plot_tab(rows = hospitais.desc$eq_regiao5, col.size = .7, row.size = .7, txt.size = .7, fg_c = c('black'))
# 
# ggsave('R/mapas/tabela_zonas_2.png', width = 4, height = 2)
# 
# 
# censo$pop_m2 <- censo$v0001   / as.numeric(st_area(censo)) 
# censo$pop_m2 |> is.infinite()
# 
# 
# ggplot() +
#   geom_sf(data = censo, aes(fill = v0001), linewidth = 0) +
#   geom_sf(data = st_boundary(buff.2k), linetype = 'dashed', aes(color = 'Buffer 2Km')) +
#   geom_sf(data = hospitais, aes(color = 'Hospitais'), size = 2, shape = 20) +
#   scale_color_manual(values = c('#176275', '#31243e')) +
#   scale_fill_paletteer_c('harrypotter::gryffindor', direction = -1) +
#   labs(fill = 'População\nCenso 2022',
#        color = '', 
#        title = '\nAcessibilidade Real',
#        subtitle = 'Area de abrandência de 2Km em torno de cada hospital',
#        caption = 'IBGE Censo 2022\nDataSUS') + 
#   annotation_scale() +
#   annotation_north_arrow(pad_x = unit(10, "cm")) +
#   tema.mapa
# ggsave('R/mapas/acessibilidade_real.png', width = 8.3, height = 8.99)
# 
# (sum(buff.2k.censo$`População 2022`)/sum(zonas$pop_22))*100
# 
# censo.zl <- st_crop(censo, read_sf('dados/zoom_zl.geojson'))
# 
# ggplot() +
#   geom_sf(data = censo.zl, aes(fill = v0001), linewidth = 0) +
#   geom_sf(data = st_boundary(st_crop(buff.2k, read_sf('dados/zoom_zl.geojson'))), linetype = 'dashed', aes(color = 'Buffer 2Km')) +
#   geom_sf(data = st_crop(hospitais, read_sf('dados/zoom_zl.geojson')), aes(color = 'Hospitais'), size = 2, shape = 20) +
#   scale_color_manual(values = c('#176275', '#31243e')) +
#   scale_fill_paletteer_c('harrypotter::gryffindor', direction = -1) +
#   labs(fill = 'População\nCenso 2022',
#        color = '', 
#        title = '\nAcessibilidade Real Zona Leste',
#        subtitle = 'Area de abrandência de 2Km em torno de cada hospital',
#        caption = 'IBGE Censo 2022\nDataSUS') + 
#   annotation_scale() +
#   annotation_north_arrow(pad_x = unit(10, "cm")) +
#   tema.mapa
# ggsave('R/mapas/acessibilidade_real_zl.png', width = 8.3, height = 8.99)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ggplot(buff.2k.censo, 
#        aes(x = log(`População 2022`), y = log(`Qtde Leitos`))) +
#   geom_point() +
#   geom_smooth(method = "gam",
#               se = FALSE,
#               color = cores$chart.1) +
#   labs(title = 'Leitos por pessoa') +
#   tema.grafico
# ggsave('R/mapas/corr_real.png', width = 4, height = 4)
# 
# #ggsave('R/mapas/corr.png', width = 8.3, height = 8.99)
# 
# 
# 
