
library(sf)
library(sp)
library(dplyr)
library(tidyr)
library(jsonlite)
library(readr)
library(grid)
library(gridExtra)
library(ggplot2)
library(orion)
library(paletteer)
library(maptools)
library(ggspatial)
library(patchwork)
library(latex2exp)

sf_use_s2(FALSE)

orion::orion.config("credentials.env")

fontnm <- 'Helvética'
map.p.shape = 18

# cmaps ----------------------------------
cores <-
  list(fill.dt0 = '#18a1af',
       fill.dt1 = '#dc2825',
       txt.title = 'black',
       txt.subtitle = 'black',
       txt.pos = '#002170',
       txt.neg = '#ed1526',
       txt.geral = 'black',
       chart.1 = '#01506f',
       chart.2 = '#403e31',
       map.desocup.l = '#023048',
       map.ent.l = '#659aba'
  )

c.reds <-
  c("#FEB2B2",
    "#FC8181", 
    "#F56565", 
    "#E53E3E",
    "#C53030",
    "#9B2C2C", 
    "#822727", 
    "#63171B")

c.oranges <-
  c('#ffdfbd',
    '#ffd4ae',
    '#ffc99f',
    '#ffbd91',
    '#ffb282',
    '#ffa773',
    '#ff9c64',
    '#ff9056',
    '#ff8547',
    '#ff7a38')

c.rdbu <-
  c('#FC8181',
    '#E53E3E',
    '#822727',
    '#990e18',
    '#b9e3fa',
    '#81d4ec',
    '#548eb9',
    '#1e689c')


c.blues <-
  c("#E0F7FA", 
    "#B2EBF2",
    "#81DEEA", 
    "#4DD0E1", 
    "#26C6DA",
    "#00BCD4",
    "#00ACC1", 
    "#0097A7", 
    "#00838F", 
    "#006064")

# temas -----------------------------------------
sizes <-
  list(title = 16,
       subtitle = 12,
       axis.title = 9.5,
       axis.txt = 9.5,
       lgd.title = 9.5,
       lgd.txt = 9.5,
       chart.txt = 4,
       chart.lw.1 = 2,
       chart.lw.2 = 1,
       chart.p.s = 2.5,
       tbl.col = 1,
       tbl.txt = 1,
       tbl.txt.f = 1.2,
       tbl.txt.nf = .8,
       tbl.row = 1,
       map.p = 1)

tema.geral <-
  theme(
    plot.background = element_rect(fill = 'white'),
    panel.background  = element_rect(fill = 'white'),
    plot.margin=grid::unit(c(0,0,0,0), "mm"),
    plot.title = element_text(face= 'bold', 
                              family = fontnm, 
                              size = sizes$title, 
                              color = cores$txt.title),
    plot.subtitle = element_text(face = 'plain',
                                 family = fontnm,
                                 size = sizes$subtitle, 
                                 color = cores$txt.subtitle),
    legend.text = element_text(face = 'plain', 
                               family = fontnm,
                               size = sizes$lgd.txt, 
                               color = cores$txt.geral),
    legend.title = element_text(face = 'bold', 
                                family = fontnm,
                                size = cores$txt.geral, 
                                color = 'black', 
                                hjust = 0.5,
                                vjust = 0.5),
    strip.background = element_rect('transparent'),
    strip.text  = element_text(face = 'bold',
                               family = fontnm,
                               size = sizes$subtitle, 
                               color = cores$txt.title)
  )

tema.grafico <-
  tema.geral +
  theme(
    panel.grid.major.y = element_line(linewidth = .2, color = '#b6c8ce'),
    panel.grid.minor.y = element_line(linewidth = .1, color = '#b6c8ce'),
    axis.text = element_text(face = 'plain', 
                             family = 'Helvética',
                             size = sizes$axis.txt, 
                             color = 'black'),
    axis.title = element_text(face = 'bold',
                              family = 'Helvética',
                              size = sizes$axis.title, 
                              color = 'black')
  )

tema.mapa <-
  tema.geral +
  theme(
    # legend.margin = margin(0, 0, 0, 0),
    # legend.spacing.x = unit(0, "mm"),
    # legend.spacing.y = unit(0, "mm"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    # axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()#,
    #legend.position = 'bottom'
  )

