# ---
# title: "Fitness distributions"
# author: "Moises Exposito-Alonso (moisesexpositoalonso@gmail.com)"
# date: '`r Sys.Date()`'
# output:
#   html_document: default
# ---


## Packages set up and loading data

library(knitr)
library(dplyr,tidyr)
library(ggplot2);library(cowplot)
library(devtools)
library(RColorBrewer)
# library(moiR)
load_all("~/moiR")
load_all(".")

data(field)
data(field.s)
data(field.c)
head(field.c)



ggplot(field.c) +
  geom_histogram(aes(x=Flowering_time,group=water,fill=water),alpha=0.8,bins=35) +
  scale_fill_manual(values = watercolors())+
  facet_grid(site~indpop)+
  theme_bw() +
  xlab('Flowering time') +
  ylab('# Pots') +
  scale_alpha(guide = 'none') ->
pflo
pflo


ggplot(field.c) +
  geom_histogram(aes(x=Germination_time,group=water,fill=water),alpha=0.8,bins=35) +
  scale_fill_manual(values = watercolors())+
  facet_grid(site~indpop)+
  theme_bw() +
  xlab('Germination time') +
  ylab('# Pots') +
  scale_alpha(guide = 'none') ->
pger
pger


save_plot(filename = 'figs/Figure_flowering_distribution.pdf',base_height = 6,base_width = 6, plot=pflo)
save_plot(filename = 'figs/Figure_germination_distribution.pdf',base_height = 6,base_width = 6, plot=pger)
