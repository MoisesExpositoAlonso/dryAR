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


## PLots of fitnes

ggplot(field.s) +
  geom_histogram(aes(x=Fruits,group=water,fill=water),alpha=0.8) +
  scale_fill_manual(values = watercolors())+
  facet_grid(site~indpop)+
  theme_bw() +
  xlab('# Fruits') +
  ylab('# Genotypes') +
  scale_alpha(guide = 'none') -> pf
pf

ggplot(field.s) +
  geom_histogram(aes(x=Survival_fruit,group=water,fill=water),alpha=0.8,bins=8) +
  scale_fill_manual(values = watercolors())+
  facet_grid(site~indpop)+
  theme_bw() +
  xlab('Survival fraction') +
  ylab('# Genotypes') +
  scale_alpha(guide = 'none') -> ps
ps
save_plot(filename = 'figs/Figure_fruit_distribution.pdf',base_height = 6,base_width = 6, plot=pf)
save_plot(filename = 'figs/Figure_survival_distribution.pdf',base_height = 6,base_width = 6, plot=ps)

