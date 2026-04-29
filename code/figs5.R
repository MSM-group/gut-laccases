rm(list = ls())

#Load required packages
library(tidyverse)
library(ggpubr)

#Import taxonomy data of MAGs with laccase homologs as tibble
dat <- readr::read_csv("data/figs5.csv")

#Create plot
species_count_barplot <- ggplot2::ggplot(dat, aes(assigned_name)) +
  geom_bar() +
  ggpubr::theme_pubr() +
  ggplot2::theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, face = "italic", size = 8),
                 axis.title.y = element_text(size = 8),
                 axis.text.y = element_text(size = 8),
                 axis.title.x = element_text(size = 8)) +
  ggplot2::labs(x = "Species", y = "# of MAGs with laccases")

#Save plot as .jpg file
ggplot2::ggsave(species_count_barplot, file = "plots/figs5.jpg", device = "jpeg",dpi = 300, units = "in", width = 4, height = 5)