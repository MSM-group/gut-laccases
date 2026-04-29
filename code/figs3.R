rm(list = ls())

#Load required packages
library(pacman)
pacman::p_load("tidyverse", "ggpubr")

#Import metadata as tibble
metadat <- readr::read_csv("data/metadata_independent_samples.csv")

#Import rpoB abundance data as tibble
dat <- metadat %>%
  dplyr::left_join(readr::read_csv("data/figs3.csv"), by = c("ID" = "sample_id")) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(rpoB_abundance_plus_one = rpoB_abundance + 1)

#Create plot
ggplot2::ggplot(data = dat, aes(x = URCA, y = rpoB_abundance_plus_one)) +
  ggplot2::geom_point(alpha = 0.3) +
  ggplot2::geom_smooth(method = "lm", se = TRUE, color = "black") +
  ggpmisc::stat_poly_eq(use_label(c("adj.R2", "p"))) +
  ggpubr::theme_pubr() +
  ggplot2::labs(x = "URCA", y = "Abundance of contigs with rpoB [RPM] + 1") +
  ggplot2::scale_y_log10() +
  ggplot2::theme(text = element_text(size = 8))

#Save plot as .jpg file
ggplot2::ggsave("plots/figs3.jpg", dpi = 300, device = "jpeg", units = "in", width = 7, height = 4.5)