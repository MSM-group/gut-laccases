rm(list = ls())

#Load required packages
library(pacman)
pacman::p_load("tidyverse", "ggpubr")

#Import MAG quality data
busco_results <- readr::read_tsv("data/figs2.tsv") %>%
  dplyr::mutate(quality = dplyr::case_when(completeness > 90 & contamination < 5 ~ "high",
                                           completeness >= 50 & contamination < 10 ~ "medium",
                                           TRUE ~ "low"))

#Summarize MAG quality
summary_stats <- busco_results %>%
  dplyr::group_by(quality) %>%
  dplyr::summarize(count = n())

#Create completeness/contamination plot
mags_quality_plot <- ggplot2::ggplot(busco_results, aes(x = completeness, y = contamination)) +
  geom_point(alpha = 0.3) +
  ggpubr::theme_pubr() +
  ggplot2::theme(axis.text = element_text(size = 8),
                 axis.title = element_text(size = 8)) +
  ggplot2::labs(x = "Completeness", y = "Contamination")

#Save plot as .jpg file
ggplot2::ggsave("plots/figs2.jpg", dpi = 300, device = "jpeg", units = "in", width = 4.5, height = 4.5)
