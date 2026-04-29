rm(list = ls())

#Load required packages
library(pacman)
pacman::p_load("tidyverse", "janitor", "ggpubr", "ggpmisc")

#Import metadata for independent samples from csv to tibble
metadat <- readr::read_csv("data/metadata_independent_samples.csv")

#Import laccase abundance data as tibble
dat_A <-  metadat %>%
  dplyr::left_join(readr::read_csv("data/fig2a.csv"), by = c("ID" = "sample_id")) %>%
  tidyr::replace_na(list(laccase_abundance = 0)) %>%
  dplyr::mutate(laccase_abundance_plus_one = laccase_abundance + 1) %>%
  dplyr::arrange(Paper)

#Import laccase Faith's phylogenetic distance data as tibble
dat_B <- readr::read_csv("data/fig2b.csv")

#Merge abundance and Faith's PD data for plotting
dat <- dat_A %>%
  dplyr::left_join(dat_B, by = c("ID" = "sample")) %>%
  tidyr::pivot_longer(cols = laccase_abundance_plus_one:PD, names_to = "metric", values_to = "value") %>%
  tidyr::drop_na()

#Create plots
plot_labels <- data.frame(metric = c("laccase_abundance_plus_one", "PD"),
                          label = c("A", "B"))
ggplot(dat, aes(x = URCA, y = value)) +
  geom_point(aes(color = Paper), alpha = 0.3) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    inherit.aes = FALSE,
    aes(x = URCA, y = value),
    color = "black"
  ) +
  geom_text(data = plot_labels,
            aes(label = label),
            x = -Inf,
            y = Inf,
            hjust = 2.5,
            vjust = 1,
            fontface = "bold",
            size = 14/.pt,
            inherit.aes = FALSE) +
  ggpmisc::stat_poly_eq(use_label(c("adj.R2", "p")), size = 8 / .pt) +
  facet_wrap(vars(metric),
             scales = "free_y",
             strip.position = "left",
             labeller = as_labeller(c(laccase_abundance_plus_one = "Laccase contig abundance [RPM] + 1", PD = "Faith's PD"))) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Urbanized  <----  URCA  ---->  Remote", y = NULL) +
  ggpubr::theme_pubr() + 
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        strip.text.y = element_text(angle = 90, size = 8, color = "black"),
        axis.title.y = element_blank(),
        axis.line = element_line(color = "black"),
        legend.position = "top",
        legend.margin = margin(l = -30, unit = "pt"),
        legend.text = element_text(size = 8),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 8),
        axis.text = element_text(size = 8)) +
  scale_y_log10() +
  coord_cartesian(clip = "off") +
  guides(color = guide_legend(override.aes = list(alpha = 1)))

#Save plot as .jpg file
ggplot2::ggsave("plots/fig2ab.jpg", dpi = 300, device = "jpeg", units = "in", width = 4.7, height = 4)