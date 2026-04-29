rm(list = ls())

#Load required packages
library(pacman)
pacman::p_load("tidyverse", "ggpubr", "readxl")

#Import annotated bacdive results as tibble
dat <- readxl::read_excel("data/figs4.xlsx") %>%
  dplyr::mutate(scientific_name = stringr::str_remove(scientific_name, "哄")) %>%
  dplyr::group_by(scientific_name, category) %>%
  dplyr::summarise(count = n(), .groups = "drop") %>%
  dplyr::group_by(scientific_name) %>%
  dplyr::mutate(fraction = count / sum(count)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(scientific_name = as_factor(scientific_name) %>%
                  fct_rev())

#Create plot
plot <- ggplot2::ggplot(dat, aes(x = scientific_name, y = count, fill = category)) +
  ggplot2::geom_bar(position="fill", stat="identity") +
  ggpubr::theme_pubr()  +
  ggplot2::labs(x = "Species", y = "% of strains", fill = "Strain isolation source") +
  ggplot2::scale_y_continuous(labels = percent) +
  ggplot2::coord_flip() +
  ggplot2::scale_fill_brewer(palette = "Set1") +
  ggplot2::theme(axis.text.y = element_text(size = 8, face = "italic"),
                 legend.text = element_text(size = 8),
                 legend.title = element_blank(),
                 axis.title = element_text(size = 8),
                 axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
                 legend.margin = margin(l = -150, unit = "pt"))

#Save plot as .jpg
ggplot2::ggsave("plots/figs4.jpg", plot, device = "jpeg", units = "in", width = 4.5, height = 4.5, dpi = 300)