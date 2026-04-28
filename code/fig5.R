rm(list = ls())

#Load required packages
library(pacman)
pacman::p_load("tidyverse", "readxl", "ggpubr", "RColorBrewer")

#Import experimental data from excel table as a tibble
dat_in <- readxl::read_excel("data/fig5.xlsx") %>%
  janitor::clean_names()

#Clean data for plotting
dat <- dat_in %>%
  tidyr::pivot_longer(-compound, names_to = "condition", values_to = "value") %>%
  dplyr::mutate(value_type = dplyr::case_when(stringr::str_detect(condition, "stdev") ~ "stddev",
                                              TRUE ~ "mean"),
                laccase = dplyr::case_when(stringr::str_detect(condition, "mco1")|stringr::str_detect(condition, "positive_control") ~ "MCO1",
                                           stringr::str_detect(condition, "leaf")|stringr::str_detect(condition, "hyv") ~ "mMCO",
                                           stringr::str_detect(condition, "soil")|stringr::str_detect(condition, "hey") ~ "aMCO",
                                           stringr::str_detect(condition, "gut")|stringr::str_detect(condition, "vei") ~ "Vei1",
                                           TRUE ~ "Abiotic control"),
                abts = dplyr::case_when(stringr::str_detect(condition, "w_o") ~ "w/o ABTS",
                                        TRUE ~ "w/ ABTS"),
                conditions = stringr::str_c(laccase, abts, sep = " ")) %>%
  tidyr::pivot_wider(names_from = value_type, values_from = value, id_cols = c(compound, conditions)) %>%
  dplyr::mutate(conditions = as_factor(conditions) %>%
                  fct_relevel(c("MCO1 w/ ABTS", "MCO1 w/o ABTS", "aMCO w/ ABTS", "aMCO w/o ABTS", "mMCO w/ ABTS", "mMCO w/o ABTS", "Vei1 w/ ABTS", "Vei1 w/o ABTS", "Abiotic control w/ ABTS")),
                compound = as_factor(compound) %>%
                  fct_relevel(c("Bisphenol AF", "Cyflumetofen", "Fluazinam")) %>%
                  fct_rev())

#Create color palette
pal <- c(brewer.pal(6, "Reds")[c(6,2)], brewer.pal(6, "Blues")[c(6,2)], brewer.pal(6, "Greens")[c(6,2)], brewer.pal(6, "Purples")[c(6,2)], "grey50")

#Create bar plot
laccase_biotransformation_plot <- ggplot2::ggplot(dat, aes(x = mean, y = compound, fill = conditions)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(xmin=mean-stddev, xmax=mean+stddev), width=.4,
                position=position_dodge(.9)) +
  theme_pubr() +
  scale_fill_discrete(palette = pal,
                      breaks = c("MCO1 w/ ABTS", "MCO1 w/o ABTS", "aMCO w/ ABTS", "aMCO w/o ABTS", "mMCO w/ ABTS", "mMCO w/o ABTS", "Vei1 w/ ABTS", "Vei1 w/o ABTS", "Abiotic control w/ ABTS")) +
  labs(y = "Compound", x = expression(C/C[0])) +
  theme(legend.text = element_text(size = 8, margin = margin(l = 1.5, unit = "pt")),
        legend.title = element_blank(),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position = c(0.72, 0.87),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA),
        legend.key.size = unit(0.5, "line"))

#Export bar plot as .jpg file
ggplot2::ggsave("plots/fig5.jpg", dpi = 300, device = "jpeg", units = "in", width = 3, height = 4.5)