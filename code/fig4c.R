rm(list = ls())

#Load required packages
library(pacman)
pacman::p_load("tidyverse", "ggpubr", "janitor", "readxl")

#Import experimental data from excel table as a tibble
dat <- readxl::read_excel("data/fig4c.xlsx") %>%
  janitor::clean_names() %>%
  dplyr::rename(ph = p_h)

#Create scatter plot
ph_activity_plot <- ggplot2::ggplot(dat, aes(x = ph, y = percent_activity)) + 
  geom_point() +
  geom_errorbar(aes(ymin=percent_activity-st_dev, ymax=percent_activity+st_dev), width=.1,
                position=position_dodge(.9)) +
  theme_pubr() +
  labs(x = "pH", y = "% Activity") +
  scale_y_continuous(breaks = seq(0, 100, 20), limits = c(0, 105)) +
  scale_x_continuous(breaks = seq(4, 7, 0.5)) +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 8))

#Save plot as .jpg file
ggplot2::ggsave("plots/fig4c.jpg", dpi = 300, device = "jpeg", units = "in", width = 3, height = 3.5)
  