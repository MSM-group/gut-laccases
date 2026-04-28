rm(list = ls())

# Load required packages
library(pacman)
pacman::p_load("readxl", "ggplot2", "ggpubr")

# Import experimental data from excel table as a tibble
data <- read_excel("data/fig4a.xlsx") %>%
  dplyr::mutate(Time = as.factor(Time) %>%
                  forcats::fct_relevel("60", "10"),
                enzyme = as.factor(Enzyme) %>%
                  forcats::fct_relevel("Akk1", "Akk2", "Ent1", "Ent2", "Kur", "Lac", "Vei2_Δ101", "Vei1", "Vei1_Δ120"))

# Create bar plot
plot <- ggplot(data, aes(x = Enzyme, y = Abs, fill = Time)) +
  geom_col(position = position_dodge(width = 0.9),
           color = "black") +
  geom_errorbar(aes(ymin = Abs, ymax = Abs + Error),
                position = position_dodge(width = 0.9),
                width = 0.25) +
  labs(x = "Enzyme",
       y = "Absorbance 420 nm",
       fill = "Time [min]") +
  theme_pubr() +
  scale_fill_discrete(palette = c("black", "grey")) +
  theme(text = element_text(size = 8),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8))

#Export bar plot as .jpg file
ggplot2::ggsave("plots/fig4a.jpg", device = "jpeg", units = "in", height = 3.5, width = 3)