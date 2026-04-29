rm(list = ls())

#Load required packages
library(pacman)
pacman::p_load("tidyverse", "janitor", "ggpubr", "ape", "phyloseq", "picante", "treeio")

#Import metadata as tibble
metadat <- readr::read_csv("data/metadata_independent_samples.csv")

#Import abundance data of laccases with phylogeny as tibble
dat <- readr::read_csv("data/fig2c.csv")

#Import tree and remove reference laccases
tree_in <- treeio::read.newick("data/fig3.nwk")
drop_tips <- tree_in$tip.label[!(tree_in$tip.label %in% dat$duplicate_of)]
tree <- ape::drop.tip(tree_in, drop_tips)

#Format data for UniFrac calculation
otu_mat <- dat %>%
  tibble::column_to_rownames("duplicate_of") %>%
  as.matrix()
sample_df <- metadat %>%
  dplyr::filter(ID %in% colnames(dat)) %>%
  tibble::column_to_rownames("ID")
physeq <- phyloseq(
  otu_table(otu_mat, taxa_are_rows = TRUE),
  sample_data(sample_df),
  phy_tree(tree)
)

#Caulculate unweighted UniFrac distances and perform PCoA
table_pd <- t(as(otu_table(physeq), "matrix"))
unifrac_unweighted <- picante::unifrac(table_pd, tree)
pcoa_unifrac_unweighted <- ape::pcoa(unifrac_unweighted)

#Extract PCoA coordinates for plot
pcoa_df_unifrac_unweighted <- pcoa_unifrac_unweighted$vectors %>%
  as.data.frame() %>%
  tibble::rownames_to_column("sample") %>%
  dplyr::rename(PCoA1 = Axis.1, PCoA2 = Axis.2) %>%
  dplyr::left_join(metadat, by = c("sample" = "ID"))

#Create plot
var_exp <- round(100 * pcoa_unifrac_unweighted$values$Relative_eig[1:2], 1)
plot <- ggplot(pcoa_df_unifrac_unweighted, aes(x = PCoA1, y = PCoA2, color = URCA)) +
  geom_point() +
  geom_text(
    label = "c)",
    x = -Inf, y = Inf,
    hjust = 2.5,
    vjust = 1,
    fontface = "bold",
    size = 14 / .pt,
    color = "black",
    inherit.aes = FALSE
  ) +
  ggpubr::theme_pubr() +
  labs(
    x = paste0("Axis 1 (", as.character(var_exp[1]), "%)"), 
    y = paste0("Axis 2 (", as.character(var_exp[2]), "%)")
  ) +
  scale_color_viridis_c() +
  theme(
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 8)
  ) +
  coord_cartesian(clip = "off")

ggplot2::ggsave("plots/fig2c.jpg", plot, dpi = 300, device = "jpeg", units = "in", width = 2.3, height = 4)