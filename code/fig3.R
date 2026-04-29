rm(list = ls())

#Load required packages
library(pacman)
pacman::p_load("ape", "tidyverse", "ggtree", "janitor", "ggnewscale", "rentrez", "scales", "phylosignal", "phylobase", "xml2", "ursa")

#Import phylogenetic tree of laccases sequences
tree_in <- treeio::read.newick("data/fig3.nwk")
tree_plot_1 <- ggtree::ggtree(tree_in, layout = "rectangular")

#Import laccase and sample metadata
metadat_laccases <- readr::read_csv("data/fig3.csv")
metadat_samples <- readr::read_csv("data/metadata_samples.csv")
metadat_1 <- metadat_laccases %>%
  dplyr::left_join(metadat_samples, by = c("sample" = "ID"))
                   
#Retrieve family level NCBI taxonomy classfication
uids <- metadat_1$ncbi_taxonomy %>%
  unique()
upload <- entrez_post(db = "taxonomy", id = uids)
xml_content <- entrez_fetch(
  db = "taxonomy", 
  web_history = upload, 
  rettype = "xml"
)
doc <- read_xml(xml_content)
nodes <- xml_find_all(doc, "/TaxaSet/Taxon") 
family_assignments <- lapply(nodes, function(node) {
  uid <- xml_text(xml_find_first(node, "./TaxId"))
  current_name <- xml_text(xml_find_first(node, "./ScientificName"))
  current_rank <- xml_text(xml_find_first(node, "./Rank"))
  if (!is.na(current_rank) && current_rank == "family") {
    family <- current_name
  } else {
    family_node <- xml_find_first(node, "./LineageEx/Taxon[Rank='family']/ScientificName")
    family <- xml_text(family_node)
  }
  
  tibble(
    ncbi_taxonomy = uid,
    scientific_name = current_name,
    lowest_rank = current_rank,
    family = family
  )
}) %>%
  bind_rows() %>%
  distinct()

#Get mean URCA of samples with laccases and assemble plot metadata
node_link <- tibble(label = tree_plot_1@data$label, node = tree_plot_1@data$node)
metadat_2 <- metadat_1 %>%
  dplyr::group_by(duplicate_of) %>%
  dplyr::summarise(mean_URCA = mean(URCA, na.rm = TRUE)) %>%
  dplyr::left_join(node_link, by = c("duplicate_of" = "label")) %>%
  dplyr::left_join(dplyr::select(metadat_laccases, duplicate_of, ncbi_taxonomy), by = "duplicate_of") %>%
  dplyr::left_join(dplyr::mutate(family_assignments, ncbi_taxonomy = as.double(ncbi_taxonomy)), by = "ncbi_taxonomy")
metadat_3 <- tibble(duplicate_of = tree_plot_1 %>%
                    dplyr::filter(isTip == TRUE) %>%
                    pull(label)) %>%
  dplyr::left_join(metadat_2, by = "duplicate_of")

#Create plot
tree_plot_2 <- tree_plot_1  %<+% metadat_3 +
  ggtree::theme_tree() 
treetmap_dat <- data.frame(mean_URCA = dplyr::filter(tree_plot_2@data, isTip == TRUE) %>%
                             dplyr::mutate(mean_URCA = dplyr::case_when(is.nan(mean_URCA) ~ 28,
                                                                   TRUE ~ mean_URCA))%>%
                             pull(mean_URCA))
rownames(treetmap_dat) <- dplyr::filter(tree_plot_2@data, isTip == TRUE) %>%
  pull(label)
set.seed(67)
cubehelix_palette <- c(cubehelix(na.omit(metadat_3$family) %>%
                                 unique() %>%
                                 length(), gamma = 1.5, dark = 100, light = 220), "grey90", "black")
format_labels <- function(x) {
  lapply(x, function(l) {
    if (l %in% c("No family assignment", "Reference laccase")) {
      l
    } else {
      bquote(italic(.(l)))
    }
  })
}
tree_plot_2@data <- tree_plot_2@data %>%
  dplyr::mutate(family = dplyr::case_when(is.na(family) & !is.na(ncbi_taxonomy) & isTip == TRUE ~ "No family assignment",
                                          is.na(family) & is.na(ncbi_taxonomy) & isTip == TRUE ~ "Reference laccase",
                                          TRUE ~ family)) %>%
  dplyr::arrange(family) %>%
  dplyr::mutate(family = family %>%
                  as_factor() %>%
                  fct_relevel("No family assignment", after = Inf) %>%
                  fct_relevel("Reference laccase", after = Inf),
                tested = case_when(label %in% c("UrtQLP2E9X6p397RBN4r39_2", "NnqfqxssYuBZDZgVVf4AGr_9", "jj54KTrq2tPoS3oB95CWpb_3", "BU5axyDn7aeu7yGoESJCGb_10", "PvSv2mwhn8efmYDPquH3h3_2", "mMCO", "aMCO", "MCO1") ~ "Yes",
                                   TRUE ~ "No"))
tree_plot_2 <- tree_plot_2 + scale_color_discrete(palette = cubehelix_palette, labels = format_labels) +
  ggtree::geom_tippoint(aes(x = x, y = y, color = family, shape = tested)) +
  new_scale_fill() +
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) +
  labs(color = "Kraken2 family assignment", shape = "Experimentally validated")
tree_plot_3 <- ggtree::gheatmap(tree_plot_2, treetmap_dat, width = 0.1, colnames = FALSE) +
  scale_fill_continuous(name = "Mean URCA", palette = pal_viridis(option = "viridis"), na.value = "grey90", limits = c(1, 30))

#Save plot as .jpg file
ggplot2::ggsave("plots/fig3.jpg", tree_plot_3, device = "jpeg", units = "in", width = 7, height = 7, dpi = 300)

#Compute Moran's I and Blomberg's K
valid_treetmap_dat <- treetmap_dat[!is.na(treetmap_dat$mean_URCA), , drop = FALSE]
valid_tips <- rownames(valid_treetmap_dat)
phy_tree <- ape::as.phylo(tree_in)
pruned_tree <- ape::keep.tip(phy_tree, valid_tips)
if (!ape::is.binary(pruned_tree)) {
  pruned_tree <- ape::multi2di(pruned_tree)
}
p4d_urca <- phylobase::phylo4d(pruned_tree, tip.data = valid_treetmap_dat)
phylo_sig_results <- phylosignal::phyloSignal(
  p4d = p4d_urca,
  methods = c("I", "K") 
)

