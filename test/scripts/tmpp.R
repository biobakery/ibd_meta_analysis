rm(list = ls())
library(tidyverse)
library(phyloseq)
l_biom <- list()
for(study in c("BIDMC-FMT",
               "CS-PRISM",
               "LSS-PRISM",
               "MucosalIBD",
               # "STiNKi",
               "Pouchitis",
               "RISK")) {
  biom_tmp <- import_biom(
    paste0("processed/", study, 
           "/16s/all_samples_taxonomy_closed_reference.biom"))
  assign(study, biom_tmp)
  l_biom[[study]] <- biom_tmp
}
df_meta <- read_tsv("raw/tmp/metadata_raivo/sample2project_common.txt")
load("../IBD_structure/data/clean/phylo_all_genus_with_tree.RData")
df_meta_Pouchitis <- sample_data(phylo_all_genus)
class(df_meta_Pouchitis) <- "data.frame"
df_meta_Pouchitis2 <- df_meta_Pouchitis %>% 
  subset(collection == "MSH") %>% 
  rownames_to_column("sampleID") %>% 
  as.tibble
df_meta_Pouchitis1 <- df_meta %>% 
  subset(Project == "Pouchitis")
df_meta_Pouchitis_combined <- df_meta_Pouchitis1 %>% 
  left_join(df_meta_Pouchitis2, by = c("OriginalID" = "DirkSampleID"))
df_meta_Pouchitis_combined <- df_meta_Pouchitis_combined %>% 
  mutate(Diagnosis = Diagnosis.y,
         Location = biopsy_location %>% 
           recode("Terminalileum" = "Terminal Ileum"),
         Gender = Gender.x)
df_meta_Pouchitis_include <- df_meta_Pouchitis_combined %>% 
  filter(Location == "Terminal Ileum")
df_meta_Pouchitis_include <- df_meta_Pouchitis_include[, colnames(df_meta)]

df_meta_use <- df_meta %>%
  mutate(Location = 
           Location %>% 
           recode("L1" = "Terminal Ileum",
                  "L2" = "Terminal Ileum",
                  "L3" = "Terminal Ileum")) %>% 
  filter(!is.na(Diagnosis),
         !is.na(Location),
         Location %in% c("Terminal Ileum", "Rectum", "Stool")) %>% 
  rbind(df_meta_Pouchitis_include)

taxa_common <- l_biom %>% 
  lapply(taxa_names) %>% 
  Reduce("intersect", .)

l_mat_otu <- list()
l_df_meta <- list()
for(study in c("BIDMC-FMT",
               "CS-PRISM",
               "LSS-PRISM",
               # "STiNKi",
               "Pouchitis",
               "RISK")) {
  biom_tmp <- l_biom[[study]]
  mat_otu_tmp <- otu_table(biom_tmp)@.Data
  samples_common <- intersect(colnames(mat_otu_tmp), df_meta_use$GID)
  mat_otu_tmp <- mat_otu_tmp[, samples_common]
  mat_otu_tmp <- mat_otu_tmp %>% apply(2, function(x) x / sum(x))
  mat_otu_tmp <- mat_otu_tmp[taxa_common, ]
  l_mat_otu[[study]] <- mat_otu_tmp
  df_meta_tmp <- df_meta_use %>% 
    filter(GID %in% samples_common)
  df_meta_tmp <- df_meta_tmp %>% 
    as.data.frame() %>% 
    column_to_rownames("GID")
  df_meta_tmp <- df_meta_tmp[samples_common, ]
  l_df_meta[[study]] <- df_meta_tmp
}
mat_otu <- Reduce("cbind", l_mat_otu)
mat_otu[is.na(mat_otu)] <- 0
zeros <- (mat_otu %>% apply(2, sum)) != 0
df_meta_all <- Reduce("rbind", l_df_meta)
mat_otu <- mat_otu[, zeros]
df_meta_all <- df_meta_all[zeros, ]
df_meta_all$Diagnosis <- df_meta_all$Diagnosis %>% recode("control" = "Control",
                                                          "IC" = "CD")
  
biom_all <- phyloseq(otu_table(mat_otu, taxa_are_rows = T),
                     sample_data(df_meta_all))

ordination <- ordinate(biom_all, method = "MDS", distance = "bray")
plot_ordination(biom_all, ordination, color = "Project")
plot_ordination(biom_all, ordination, color = "Location")
plot_ordination(biom_all, ordination, color = "Diagnosis")

