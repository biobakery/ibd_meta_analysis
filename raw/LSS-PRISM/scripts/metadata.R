rm(list = ls())
library(magrittr)
source("scripts/misc/helpers.R")
study <- "LSS-PRISM"
template <- readr::read_csv("data/template.csv")
dir.create(paste0("processed/", study, "/metadata/"),
           recursive = TRUE,
           showWarnings = FALSE)

meta1 <- readr::read_tsv("data/metadata_raivo/sample2project_common.txt") %>% 
  dplyr::filter(Project == study, Technology == "16S") %>% 
  dplyr::select(GID, Project, DonorID, OriginalID, SequencingRun)
meta2 <- readxl::read_xlsx("raw/CS-PRISM/metadata/consolidatedWT_IBD_metadata.xlsx",
                           sheet = "Data")
meta3 <- readxl::read_xlsx("raw/CS-PRISM/metadata/consolidatedWT_IBD_metadata.xlsx",
                           sheet = "metadata")
meta_raw <- meta1 %>% 
  dplyr::left_join(meta2, by = c("GID" = "16S G#"))
meta3 %>% dplyr::filter(`16S G` %in% meta_raw$GID | `16S G2` %in% meta_raw$GID) %>% 
  extract2("Interval (Categorical)") %>% unique()
meta_raw <- meta1 %>% 
  dplyr::left_join(meta2) %>% 
  dplyr::left_join(meta3)

meta_curated <- meta_raw %>% 
  dplyr::mutate(
    dataset_name = "LSS-PRISM",
    study_accession = "LSS-PRISM",
    PMID = NA_character_,
    subject_accession = DonorID %>% as.character(),
    alternative_subject_accession = NA_character_,
    sample_accession = GID %>% as.character(),
    alternative_sample_accession = NA_character_,
    batch = SequencingRun %>% as.character(),
    sample_accession_16S = GID %>% as.character(),
    sample_accession_WGS = NA_character_,
    sample_type = Location %>% 
      dplyr::recode("Stool" = "stool",
                    .missing = NA_character_),
    body_site = Location %>% 
      dplyr::recode("Stool" = "stool",
                    .missing = NA_character_),
    disease = Diagnosis %>% 
      dplyr::recode("CD" = "CD",
                    "UC" = "UC",
                    "IC" = "IC",
                    .missing = NA_character_),
    control = NA_character_,
    IBD_subtype = NA_character_,
    IBD_subtype_additional = NA_character_,
    L.cat = NA_character_,
    E.cat = NA_character_,
    B.cat = NA_character_,
    perianal = NA_character_,
    age = Age %>% as.numeric(),
    age_c = NA_character_,
    age_at_diagnosis = NA_real_,
    age_at_diagnosis_c = NA_character_,
    gender = Gender %>% 
      dplyr::recode("Male" = "m",
                    "Female" = "f"),
    BMI = NA_real_,
    alcohol = NA_character_,
    smoke = SmokingStatus %>% 
      dplyr::recode("Former_smoker" = "former",
                    "Never_smoked" = "never"),
    site = "MSH",
    calprotectin = FecalCalprotectin %>% as.numeric(),
    PCDAI = NA_real_,
    antibiotics = NA_character_,
    antibiotics_supp = NA_character_,
    immunosuppressants = NA_character_,
    immunosuppressants_supp = NA_character_,
    steroids = NA_character_,
    steroids_supp = NA_character_,
    mesalamine = NA_character_,
    mesalamine_supp = NA_character_,
    biologics = NA_character_,
    biologics_supp = NA_character_,
    time_point = LSSMonth %>% as.character(),
    time_point_supp = "LSS Month",
    family = NA_character_,
    family_supp = NA_character_,
    extraction_kit_16S = NA_character_,
    sequencing_platform_16S = NA_character_,
    number_reads_16S = NA_integer_,
    number_bases_16S = NA_integer_,
    minimum_read_length_16S = NA_integer_,
    median_read_length_16S = NA_integer_
  ) %>% dplyr::select(template$col.name %>% dplyr::one_of())

meta_curated <- meta_curated[, template$col.name]
if(check.template(meta_curated, template)) {
  cat("Metadata for", study,"processed and check successful!\n")
  write.table(meta_curated,
              file = paste0("processed/", study, "/metadata/metadata.txt"),
              quote = F,
              sep = "\t",
              row.names = F)
}