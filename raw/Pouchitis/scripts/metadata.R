rm(list = ls())
library(magrittr)
source("scripts/misc/helpers.R")
study <- "Pouchitis"
template <- readr::read_csv("data/template.csv")
dir.create(paste0("processed/", study, "/metadata/"),
           recursive = TRUE,
           showWarnings = FALSE)

meta1 <- readr::read_tsv("data/metadata_raivo/sample2project_common.txt") %>% 
  dplyr::filter(Project == study, Technology == "16S") %>% 
  dplyr::select(GID, Project, DonorID, OriginalID, SequencingRun)
meta2 <- paste0("raw/", study, 
                "/metadata/Sample List Broad_pheno7_6_2012.xlsx") %>% 
  readxl::read_excel()
meta_raw <- meta1 %>% 
  dplyr::left_join(meta2, by = c("OriginalID" = "Sample ID"))

meta_curated <- meta_raw %>% 
  dplyr::mutate(
    dataset_name = "Pouchitis",
    study_accession = NA_character_,
    PMID = "25887922",
    subject_accession = paste0(Family_ID, ":", Member_ID) %>% as.character(),
    alternative_subject_accession = NA_character_,
    sample_accession = OriginalID %>% as.character(),
    alternative_sample_accession = NA_character_,
    batch = SequencingRun %>% as.character(),
    sample_accession_16S = GID %>% as.character(),
    sample_accession_WGS = NA_character_,
    sample_type = `Sample Location` %>% 
      dplyr::recode("Pouch" = "biopsy",
                    "PPI" = "biopsy",
                    "Sigmoid" = "biopsy",
                    "TI" = "biopsy"),
    body_site = `Sample Location` %>% 
      dplyr::recode("Pouch" = "pouch",
                    "PPI" = "PPI",
                    "Sigmoid" = "sigmoid",
                    "TI" = "TI"),
    body_site_additional = NA_character_,
    disease = Diagnosis %>% # These mappings are generated based on Dirk's metadata sheet
      dplyr::recode("CD" = "CD",
                    "UC" = "UC",
                    "IC" = "IC",
                    "MC" = "IC",
                    "unconfirmed IBD" = NA_character_,
                    "FAP" = "FAP",
                    "FAP/cdiff" = "FAP",
                    "HC" = "control",
                    "Unaffected" = "control",
                    .missing = NA_character_),
    control = disease %>% 
      dplyr::recode("CD" = NA_character_,
                    "UC" = NA_character_,
                    "IC" = NA_character_,
                    "FAP" = NA_character_,
                    "control" = "HC",
                    .missing = NA_character_),
    IBD_subtype = NA_character_,
    IBD_subtype_additional = NA_character_,
    L.cat = `Montreal Disease location` %>% 
      dplyr::recode("L1" = "L1",
                    "L2" = "L2",
                    "L3" = "L3",
                    .default = NA_character_,
                    .missing = NA_character_),
    E.cat = `Montreal Disease location` %>% 
      dplyr::recode("E1" = "E1",
                    "E2" = "E2",
                    "E3" = "E3",
                    .default = NA_character_,
                    .missing = NA_character_),
    B.cat = Behaviour %>% 
      dplyr::recode("1.0" = "B1",
                    "2.0" = "B2",
                    "3.0" = "B3",
                    .default = NA_character_,
                    .missing = NA_character_) %>% 
      ifelse(disease == "CD", ., NA_character_),
    perianal = NA_character_,
    age = Age %>% as.numeric(),
    age_at_diagnosis = Ageof_Diag %>% 
      dplyr::recode("UK" = NA_character_) %>% 
      as.numeric(),
    age_at_diagnosis.cat = NA_character_,
    race = NA_character_,
    gender = Gender %>% 
      dplyr::recode("Male" = "m",
                    "Female" = "f"),
    BMI = BMI %>% 
      dplyr::recode("UK" = NA_character_) %>% 
      as.numeric(),
    alcohol = NA_character_,
    smoke = Smoking %>% 
      dplyr::recode("Current Smoker" = "current",
                    "Ex-Smoker" = "former",
                    "Never Smoked" = "never",
                    "UK" = NA_character_,
                    .missing = NA_character_),
    site = `Collection Centre` %>% as.character(),
    calprotectin = NA_real_,
    PCDAI = NA_real_,
    antibiotics = Antibiotics %>% 
      dplyr::recode("1.0" = "y",
                    "0.0" = "n",
                    "UK" = NA_character_,
                    .missing = NA_character_),
    antibiotics_supp = NA_character_,
    immunosuppressants = Immunosuppression %>% 
      dplyr::recode("1.0" = "y",
                    "0.0" = "n",
                    "UK" = NA_character_,
                    .missing = NA_character_),
    immunosuppressants_supp = NA_character_,
    steroids = Steroids %>% 
      dplyr::recode("1.0" = "y",
                    "0.0" = "n",
                    "UK" = NA_character_,
                    .missing = NA_character_),
    steroids_supp = NA_character_,
    mesalamine_5ASA = Mesalamine %>% 
      dplyr::recode("1.0" = "y",
                    "0.0" = "n",
                    "UK" = NA_character_,
                    .missing = NA_character_),
    mesalamine_5ASA_supp = NA_character_,
    biologics = NA_character_,
    biologics_supp = NA_character_,
    time_point = NA_character_,
    time_point_supp = NA_character_,
    family = Family_ID,
    family_supp = "Family ID",
    extraction_kit_16S = NA_character_,
    sequencing_platform_16S = NA_character_,
    number_reads_16S = NA_integer_,
    number_bases_16S = NA_integer_,
    minimum_read_length_16S = NA_integer_,
    median_read_length_16S = NA_integer_
  ) %>% dplyr::select(template$col.name %>% dplyr::one_of())

meta_curated <- meta_curated[, template$col.name]
test <- check_subject(meta_curated, "age_at_diagnosis")
meta_curated %>% dplyr::filter(subject_accession %in% test$subject_accession[test$n_cat > 1]) %>% View
if(check.template(meta_curated, template)) {
  cat("Metadata for", study,"processed and check successful!\n")
  write.table(meta_curated,
              file = paste0("processed/", study, "/metadata/metadata.txt"),
              quote = F,
              sep = "\t",
              row.names = F)
}
