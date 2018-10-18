rm(list = ls())
library(magrittr)
source("scripts/misc/helpers.R")
study <- "jansson_twins_ibd"
template <- readr::read_csv("data/template.csv")
dir.create(paste0("processed/", study, "/metadata/"),
           recursive = TRUE,
           showWarnings = FALSE)

meta_raw <- paste0("raw/", 
                   study, 
                   "/metadata/1070_20170412-111709.txt") %>% 
  readr::read_tsv() # downloaded from Qiita

meta_curated <- meta_raw %>% 
  dplyr::mutate(
    dataset_name = study %>% as.character(),
    study_accession = study %>% as.character(),
    PMID = "20816835",
    subject_accession = host_subject_id %>% as.character(),
    alternative_subject_accession = NA_character_,
    sample_accession = sample_name %>% as.character(),
    alternative_sample_accession = anonymized_name %>% as.character(),
    batch = NA_character_,
    sample_accession_16S = sample_name %>% 
      gsub(".", "_", ., fixed = TRUE),
    sample_accession_WGS = NA_character_,
    sample_type = body_site %>% 
      dplyr::recode("UBERON:feces" = "stool",
             "UBERON:colonic mucosa" = "biopsy",
             "UBERON:ileal mucosa" = "biopsy"),
    body_site = body_site %>% 
      dplyr::recode("UBERON:feces" = "stool",
             "UBERON:colonic mucosa" = "colon",
             "UBERON:ileal mucosa" = "ileum"),
    disease = disease_stat2 %>% 
      dplyr::recode("colonic Crohn's disease" = "CD",
             "Ileal and colonic Crohn's disease" = "CD",
             "ileal Crohn's disease" = "CD",
             "ulcerative colitis-LS" = "UC",
             "Healthy" = "control"),
    control = disease_stat2 %>% 
      dplyr::recode("colonic Crohn's disease" = NA_character_,
                    "Ileal and colonic Crohn's disease" = NA_character_,
                    "ileal Crohn's disease" = NA_character_,
                    "ulcerative colitis-LS" = NA_character_,
                    "Healthy" = "HC"),
    IBD_subtype = disease_stat2 %>% 
      dplyr::recode("colonic Crohn's disease" = "cCD",
             "Ileal and colonic Crohn's disease" = "cCD",
             "ileal Crohn's disease" = "iCD",
             "ulcerative colitis-LS" = "UC",
             "Healthy" = NA_character_),
    IBD_subtype_additional = NA_character_,
    L.cat = NA_character_,
    E.cat = NA_character_,
    B.cat = NA_character_,
    perianal = NA_character_,
    age = age %>% as.numeric,
    age_c = NA_character_,
    age_at_diagnosis = NA_real_,
    age_at_diagnosis_c = NA_character_,
    gender = sex %>% 
      dplyr::recode("male" = "m",
             "female" = "f"),
    BMI = NA_real_,
    alcohol = NA_character_,
    smoke = NA_character_,
    site = geo_loc_name %>% as.character(),
    calprotectin = NA_real_,
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
    time_point = NA_character_,
    time_point_supp = NA_character_,
    family = pair_number %>% as.character(),
    family_supp = "Family indicates twin pairs",
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