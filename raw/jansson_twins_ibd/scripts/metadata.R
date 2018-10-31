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
    study_accession = NA_character_,
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
    body_site_additional = NA_character_,
    disease = disease_stat %>% 
      dplyr::recode("colonic Crohn's disease" = "CD",
                    "ileal and colonic Crohn's disease with ulcerative colitis" = "CD",
                    "healthy" = "control",
                    "Icolonic Crohn's disease" = "CD",
                    "ileal Crohn's disease" = "CD",
                    "ileal Crohn's disease with ulcerative colitis" = "CD",
                    "ulcerative colitis-PROC" = "UC",
                    "ulcerative colitis-LS" = "UC",
                    "ulcerative colitis-EXT" = "UC",
                    "ulcerative colitis" = "UC",
                    .missing = NA_character_),
    control = disease_stat %>% 
      dplyr::recode("healthy" = "HC",
                    .default = NA_character_,
                    .missing = NA_character_),
    IBD_subtype = disease_stat %>% 
      dplyr::recode("colonic Crohn's disease" = "cCD",
                    "ileal and colonic Crohn's disease with ulcerative colitis" = "cCD",
                    "healthy" = NA_character_,
                    "Icolonic Crohn's disease" = "cCD",
                    "ileal Crohn's disease" = "iCD",
                    "ileal Crohn's disease with ulcerative colitis" = "iCD",
                    "ulcerative colitis-PROC" = "UC",
                    "ulcerative colitis-LS" = "UC",
                    "ulcerative colitis-EXT" = "UC",
                    "ulcerative colitis" = "UC",
                    .missing = NA_character_),
    IBD_subtype_additional = NA_character_,
    L.cat = disease_stat %>% 
      dplyr::recode("colonic Crohn's disease" = "L2",
                    "ileal and colonic Crohn's disease with ulcerative colitis" = "L3",
                    "healthy" = NA_character_,
                    "Icolonic Crohn's disease" = "L2",
                    "ileal Crohn's disease" = "L1",
                    "ileal Crohn's disease with ulcerative colitis" = "L1",
                    "ulcerative colitis-PROC" = NA_character_,
                    "ulcerative colitis-LS" = NA_character_,
                    "ulcerative colitis-EXT" = NA_character_,
                    "ulcerative colitis" = NA_character_,
                    .missing = NA_character_),
    E.cat = disease_stat %>% 
      dplyr::recode("colonic Crohn's disease" = NA_character_,
                    "ileal and colonic Crohn's disease with ulcerative colitis" = NA_character_,
                    "healthy" = NA_character_,
                    "Icolonic Crohn's disease" = NA_character_,
                    "ileal Crohn's disease" = NA_character_,
                    "ileal Crohn's disease with ulcerative colitis" = NA_character_,
                    "ulcerative colitis-PROC" = "E1",
                    "ulcerative colitis-LS" = "E2",
                    "ulcerative colitis-EXT" = "E3",
                    "ulcerative colitis" = NA_character_,
                    .missing = NA_character_),
    B.cat = NA_character_,
    perianal = NA_character_,
    age = age %>% as.numeric,
    age_at_diagnosis = NA_real_,
    age_at_diagnosis.cat = NA_character_,
    race = NA_character_,
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
    mesalamine_5ASA = NA_character_,
    mesalamine_5ASA_supp = NA_character_,
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
