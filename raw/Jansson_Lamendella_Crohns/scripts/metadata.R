rm(list = ls())
library(magrittr)
source("scripts/misc/helpers.R")
study <- "Jansson_Lamendella_Crohns"
template <- readr::read_csv("data/template.csv")
dir.create(paste0("processed/", study, "/metadata/"),
           recursive = TRUE,
           showWarnings = FALSE)

meta1 <- paste0("raw/", 
                study, 
                "/metadata/1629_20180101-113841.txt") %>% 
  readr::read_tsv() # downloaded from Qiita
meta2 <- paste0("raw/", 
                study, 
                "/metadata/ebi_sample_accessions_study_1629.tsv") %>% 
  readr::read_tsv() # downloaded from Qiita
meta3 <- paste0("raw/", 
                study, 
                "/metadata/PRJEB18471.txt") %>% 
  readr::read_tsv() # downloaded from EBI
meta_raw <- meta1 %>% 
  dplyr::left_join(meta2) %>% 
  dplyr::left_join(meta3, by = c("sample_accession" = "secondary_sample_accession"))

meta_curated <- meta_raw %>% 
  dplyr::mutate(
    dataset_name = "Jansson_Lamendella_Crohns",
    study_accession = "PRJEB18471",
    PMID = "28191884",
    subject_accession = host_subject_id %>% as.character(),
    alternative_subject_accession = NA_character_,
    sample_accession = sample_accession %>% as.character(),
    alternative_sample_accession = sample_accession.y %>% as.character(),
    batch = NA_character_,
    sample_accession_16S = run_accession %>% as.character(),
    sample_accession_WGS = NA_character_,
    sample_type = body_site %>% 
      dplyr::recode("UBERON:feces" = "stool"),
    body_site = body_site %>% 
      dplyr::recode("UBERON:feces" = "stool"),
    body_site_additional = NA_character_,
    disease = diagnosis_full %>% 
      dplyr::recode("CD" = "CD",
                    "UC" = "UC",
                    "HC" = "control", 
                    "CC" = "control",
                    "LC" = "control"),
    control = diagnosis_full %>% 
      dplyr::recode("CD" = NA_character_,
                    "UC" = NA_character_,
                    "HC" = "HC",
                    "CC" = "nonIBD",
                    "LC" = "nonIBD"),
    IBD_subtype = ibd_subtype %>% 
      dplyr::recode("CCD" = "cCD", 
                    "ICD_nr" = "iCD",
                    "ICD_r" = "iCD",
                    "UC" = "UC",
                    "LC" = NA_character_,
                    "CC" = NA_character_,
                    "HC" = NA_character_),
    IBD_subtype_additional = ibd_subtype %>% 
      dplyr::recode("CCD" = "not applicable", 
                    "ICD_nr" = "iCD non resection",
                    "ICD_r" = "iCD resection",
                    "UC" = NA_character_,
                    "LC" = NA_character_,
                    "CC" = NA_character_,
                    "HC" = NA_character_),
    L.cat = cd_location %>% 
      dplyr::recode("Ileal (L1)" = "L1",
                    "Colonic (L2)" = "L2",
                    "Ileocolonic (L3)" = "L3",
                    "Ileal and Upper-GI (L1+L4)" = "L1+L4",
                    "Ileocolonic and Upper-GI (L3+L4)" = "L3+L4",
                    "not applicable" = NA_character_),
    E.cat = uc_extent %>% 
      dplyr::recode("Proctitis (E1)" = "E1",
                    "Left sided (E2)" = "E2",
                    "Extensive (E3)" = "E3",
                    "not applicable" = NA_character_),
    B.cat = cd_behavior %>% 
      dplyr::recode("Non-stricturing, non-penetrating (B1)" = "B1",
                    "Stricturing (B2)" = "B2",
                    "Penetrating (B3)" = "B3",
                    "not applicable" = NA_character_),
    perianal = perianal_disease %>% 
      dplyr::recode("yes" = "y",
                    "no" = "n",
                    "not applicable" = NA_character_),
    age = NA_real_,
    age_at_diagnosis = NA_real_,
    age_at_diagnosis.cat = NA_character_,
    race = "white", # All samples are Caucasian according to published manuscript
    gender = sex %>% 
      dplyr::recode("male" = "m",
                    "female" = "f"),
    BMI = bmi %>% 
      dplyr::recode("missing: not provided" = NA_character_) %>% 
      as.numeric(),
    alcohol = NA_character_,
    smoke = NA_character_,
    site = geo_loc_name %>% as.character(),
    calprotectin = calprotectin %>% 
      dplyr::recode("not applicable" = NA_character_,
                    "not collected" = NA_character_) %>% 
      as.numeric(),
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
    time_point = paste0(timepoint, 
                        ":",
                        collection_timestamp),
    time_point_supp = "timepoint:collection_timestamp",
    family = NA_character_,
    family_supp = NA_character_,
    extraction_kit_16S = NA_character_,
    sequencing_platform_16S = NA_character_,
    number_reads_16S = NA_integer_,
    number_bases_16S = NA_integer_,
    minimum_read_length_16S = NA_integer_,
    median_read_length_16S = NA_integer_
  ) %>% dplyr::select(template$col.name %>% dplyr::one_of())

# Two samples seem to have discordant IBD subtype info; set to missing/concordant
meta_curated[meta_curated$sample_accession == "ERS1464142", 
             c("IBD_subtype", "IBD_subtype_additional", "L.cat")] <- c("UC", NA_character_, NA_character_)
meta_curated[meta_curated$sample_accession == "ERS1464315", 
             c("IBD_subtype", "IBD_subtype_additional", "L.cat")] <- c("cCD", "not applicable", "L2")
# One sample is missing E cat somehow
meta_curated[meta_curated$sample_accession == "ERS1464091", "E.cat"] <- "E3"

meta_curated <- meta_curated[, template$col.name]
if(check.template(meta_curated, template)) {
  cat("Metadata for", study,"processed and check successful!\n")
  write.table(meta_curated,
              file = paste0("processed/", study, "/metadata/metadata.txt"),
              quote = F,
              sep = "\t",
              row.names = F)
}
