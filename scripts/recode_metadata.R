rm(list = ls())
library(magrittr)
source("scripts/misc/helpers.R")
template <- readr::read_csv("data/template_new.csv",
                            col_types = "ccccccccccccc")
for(study in c("BIDMC-FMT",
              "CS-PRISM",
              "Herfarth_CCFA_Microbiome_3B_combined",
              "HMP2",
              "Jansson_Lamendella_Crohns",
              "LSS-PRISM",
              "MucosalIBD",
              "Pouchitis",
              "PROTECT",
              "RISK")) {
  meta_curated <- readr::read_tsv(paste0("processed/",
                                             study, 
                                             "/metadata/metadata.txt"),
                                      col_types = template$var.class %>% 
                                        dplyr::recode("character" = "c",
                                                      "numeric" = "d") %>% 
                                        paste(collapse = ""))
  
  meta_curated_recoded <- meta_curated %>% 
    dplyr::mutate(
      `Dataset name` = dataset_name,
      `PMID` = PMID,
      `Subject accession` = subject_accession,
      `Sample accession` = sample_accession,
      `Sample accession (16S)` = sample_accession_16S,
      `Sample accession (WGS)` = sample_accession_WGS,
      `Sample accession (MBX)` = sample_accession_MBX,
      `Database` = database,
      `Study accession (database)` = study_accession_db,
      `Subject accession (database)` = subject_accession_db,
      `Sample accession (database)` = sample_accession_db,
      `Batch` = batch,
      `Sample type` = sample_type %>% 
        dplyr::recode("biopsy" = "Biopsy",
                      "stool" = "Stool",
                      "serum" = "Serum",
                      "urine" = "Urine"),
      `Body site` = body_site %>% 
        dplyr::recode("ileum" = "Ileum",
                       "colon" = "Colon",
                       "rectum" = "Rectum"),
      `Body site (original encoding)` = body_site_additional,
      `Disease` = disease %>% 
        dplyr::recode("CD" = "CD",
                      "UC" = "UC",
                      "IC" = "IC",
                      "FAP" = "FAP",
                      "control" = "Control"),
      `Control` = control %>% 
        dplyr::recode("HC" = "HC",
                      "nonIBD" = "Non-IBD"),
      `Montreal L classification` = L.cat,
      `Montreal E classification` = E.cat,
      `Montreal B classification` = B.cat,
      `Perianal` = perianal %>% 
        dplyr::recode("y" = "Yes",
                      "n" = "No"),
      `Age` = age,
      `Age at diagnosis` = age_at_diagnosis,
      `Montreal A classification` = age_at_diagnosis.cat,
      `Race` = race %>% 
        dplyr::recode("white" = "White",
                      "african_american" = "African American",
                      "asian_pacific_islander" = "Asian-Pacific Islander",
                      "native_american" = "Native American",
                      "more_than_one" = "More than one",
                      "other" = "Other"),
      `Gender` = gender %>% 
        dplyr::recode("m" = "Male",
                      "f" = "Female"),
      `BMI` = BMI,
      `Alcohol` = alcohol %>% 
        dplyr::recode("y" = "Yes",
                      "n" = "No"),
      `Smoking history` = smoke %>% 
        dplyr::recode("former" = "Former",
                      "current" = "Current",
                      "never" = "Never"),
      `Calprotectin` = calprotectin,
      `PCDAI` = PCDAI,
      `HBI` = HBI,
      `SCCAI` = SCCAI,
      `Antibiotics` = antibiotics %>% 
        dplyr::recode("y" = "Yes",
                      "n" = "No"),
      `Antibiotics (original encoding)` = antibiotics_supp,
      `Immunosuppressants` = immunosuppressants %>% 
        dplyr::recode("y" = "Yes",
                      "n" = "No"),
      `Immunosuppressants (original encoding)` = immunosuppressants_supp,
      `Steroids` = steroids %>% 
        dplyr::recode("y" = "Yes",
                      "n" = "No"),
      `Steroids (original encoding)` = steroids_supp,
      `5ASA` = mesalamine_5ASA %>% 
        dplyr::recode("y" = "Yes",
                      "n" = "No"),
      `5ASA (original encoding)` = mesalamine_5ASA_supp,
      `Biologics` = biologics %>% 
        dplyr::recode("y" = "Yes",
                      "n" = "No"),
      `Biologics (original encoding)` = biologics_supp,
      `Timepoint` = time_point,
      `Timepoint (original encoding)` = time_point_supp,
      `Family ID` = family,
      `Family ID (original encoding)` = family_supp,
      `MBX method` = method_MBX
    ) %>% 
    dplyr::select(dplyr::one_of(template$`proper name`))
  
  
  if(check.template.recoded(meta_curated_recoded, template)) {
    cat("Metadata for", study,"recoded and checked successful!\n")
    write.table(meta_curated_recoded,
                file = paste0("processed/", study, 
                              "/metadata/metadata_recoded.txt"),
                quote = F,
                sep = "\t",
                row.names = F)
  }
}
