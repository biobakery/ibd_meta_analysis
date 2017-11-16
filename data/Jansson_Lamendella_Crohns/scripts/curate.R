setwd("/n/hutlab11_nobackup/users/syma/ibd_meta_analysis/")
source("scripts/source.R")
study <- "Jansson_Lamendella_Crohns"

template <- read.csv('data/template.csv',
                     stringsAsFactors = F)
meta_raw_merged <- read.table(paste0("raw/", study, "/metadata_raw_merged.txt"),
                              header = T,
                              sep = '\t',
                              stringsAsFactors = F,
                              check.names = F)
meta_curated <- meta_raw_merged %>% 
  mutate(dataset_name = study,
         study_accession = "ERP020401",
         PMID = "28191884",
         sample_accession = sample_accession,
         sampleID = paste0(study_accession, ":", sample_accession),
         subjectID = paste0(study_accession, ":", description.x),
         body_site = body_site %>% 
           recode("UBERON:feces" = "stool"),
         sample_type = sample_type,
         disease = diagnosis_full %>% 
           recode("CD" = "CD",
                  "UC" = "UC",
                  "HC" = "control", 
                  "CC" = "control",
                  "LC" = "control"),
         control = diagnosis_full %>% 
           recode("CD" = "not applicable",
                  "UC" = "not applicable",
                  "HC" = "healthy",
                  "CC" = "collagenous colitis",
                  "LC" = "lymphocytic colitis"),
         IBD_subtype = ibd_subtype %>% 
           recode("CCD" = "cCD", 
                  "ICD_nr" = "iCD",
                  "ICD_r" = "iCD",
                  "UC" = "UC",
                  "LC" = "not applicable",
                  "CC" = "not applicable",
                  "HC" = "not applicable"),
         IBD_subtype_additional = ibd_subtype %>% 
           recode("CCD" = "not applicable", 
                  "ICD_nr" = "iCD non resection",
                  "ICD_r" = "iCD resection",
                  "UC" = "not applicable",
                  "LC" = "not applicable",
                  "CC" = "not applicable",
                  "HC" = "not applicable"),
         L.cat = cd_location %>% 
           recode("Ileal (L1)" = "L1",
                  "Colonic (L2)" = "L2",
                  "Ileocolonic (L3)" = "L3",
                  "Ileal and Upper-GI (L1+L4)" = "L1+L4",
                  "Ileocolonic and Upper-GI (L3+L4)" = "L3+L4",
                  "not applicable" = "not applicable"),
         E.cat = uc_extent %>% 
           recode("Proctitis (E1)" = "E1",
                  "Left sided (E2)" = "E2",
                  "Extensive (E3)" = "E3",
                  "not applicable" = "not applicable"),
         B.cat = cd_behavior %>% 
           recode("Non-stricturing, non-penetrating (B1)" = "B1",
                  "Stricturing (B2)" = "B2",
                  "Penetrating (B3)" = "B3",
                  "not applicable" = "not applicable"),
         perianal = perianal_disease %>% 
           recode("yes" = "y",
                  "no" = "n",
                  "not applicable" = "not applicable"),
         age = NA,
         gender = sex %>% 
           recode("male" = "m",
                  "female" = "f"),
         BMI = bmi %>% 
           recode("missing: not provided" = "NA") %>% 
           as.numeric,
         alcohol = NA,
         smoke = NA,
         country = geo_loc_name,
         calprotectin = calprotectin %>% 
           recode("not applicable" = "-1",
                  "not collected" = "NA") %>% 
           as.numeric,
         antibiotics_usage = NA,
         antibiotics_family = NA,
         time_point = timepoint,
         family = NA,
         DNA_extraction_kit = NA,
         sequencing_platform = NA,
         number_reads = NA,
         number_bases = NA,
         minimum_read_length = NA,
         median_read_length = NA
) %>% select(template$col.name %>% one_of)

for(var in setdiff(template$col.name, colnames(meta_curated))) {
  meta_curated[[var]] <- NA
}
meta_curated <- meta_curated[, template$col.name]
write.table(meta_curated,
            file = paste0("curated/", study, ".txt"),
            quote = F,
            sep = "\t",
            row.names = F)
