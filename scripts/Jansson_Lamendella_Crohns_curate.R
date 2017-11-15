setwd("/n/hutlab11_nobackup/users/syma/ibd_meta_analysis/")
source("scripts/source.R")
library(tidyverse)
study <- "Jansson_Lamendella_Crohns"

meta_raw <- read.table(paste0("raw/", study, "/metadata_raw.txt"),
                       header = T,
                       sep = '\t',
                       stringsAsFactors = F,
                       check.names = F)
meta_raw$sample_alias <- paste0('qiita_sid_1629:', meta_raw$sample_name)

sra_con <- dbConnect(SQLite(), paste0(data.dir, "SRAdb/SRAmetadb.sqlite"))
# sample accessions for the sequences
sample_accesion <- sraConvert('ERP020401', sra_con = sra_con)$sample %>% unique
# table of sample attributes
sample_table <- dbGetQuery(sra_con, paste0("select * from sample where sample_accession in (",
                                           sample_accesion %>% 
                                             paste0("'", ., "'") %>% 
                                             paste(collapse = ', '),
                                           ")"))
sample_common <- intersect(meta_raw$sample_alias, sample_table$sample_alias)
meta_raw <- meta_raw %>% subset(sample_alias %in% sample_common)
sample_table <- sample_table %>% subset(sample_alias %in% sample_common)