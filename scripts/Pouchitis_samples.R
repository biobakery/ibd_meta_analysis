setwd("/n/hutlab11_nobackup/users/syma/ibd_meta_analysis/")
source("scripts/source.R")
study <- "Pouchitis"

dir.create(paste0("raw/", study, "/"), recursive = T)
# metadata downloaded from https://huttenhower.sph.harvard.edu/sites/default/files/Pouchitis2015_Metadata_1.txt
meta_raw <- read.table(paste0("raw/", study, "/metadata_raw.txt"),
                       header = T,
                       sep = '\t',
                       stringsAsFactors = F,
                       check.names = F)
meta_raw$sample_alias <- paste0('PRJNA269954.', meta_raw$`16S_ID`)

sra_con <- dbConnect(SQLite(), paste0(data.dir, "SRAdb/SRAmetadb.sqlite"))
# sample accessions for the sequences
sample_accession <- sraConvert('SRP056002', sra_con = sra_con)$sample %>% unique
# table of sample attributes
sample_table <- dbGetQuery(sra_con, paste0("select * from sample where sample_accession in (",
                                           sample_accesion %>% 
                                             paste0("'", ., "'") %>% 
                                             paste(collapse = ', '),
                                           ")"))
sample_common <- intersect(meta_raw$sample_alias, sample_table$sample_alias)
meta_raw <- meta_raw %>% subset(sample_alias %in% sample_common)
sample_table <- sample_table %>% subset(sample_alias %in% sample_common)
meta_raw_merged <- meta_raw %>% left_join(sample_table, by = 'sample_alias')
write.table(meta_raw_merged, file = paste0("raw/", study, "/metadata_raw_merged.txt"),
            row.names = F,
            quote = F,
            sep = "\t")

# matched sample accessions
sample_accession <- sample_table$sample_accession %>% 
  sraConvert(sra_con = sra_con) %>% 
  select(sample, run)
write.table(sample_accession, 
            file =  paste0("raw/", study, "/sample_accession.txt"),
            row.names = F,
            quote = FALSE,
            sep = '\t')

# getting fastq files
dir.create(paste0(data.dir, "fastq/", study, "/"),
           recursive = T)
# only download those that are not already present
fastq_files <- list.files(paste0(data.dir, "fastq/", study, "/"))
run_to_get <- fastq_files %>% 
  gsub(".fastq.gz", "", ., fixed = T) %>% 
  setdiff(sample_accession$run, .)
if(length(run_to_get) > 0) getSRAfile(run_to_get, sra_con, fileType = 'fastq', 
                                      destDir = paste0(data.dir, "fastq/", study, "/"))