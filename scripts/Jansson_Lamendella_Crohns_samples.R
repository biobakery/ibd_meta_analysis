setwd("/n/hutlab11_nobackup/users/syma/ibd_meta_analysis/")
source("scripts/source.R")
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

# matched sample accessions
sample_accession <- sample_table$sample_accession
write.table(sample_accession, 
            file = 'raw/Jansson_Lamendella_Crohns/sample_accession.txt',
            row.names = F,
            col.names = F,
            quote = FALSE,
            sep = '\t')

# getting fastq files
dir.create(paste0(data.dir, "fastq/", study, "/"),
           recursive = T)
# only download those that are not already present
fastq_files <- list.files(paste0(data.dir, "fastq/", study, "/"))
samples_to_get <- fastq_files %>% 
  gsub(".tar.gz", "", ., fixed = T) %>% 
  setdiff(sample_accesion, .)
getSRAfile(samples_to_get, sra_con, fileType = 'fastq', 
           destDir = paste0(data.dir, "fastq/", study, "/"))