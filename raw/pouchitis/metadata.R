library(SRAdb)
library(magrittr)
utils::download.file('https://huttenhower.sph.harvard.edu/sites/default/files/Pouchitis2015_Metadata_1.txt',
                     destfile = 'raw/pouchitis/metadata_raw.txt')
meta_raw <- read.table('raw/pouchitis/metadata_raw.txt',
                       header = T,
                       sep = '\t',
                       stringsAsFactors = F,
                       check.names = F)
meta_raw$sample_alias <- paste0('PRJNA269954.', meta_raw$`16S_ID`)

sra_con <- dbConnect(SQLite(), 'data/SRAmetadb.sqlite')
sample_accesion <- sraConvert('SRP056002', sra_con = sra_con)$sample %>% unique
sample_table <- dbGetQuery(sra_con, paste0("select * from sample where sample_accession in (",
                                           sample_accesion %>% 
                                             paste0("'", ., "'") %>% 
                                             paste(collapse = ', '),
                                           ")"))


dbGetQuery(sra_con, paste0("select * from sample where sample_alias='PRJNA269954.N00379'"))
meta_raw$Classification[meta_raw$sample_alias %in% setdiff(meta_raw$sample_alias, sample_table$sample_alias)]
