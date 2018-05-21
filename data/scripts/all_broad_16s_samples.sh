#! /bin/bash
ls -l 16s/broad/reads/ | awk -v OFS="\t" '$1=$1' > 16s/broad/all_reads_GID.txt 
