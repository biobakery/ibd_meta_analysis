#! /bin/bash
split_sequence_file_on_sample_ids.py \
	-i data/16s/Qiita/Jansson_Lamendella_Crohns/seqs.fastq \
		-o raw/Jansson_Lamendella_Crohns/16s/ \
			--file_type fastq