#! /bin/bash
split_sequence_file_on_sample_ids.py \ 
-i data/16s/Qiita/jansson_twins_ibd/252_seqs.fastq \
	-o raw/jansson_twins_ibd/16s/ \
		--file_type fastq