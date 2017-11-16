#!/bin/bash
#
# grab the files, and export it so the 'child' sbatch jobs can access it
R CMD BATCH --quiet --no-restore --no-save \
	Jansson_Lamendella_Crohns_samples.R ../log/Jansson_Lamendella_Crohns_samples.Rout
R CMD BATCH --quiet --no-restore --no-save \
	Jansson_Lamendella_Crohns_curate.R ../log/Jansson_Lamendella_Crohns_curate.Rout
R CMD BATCH --quiet --no-restore --no-save \
	Pouchitis_samples.R ../log/Pouchitis_samples.Rout
R CMD BATCH --quiet --no-restore --no-save \
	Pouchitis_curate.R ../log/Pouchitis_curate.Rout
