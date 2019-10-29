#!/bin/bash
#
# grab the files, and export it so the 'child' sbatch jobs can access it
#!/bin/bash

hutlab load anadama2-0.4.0-devel
hutlab load biobakery_workflows-0.9.0-devel

for dir in raw/*/
do
	study=${dir%*/}
	study=${study##*/}
	echo "Processing" $study
	mkdir -p processed/${study}
	if [ -e ${dir}scripts/metadata.r ]
	then
		echo "Processing metadata"
		mkdir -p processed/${stduy}/metadata/log
		R CMD BATCH --quiet --no-restore --no-save \
			${dir}scripts/metadata.r processed/${stduy}/metadata/log/metadata.Rout
	fi
	if [ -e ${dir}16S/ ]
	then
		echo "Processing 16S"
		mkdir -p processed/${stduy}/16S/
		biobakery_workflows 16s --input ${dir}/16S/ --output processed/${study}/16S/
	fi
done