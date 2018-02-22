#!/bin/bash
#
# generate sample identifiers and curate metadata

hutlab load anadama2-0.4.0-devel
hutlab load biobakery_workflows-0.9.0-devel

biobakery_workflows wmgx --input raw/PRISM_WGS/WGS/ --output raw/PRPISM_WGS/WGS/ --grid-jobs 5 --threads 8 --input-extension fq.gz --pair-identifier .1
