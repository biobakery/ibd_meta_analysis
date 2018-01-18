#!/bin/bash
#
# generate sample identifiers and curate metadata

hutlab load anadama2-0.4.0-devel
hutlab load biobakery_workflows-0.9.0-devel

biobakery_workflows 16s --input raw/RISK/16S/ --output processed/RISK/16S/