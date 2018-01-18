#!/bin/bash
#
# generate sample identifiers and curate metadata

hutlab load anadama2-0.4.0-devel
hutlab load biobakery_workflows-0.9.0-devel

biobakery_workflows 16s --input raw/Jansson_Lamendella_Crohns/16S/ --output processed/Jansson_Lamendella_Crohns/16S/