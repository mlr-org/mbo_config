#!/bin/bash

## Set temp to scratch
export TMPDIR=/glade/derecho/scratch/${USER}/tmp && mkdir -p ${TMPDIR}

## Load required environment modules
ml conda mkl openmpi
conda activate mbo_config
