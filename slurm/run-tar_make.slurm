#!/bin/bash
#SBATCH --job-name=fhwa              # human-readable label for squeue output
#SBATCH --time=4:00:00                  # maximum time for this job
#SBATCH --output=fhwa_%u.out         # user's output file (password here!)
#SBATCH --partition=cpu                 # which SLURM partition to use
#SBATCH --nodes=1                       # only request one node
#SBATCH --mem=32GB                      # don't settle for less than 32 GB
#SBATCH --mail-type=ALL                 # enable email notifications for job status. Sends to submitting user
#SBATCH -A fhwa                         # project account to charge

#################################################################################
#This script is designed to be run with the container name as an sbatch argument. 
#The container needs to be located in /caldera/projects/usgs/water/impd/fhwa/containers/
#Example:
# sbatch run-tar_make.slurm mycontainer.sif
#################################################################################

module load singularity

#Container path
#argument 1 is the name of the container to use.
CONTAINER="/caldera/projects/usgs/water/impd/fhwa/containers/${1}"

# Run the tar_make() command
# 1 - number of workers to use
singularity exec ${CONTAINER} Rscript 'tar_make.R' "$SLURM_JOB_CPUS_PER_NODE"
