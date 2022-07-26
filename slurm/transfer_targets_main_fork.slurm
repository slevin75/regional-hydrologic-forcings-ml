#!/bin/bash
#SBATCH --job-name=tar_transfer         # human-readable label for squeue output
#SBATCH --time=1:00:00                  # maximum time for this job
#SBATCH --output=tar_transfer_main_fork_%u.out    # user's output file
#SBATCH --partition=cpu                 # which SLURM partition to use
#SBATCH --nodes=1                       # only request one node
#SBATCH --ntasks=1                      # only request one core
#SBATCH --mail-type=ALL                 # enable email notifications for job status. Sends to submitting user
#SBATCH -A fhwa                         # project account to charge

################################################################################
#This script syncs the _targets folder and output files from the USGS-R/main copy of the repo 
#located at /caldera/projects/usgs/water/impd/fhwa/regional-hydrologic-forcings-ml/
#to a forked copy of the repo

#The directory of the forked repo should be supplied in the sbatch command
#Example:
# sbatch --mail-user=<user>@usgs.gov transfer_targets_main_fork.slurm /path/to/user/fork/of/regional-hydrologic-forcings-ml/

#The intended use is to get targets and file data up to date after a PR is merged.
#Remember to also update your fork's code with git pull
################################################################################

module purge

USGS_DIR='/caldera/projects/usgs/water/impd/fhwa/regional-hydrologic-forcings-ml/'

#Checks that the destination target objects are not newer than the newest objects in the source
#Exits and does not transfer files if they are newer.
cd $USGS_DIR/'_targets/objects'
NEWEST_USGS_OBJECT=$(ls -t | head -n1)
cd $1/'_targets/objects'
NEWEST_FORK_OBJECT=$(ls -t | head -n1)
if [ $1'/_targets/objects'/$NEWEST_FORK_OBJECT -nt $USGS_DIR'_targets/objects'/$NEWEST_USGS_OBJECT ]
then
    printf '%s\n' "$1 has files that are newer than $USGS_DIR so files are not being copied."
    exit 1
fi

#copy _targets folder (using rsync to copy only new targets or updated targets)
rsync -a --omit-dir-times $USGS_DIR'/_targets' "$1"

#copy output files (using rsync to copy only new files or updated files)
#Gather all folder names with a /out directory. The last grep remove any files (. for file extension)
cd $USGS_DIR
SYNC_DIRS=$(ls | grep -E '[0-9]+_' | grep -Fv '.')
for dir in $SYNC_DIRS
do
    #Check that this directory has an /out subdirectory
    OUT_CHECK=$(ls "$USGS_DIR"$dir/ | grep -E 'out')
    if [ ! -z $OUT_CHECK ]
    then
        #sync files
        rsync -a --omit-dir-times "$USGS_DIR"$dir/'out' $1/$dir/
    fi
done