# Code and data for "A cross-species analysis of neuroanatomical covariance sex differences in humans and mice" 

## Requirements:

1. R (version 4.2.3) & RStudio
2. [targets package](https://books.ropensci.org/targets/) 

## Steps:

1. [Clone the repository](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository).
2. Open combined_cov_project.Rproj.
3. Install missing packages (source _targets.R).
4. Run pipeline: `tar_make()` (terminal). Results are stored in the _targets folder and can be called by using `tar_read()` or `tar_load()`. 
5. Generate figures: code_for_final_figures.qmd (Quarto). 

Pipeline runtime: 30 min - 1 hour (consider a supercomputer).


## Notes:

*Any scripts that are under the 'biowulf codes' folder are meant to be run on a supercomputer. The best way to do this is to have this entire repo be on a supercomputing cluster already. If it's not, the easiest way to do this is simply transfer your _targets folder (not script), along with the .R and .sh script onto the cluster. Put all of these in the same directory on your cluster. When you submit your computing job, the R script will refer to the _targets folder in its same directory for any objects/targets required to run the scripts. Computing requirements listed at the beginning of each .sh file is currently biowulf specific -- change it to fit your own computing cluster.

*If you're interested in how the data was cleaned, human data cleaning scripts can be found [here](https://github.com/phamlk/data-clean-human-cross-species-covariance-sex-differences) and mouse data cleaning is [here](https://github.com/phamlk/data-clean-mouse-cross-species-covariance-sex-differences). You will not be able to run these pipelines, as the raw data with all subjects that have been filtered out are not included in either repository. You can view the scripts for data cleaning in the _targets.R file and function folder of each repository. The cleaned data used for the analyses of this repository are the outputs of these data cleaning repositories. Cleaned data are included in both repositories, and the mouse data cleaning repository also has pre-Combatted (but subject filtered data) for your reference.

*Please contact Linh (linh.pham2@nih.gov) for any questions or requests to see the raw, uncleaned data.

