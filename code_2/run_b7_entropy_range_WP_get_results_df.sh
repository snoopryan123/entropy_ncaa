
#!/bin/bash

#$ -N b7_entropy_range_WP_get_results_df
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 
#$ -o job_output/$JOB_NAME-$JOB_ID.log
#$ -l m_mem_free=1G

Rscript --vanilla b7_entropy_range_WP_getResults.R 




