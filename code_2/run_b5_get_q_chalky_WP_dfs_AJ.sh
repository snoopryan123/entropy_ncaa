
#!/bin/bash

#$ -N get_q_chalky_WP_dfs
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 
#$ -o job_output/$JOB_NAME-$JOB_ID.log

## MORE RAM
#$ -l m_mem_free=1G

## ARRAY JOB
#$ -t 1-100
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log

Rscript --vanilla b5_simplified_MM_get_q_chalky_WP_dfs.R ${SGE_TASK_ID}




