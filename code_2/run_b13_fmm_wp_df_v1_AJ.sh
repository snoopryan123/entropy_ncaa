
#!/bin/bash

#$ -N b13_fmm_entropyTail_wp_df_v1
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 
#$ -o job_output/$JOB_NAME-$JOB_ID.log

## MORE RAM
#$ -l m_mem_free=10G

## ARRAY JOB
#$ -t 1-1
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log

Rscript --vanilla b13_fmm_entropyTail_wp_df.R 1 ${SGE_TASK_ID} 1





