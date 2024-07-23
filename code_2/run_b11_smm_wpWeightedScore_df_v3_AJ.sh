
#!/bin/bash

#$ -N b11_smm_wpWeightedScore_df_v3
#$ -j y
####$ -m e -M ryguy123@sas.upenn.edu 
#$ -o job_output/$JOB_NAME-$JOB_ID.log

## MORE RAM
#$ -l m_mem_free=10G

## ARRAY JOB
#$ -t 1-54
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log

Rscript --vanilla b11_smm_wpWeightedScore_df.R ${SGE_TASK_ID} 3 54




