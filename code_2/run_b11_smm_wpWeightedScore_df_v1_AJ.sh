
#!/bin/bash

#$ -N b11_smm_wpWeightedScore_df_v1
#$ -j y
####$ -m e -M ryguy123@sas.upenn.edu 
#$ -o job_output/$JOB_NAME-$JOB_ID.log

## MORE RAM
#$ -l m_mem_free=10G

## ARRAY JOB
#$ -t 1-47
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log

Rscript --vanilla b11_smm_wpWeightedScore_df.R ${SGE_TASK_ID} 1 47




