
#!/bin/bash

#$ -N mm_chalkyLambda_eMaxScore_df_v1
#$ -j y
####$ -m e -M ryguy123@sas.upenn.edu 
#$ -o job_output/$JOB_NAME-$JOB_ID.log

## MORE RAM
#$ -l m_mem_free=10G

## ARRAY JOB
#$ -t 1-19
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log

Rscript --vanilla mm_chalkyLambda_eMaxScore_df.R 1 ${SGE_TASK_ID} 19 P_538_2022




