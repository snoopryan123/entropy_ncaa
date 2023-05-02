
#!/bin/bash

#$ -N b12_tmm_eMaxScore_df_v2
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 
#$ -o job_output/$JOB_NAME-$JOB_ID.log

## MORE RAM
#$ -l m_mem_free=5G

## ARRAY JOB
#$ -t 1-10
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log

Rscript --vanilla b12_tmm_eMaxScore_df.R 2 ${SGE_TASK_ID}




