
#!/bin/bash

#$ -N b12_tmm_eMaxScore_df
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 
#$ -o job_output/$JOB_NAME-$JOB_ID.log

## MORE RAM
#$ -l m_mem_free=10G

## ARRAY JOB
#$ -t 1-3
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log

Rscript --vanilla b12_tmm_eMaxScore_df.R ${SGE_TASK_ID}




