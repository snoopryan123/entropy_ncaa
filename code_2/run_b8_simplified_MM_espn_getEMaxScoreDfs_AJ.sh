
#!/bin/bash

#$ -N b8_smm_espn_getEMaxScoreDfs
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 
#$ -o job_output/$JOB_NAME-$JOB_ID.log

## MORE RAM
#$ -l m_mem_free=10G

## ARRAY JOB
#$ -t 1-50  #1-100  #1-1
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log

Rscript --vanilla b8_simplified_MM_espn_getEMaxScoreDfs.R ${SGE_TASK_ID}




