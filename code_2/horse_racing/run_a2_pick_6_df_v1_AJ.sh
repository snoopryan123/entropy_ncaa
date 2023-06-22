
#!/bin/bash

#$ -N a2_pick_6_eProfit_v1
#$ -j y
####$ -m e -M ryguy123@sas.upenn.edu 
#$ -o job_output/$JOB_NAME-$JOB_ID.log

## MORE RAM
#$ -l m_mem_free=5G

## ARRAY JOB
#$ -t 1-50
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log

Rscript --vanilla a2_pick_6_eProfit.R 1 ${SGE_TASK_ID} 50




