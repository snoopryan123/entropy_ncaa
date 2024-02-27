
#!/bin/bash

#$ -N a2_pick_6_eProfit_v2
#$ -j y
####$ -m e -M ryguy123@sas.upenn.edu 
#$ -o job_output/$JOB_NAME-$JOB_ID.log

## MORE RAM
#$ -l m_mem_free=5G

## ARRAY JOB
#$ -t 1-45
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log

Rscript --vanilla a2_pick_6_eProfit.R 2 ${SGE_TASK_ID} 45




