
#!/bin/bash

#$ -N b7_entropy_range_WP_search
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 
#$ -o job_output/$JOB_NAME-$JOB_ID.log

## MORE RAM
#$ -l m_mem_free=5G

## ARRAY JOB
#$ -t 1-48
#$ -o job_output/$JOB_NAME-$JOB_ID-$TASK_ID.log

Rscript --vanilla b7_entropy_range_WP_search.R ${SGE_TASK_ID}




