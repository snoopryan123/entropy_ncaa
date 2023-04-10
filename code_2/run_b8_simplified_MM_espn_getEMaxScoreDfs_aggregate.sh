
#!/bin/bash

#$ -N b8_simplified_MM_espn_getEMaxScoreDfs_aggregate
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 
#$ -o job_output/$JOB_NAME-$JOB_ID.log

## MORE RAM
#$ -l m_mem_free=5G

Rscript --vanilla b8_simplified_MM_espn_getEMaxScoreDfs_aggregate.R




