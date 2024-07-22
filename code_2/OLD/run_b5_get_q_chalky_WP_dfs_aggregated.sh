
#!/bin/bash

#$ -N get_q_chalky_WP_dfs_aggregated
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 
#$ -o job_output/$JOB_NAME-$JOB_ID.log

## MORE RAM
#$ -l m_mem_free=5G

Rscript --vanilla b5_simplified_MM_get_q_chalky_WP_dfs_aggregated.R




