
#!/bin/bash

#$ -N b12_tmm_eMaxScore_df_aggregation_v1
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 
#$ -o job_output/$JOB_NAME-$JOB_ID.log

## MORE RAM
#$ -l m_mem_free=2G

Rscript --vanilla b12_tmm_eMaxScore_df_aggregation.R 1 




