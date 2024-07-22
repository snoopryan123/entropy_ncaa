
#!/bin/bash
#$ -N get_U_chalky_dfs_sMM
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 
#$ -o job_output/$JOB_NAME-$JOB_ID.log
## MORE RAM
#$ -l m_mem_free=15G

Rscript --vanilla b5_simplified_MM_get_U_chalky_dfs.R



