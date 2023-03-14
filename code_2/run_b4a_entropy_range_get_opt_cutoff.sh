
#!/bin/bash
#$ -N get_opt_cutoff
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 
#$ -o job_output/$JOB_NAME-$JOB_ID.log
## MORE RAM
#$ -l m_mem_free=15G

Rscript --vanilla b4a_entropy_range_get_opt_cutoff.R



