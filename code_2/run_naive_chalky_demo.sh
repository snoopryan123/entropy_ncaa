
#!/bin/bash
#$ -N naive_chalky_demo.R
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 
#$ -o job_output/$JOB_NAME-$JOB_ID.log
## MORE RAM
#$ -l m_mem_free=150G

Rscript --vanilla b1_naive_chalky_demo.R



