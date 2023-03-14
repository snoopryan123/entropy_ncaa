
#!/bin/bash
#$ -N bracket_diversity_delta
#$ -j y
#$ -m e -M ryguy123@sas.upenn.edu 
#$ -o job_output/$JOB_NAME-$JOB_ID.log
## MORE RAM
#$ -l m_mem_free=15G

Rscript --vanilla b6a_bracket_diversity_delta_demo.R



