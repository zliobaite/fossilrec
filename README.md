# Recommender systems for species distribution modeling

This is a repository to accompany a research paper:

Zliobaite, I. (2021). Recommender systems for fossil species distribution modeling. Currently under review.

The repository contains input datasets as well as scripts for running the analysis and reproducing the figures and tables reported in the manuscript. The occurence matrix is compiled from a public data dump downloaded from on 20210309. Data preprocessing steps are described in the manuscript.

The scripts are not very well annotated for the time being, sorry about that. I hope to improve that upon publication of the study. I put random seeds in order to be able to reproduce the exact instantiations of the experiments. The randomized component is the initialization of the starting matrixes for factorization. 


The scripts for reproducing the analysis are as follows:

	run1_factorization.R	

taxes genus-sites occurence matrix as input and does matrix factorization.

	run2_loop_parameters.R
	
does matrix factorization with different parameter settings (grid search), this tests 64 variants of parameter settings and takes a few hours to complete on a commodity laptop. This experiment is reported in Appendix A of the manuscript.

	run3_abundances.R

is the experiment of predicting relative abundances.

	run4_fill_indets.R
	
is the experiment of predicting indetermined cerdivs.

	run5_clusters.R
	
produces the community tree for analysis of companionships.