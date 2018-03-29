READ ME for 	'Statistical Programming Languages'

Title:		Predicting return probabilities of an online retailer by 
		machine learning algorithms

Authors: 	Wehrmann, Franziska. Ferrari, Damiano. Haeusler, Konstantin.

Content of this folder: 

folder name	|	description
---------------------------------------------------------------------------
data		|	contains the data used in this case study
---------------------------------------------------------------------------
graphs		| 	contains the .R files through which the graphs
		|	have been created and edited
---------------------------------------------------------------------------
latex		|	contains the .tex files through which the term 
		|	paper has been created (also the pdf)
---------------------------------------------------------------------------
nnet		|	.R files for implementation of neural network
---------------------------------------------------------------------------
quantlets	|	contains .R-files and metainfo-files for each 
		|	quantlet
---------------------------------------------------------------------------
rforest		|	.R files for implementation of random forest
---------------------------------------------------------------------------
xgboost		|	.R files for implementation of extreme gradient 
		|	boosting
---------------------------------------------------------------------------


Order of execution: 

1. [refers to section 3 of our seminar paper]
   Data_Cleaning.R

2. [refers to section 4 of our seminar paper]
   Decompose_Dataset.R

3. Prepare_Dataset.R

4.  [refers to section 5 & 6 of our seminar paper]
    a) nnet/00-1-kfold_cv.R    and nnet/00-2-rep_cv.R
    b) rforest/00-1-kfold_cv.R and rforest/00-0-rep_cv.R
    c) xgboost/00-1-kfold_cv.R and xgboost/00-2-rep_cv.R

After having executed these files, one can see that the neural network
outperforms the other two algorithms. Thus, the following fine tuning is
only done for neural network.

5. [refers to section 7 of our seminar paper]
   nnet/01-par_tuning.R
   nnet/02-tau_tuning.R
   nnet/03-1-ref_prediction
   nnet/03-2-true_prediction

