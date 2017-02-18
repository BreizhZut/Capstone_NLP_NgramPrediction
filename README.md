# Capstone Natural Language Processing N-gram Prediction
This is a repo associated with the 2016/2017 Coursera Capstone Project from John Hopkin's University

## Introduction

The objective of this project is to build a word prediction model such as those that exist on tablets and smartphones. 
The model is to be deployed as a shiny app. As far as I understood, the user type some text, then clicks a button and the app should return a prediction.  The model should use the lest 3 words as input to make a prediction. 
To develop this app, we use the  [Coursera-Swiftkey data set](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip).
At this date, I have no clue how to proceed


## Content

### Milestone Repport

1. First Milestone report
	* `NLP_MilesStone_dtweed.pdf` a pdf version of the MilesStone Report for this project focussing on exploratory analysis
	*  `NLP_MilesStone_dtweed.md` the markdown version 
	*  `NLP_MilesStone_dtweed_files/figure_html` directory containing the figures
1. Second Milestone report
	* `NLP_MilesStone2_dtweed.pdf` a pdf version of the MilesStone Report for this project focussing on exploratory analysis
	*  `NLP_MilesStone2_dtweed.md` the markdown version 
	*  `NLP_MilesStone2_dtweed_files/figure_html` directory containing the figures

### R scripts
	
1. R scripts relevant to the first report (now outdated)
	* `NLP_capstone.R` script file containing function shown in the report
	* `build_word_prediction_model.R` script containing a function take a directory of text documents for training and returns a prediction data frame as a function of the first and second word. 
1.  R script relevant to the second report
	* `processing.R` Contains the relevant function necessary to process the data
	* `build_dictionaries.R` contains the necessary function to create a dictionary of words
	* `stat_sampling.R` contains the functions necessary to reproduce the data frame used  for creating the different figures of the second report. 

### Data file

1. dictionaries build to represent 10% subsample
	* `dictionary_1.txt` represents 90% of the content including stop words
	* `dictionary_nostp_1.txt` represents 90% of the content excluding stop words
2. `topten.csv`: most frequent words frequencies 
	* Each column represents a word (selected from the 10% subsample)
	* Each row contain the word frequencies for 1% of the full sample
3. `voc.csv`: proportions of different kind of words in the data
	* Columns 1 to 4: nb of words defined as 
		* `wstp`: stop words
		*  `woth`: dictionary words excluding stop words and profanit
		*  'wbad`: profanity
		*  `wout`: not found in the dictionary
	* For each time of word `stp`,`oth`,`bad` and `out`
		* `.blogs`: fraction of this type of word in the blogs file
		* `.news`	: fraction of this type of word in the news file
		* `.twitter`: fraction of this type of word in the twitter file
		* `.all`: fraction of this type of word in all tree files combined
	*  Each row corresponds to 1% of the full sample

4. `nword.csv`
	* Each column correspond to the number of word in a dictionary
		* `all50`: represents 50% of the sub-sample including stop words
		* `nonstop50`: represents 50% of the sub-sample excluding stop words
		* `all90`: represents 90% of the sub-sample including stop words
		* `nonstop90`: represents 90% of the sub-sample excluding stop words
	* Each row correspond to the results obtained on 1% of the full sample 
5. number of N-grams and user time
	* Each column corresponds to the number of words per term
	* `nngram` Each rows is the number of terms for 1% of the full sample
	* `tngram` Each rows is user time require to count the number of term for 1% of the full sample


file `ngram`|file `tgram`|Split sentence|Use dictionary|Remove stop words|
----|----|----|----|----|
`ncut_nngram.csv`|`ncut_tngram.csv`|||
`ncut_nngram_fdic.csv`|`ncut_tngram_fdic.csv`||&#10004;||
`ncut_nngram_fdicstp.csv`|`ncut_tngram_fdicstp.csv`||&#10004;|&#10004;|
`scut_nngram.csv`|`scut_tngram.csv`|&#10004;|||
`scut_nngram_fdic.csv`|`scut_tngram_fdic.csv`|&#10004;|&#10004;||
`scut_nngram_fdcistp.csv`|`scut_tngram_fdcistp.csv`|&#10004;|&#10004;|&#10004;|