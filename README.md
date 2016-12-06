# Capstone Natural Language Processing N-gram Prediction
This is a repo associated with the 2016 Coursera Capstone Project from John Hopkin's University

## Introduction

The objective of this project is to build a word prediction model such as those that exist on tablets and smartphones. 
The model is to be deployed as a shiny app. As far as I understood, the user type some text, then clicks a button and the app should return a prediction.  The model should use the lest 3 words as input to make a prediction. 
To develop this app, we use the  [Coursera-Swiftkey data set](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip).
At this date, I have no clue how to proceed


## Content

1. Documents:
	* `NLP_MilesStone_dtweed.pdf` a pdf version of the MilesStone Report for this project focussing on exploratory analysis
	*  `NLP_MilesStone_dtweed.md` the markdown version 
	*  `figure_html` directory containing the figures
1. R script (should get updated, cleaned up)
	* `NLP_capstone.R` script file containing function shown in the report
	* `build_word_prediction_model.R` script containing a function take a directory of text documents for training and returns a prediction data frame as a function of the first and second word. 