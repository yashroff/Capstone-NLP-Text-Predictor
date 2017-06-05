# TextPred
Capstone project: given input of 2,3 or 4 words, predict the next word

Please see final presentation at https://rpubs.com/yashroff/CapstonePresentation and Interim Presentation at 

# Text Prediction Algorithm

* The goal of this exercise was to build an app that takes an input phrase and predicts the next word.

* It reads in a corpus of 1,000,000 blog posts, news articles and tweets and builds 1-gram, 2-gram, 3-gram and 4-gram models. It uses the models to calculate frequencies of each individual word encountered in the corpus, each 2-word phrase (bi-gram), each 3-word phrase and each 4-word phrase. These are stored in Document-Term Matrices and used to predict next word user enters.

* Current model uses Stupid Backoff algorithm with Good-Turing Smoothing to calculate probability of each possible next word.

# Limitations

* Due to processing limitations on my PC (specifically memory limitations), current model was built using only 20% of the 1 million input entries and only considers n-grams appearing 2 or more times. I also removed the profanity filter from the data cleansing step to increase processing time.
