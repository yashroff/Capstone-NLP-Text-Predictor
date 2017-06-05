Readme
========================================================
author: Yasneen Ashroff
date: Jan 5 2017
autosize: true

Presentation
========================================================
Please see the interim milestone presentation at http://rpubs.com/yashroff/Capstone and the final presentation at http://rpubs.com/yashroff/CapstonePresentation.

Text Prediction Algorithm
========================================================
The goal of this exercise was to build an app that takes an input phrase and predicts the next word.

- It reads in a corpus of 1,000,000 blog posts, news articles and tweets and builds 1-gram, 2-gram, 3-gram and 4-gram models. It uses the models to calculate frequencies of each individual word encountered in the corpus, each 2-word phrase (bi-gram), each 3-word phrase and each 4-word phrase. These are stored in Document-Term Matrices and used to predict next word user enters.

- Current model uses Stupid Backoff algorithm with Good-Turing Smoothing to calculate probability of each possible next word. 

Limitations
========================================================
- Due to processing limitations on my PC (specifically memory limitations), current model was built using only 20% of the 1 million input entries and only considers n-grams appearing 2 or more times. I also removed the profanity filter from the data cleansing step to increase processing time.  

Future Versions
========================================================
The current accuracy rate is around 15%. While this is low, the Swiftkey accuracy rate is roughly 35%. To improve my algorithm towards the 35% mark:
- Future version will use Katz Backoff algorithm
- I will revisit the data cleansing steps 
- Consider using part-of-speech tagging
- Increase the input data size, perhaps storing the n-gram models in a MySQL database.

Shiny App
========================================================
The shiny app can be found here:
https://yashroff.shinyapps.io/text_prediction/
