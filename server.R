#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library("rJava")
library("NLP")
library("openNLP")
library("RWeka")
library("tm")
library("stringr")
library("caret")
library("kernlab")
library("quanteda")


## four-grams (given 3 word input: predict 4th word)
predFn<-function(input1,input2,input3){

  if (!exists("UniWf")) load(file="C:\\Users\\YAshroff\\rCoursera\\UniWf.RData")
  if (!exists("BiiWf"))  load(file="C:\\Users\\YAshroff\\rCoursera\\BiWf.RData")
  if (!exists("TriWf")) load(file="C:\\Users\\YAshroff\\rCoursera\\TriWf.RData")
  if (!exists("FourWf")) load(file="C:\\Users\\YAshroff\\rCoursera\\FourWf.RData")
  
    # create 4-gram table
  FourGramTable<-FourWf[(FourWf$Term1==input1) & (FourWf$Term2==input2) & (FourWf$Term3==input3) ,c("Term1","Term2","Term3","Term4","occurrences")]
  
  if (nrow(FourGramTable)>0){
    MatchedFourGramCount<-sum(FourGramTable[c("occurrences")])
  }
  else {MatchedFourGramCount<-0}
  
  # create 3-gram table
  ThreeGramTable<-TriWf[(TriWf$Term1==input2) & (TriWf$Term2==input3) ,c("Term1","Term2","Term3","occurrences")]
  if (nrow(ThreeGramTable)>0){
    MatchedThreeGramCount<-sum(ThreeGramTable[c("occurrences")])
  }
  else {MatchedThreeGramCount<-0}
  
  # create 2-gram table
  BiGramTable<-BiWf[(BiWf$Term1==input3) ,c("Term1","Term2","occurrences")]
  if (nrow(BiGramTable)>0){
    MatchedBiGramCount<-sum(BiGramTable[c("occurrences")])
  }
  else {MatchedBiGramCount<-0}
  
  # create 1-gram table
  MatchedUniGramCount<-UniWf[UniWf$term %in% input3 ,c("occurrences")]
  
  
  #if Four-gram count >0, calculate 4-gram scores
  if (MatchedFourGramCount>0) {
    FourGramTable$score=FourGramTable$occurrences/MatchedThreeGramCount
    colnames(FourGramTable)<-c("Term1","Term2","Term3","FinalTerm","occurrences","score")
    
    ThreeGramTable$score=0.4*ThreeGramTable$occurrences/MatchedBiGramCount
    colnames(ThreeGramTable)<-c("Term2","Term3","FinalTerm","occurrences","score")
    
    BiGramTable$score=0.4*0.4*BiGramTable$occurrences/MatchedUniGramCount
    colnames(BiGramTable)<-c("Term3","FinalTerm","occurrences","score")
    
    finalResult<-rbind(FourGramTable[,c("FinalTerm","score")], ThreeGramTable[,c("FinalTerm","score")],BiGramTable[,c("FinalTerm","score")])
    
  }
  
  else if (MatchedThreeGramCount>0) {
    ThreeGramTable$score=ThreeGramTable$occurrences/MatchedBiGramCount
    colnames(ThreeGramTable)<-c("Term2","Term3","FinalTerm","occurrences","score")
    
    BiGramTable$score=0.4*BiGramTable$occurrences/MatchedUniGramCount
    colnames(BiGramTable)<-c("Term3","FinalTerm","occurrences","score")
    
    finalResult<- rbind(ThreeGramTable[,c("FinalTerm","score")],BiGramTable[,c("FinalTerm","score")])
    
  }
  
  else if (MatchedBiGramCount>0) {
    BiGramTable$score=BiGramTable$occurrences/MatchedUniGramCount
    colnames(BiGramTable)<-c("Term3","FinalTerm","occurrences","score")
    
    finalResult<-BiGramTable[,c("FinalTerm","score")]
    
  }
  
  
  
  # print (head(FourGramTable))
  #  print (head(ThreeGramTable))
  # print (head(BiGramTable))
  
  #calculat final result
  #colnames(BiGramTable)<-c("Term3","FinalTerm","occurrences","score")
  #colnames(ThreeGramTable)<-c("Term2","Term3","FinalTerm","occurrences","score")
  #colnames(FourGramTable)<-c("Term1","Term2","Term3","FinalTerm","occurrences","score")
  
  #finalResult<-rbind(FourGramTable[,c("FinalTerm","score")], ThreeGramTable[,c("FinalTerm","score")],BiGramTable[,c("FinalTerm","score")])
  
  if (exists("finalResult") && nrow(finalResult)>0){
    finalResult<-finalResult[with(finalResult,order(-score)),]
    finalResult$score<-round(finalResult$score*100,digits=2)
    finalResult<-head(finalResult)
    print(finalResult)
    return(finalResult)
    #return(as.character(finalResult[1,"FinalTerm"]))
  }
  else return ("NA")
}


predNextWord<-function(sentence){
  #parse input sentence, apply transformations and pass to predFn
  
  rmPunct <- function(x, pattern,repl) {return (gsub(pattern,repl , x))}
  cleaned <- rmPunct(sentence, "\\!+"," EOS ")
  cleaned <- rmPunct(cleaned, "\\?+"," EOS ")  
  cleaned <- rmPunct(cleaned, "\\.+"," EOS ")  
  cleaned <- rmPunct(cleaned, "\\,+","")  
  cleaned <- rmPunct(cleaned, "\\'+","")  
  
  spl_sent<-strsplit(tolower(cleaned)," ")
  
  if (length(spl_sent[[1]]) >2){
    word3<-spl_sent[[1]][length(spl_sent[[1]])]
    word2<-spl_sent[[1]][length(spl_sent[[1]])-1]
    word1<-spl_sent[[1]][length(spl_sent[[1]])-2]
    finResult<-predFn(word1,word2,word3)
    return (finResult)
  }
  
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  modelPred <- reactive ({
    rmPunct <- function(x, pattern,repl) {return (gsub(pattern,repl , x))}
    cleaned <- rmPunct(input$sentence, "\\!+"," EOS ")
    cleaned <- rmPunct(cleaned, "\\?+"," EOS ")  
    cleaned <- rmPunct(cleaned, "\\.+"," EOS ")  
    cleaned <- rmPunct(cleaned, "\\,+","")  
    cleaned <- rmPunct(cleaned, "\\'+","")  
    
    spl_sent<-strsplit(tolower(cleaned)," ")
    
    if (length(spl_sent[[1]]) >2){
      word3<-spl_sent[[1]][length(spl_sent[[1]])]
      word2<-spl_sent[[1]][length(spl_sent[[1]])-1]
      word1<-spl_sent[[1]][length(spl_sent[[1]])-2]
      predval<-predFn(word1,word2,word3)
      return (predval)
    }
  })
  
  #output$pred1 <- renderText ({
  #  modelPred()
  #})
  
  output$pred1 <- renderDataTable ({
    modelPred()
  })
  
})
