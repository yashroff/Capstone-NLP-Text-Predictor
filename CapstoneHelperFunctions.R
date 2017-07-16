library("rJava")
library("NLP")
library("openNLP")
library("RWeka")
library("tm")
library("stringr")
library("caret")
library("kernlab")


createSampleSet <- function(inputFileName,outputTrainFile,outputTestFile){  
    #open and read the corpus file
    setwd("C:/Users/Yasneen/rCoursera/en_US_samples")
    con<-file(inputFileName)
    inList<-readLines(con,skipNul=TRUE)
    lineNum<-1
    if (file.exists(outputTrainFile)) file.remove(outputTrainFile)
    if (file.exists(outputTestFile)) file.remove(outputTestFile)
    
    for (i in 1:round(length(inList)/100)){
        #c[lineNum]<-inList[i]
        write(inList[i],file=outputTrainFile,append=TRUE)
        lineNum<-lineNum+1
    }
    
    #create test file by taking the next 1%
    for (i in lineNum+1:lineNum+1+round(length(inList)/100)){
         write(inList[i],file=outputTestFile,append=TRUE)
    }
}

#profanity filtering function
profanityFilter <- content_transformer(function(w) {
    #remove profanity: first load profanity word list
    bad_words_con<-file("C:\\Users\\Yasneen\\rCoursera\\en_US_samples\\full-list-of-bad-words-banned-by-google-txt-file_2013_11_26_04_53_31_867.txt")
    bad_words_list<-readLines(bad_words_con,skipNul=TRUE)
    #remove white space from profanity file
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)
    bad_words_list<-trim(bad_words_list)
    for(i in 1:length(bad_words_list)){
        w <-  gsub(trimws(bad_words_list[i]),"", w)
    }
    return(w)
})

### TRANSFORMATIONS
#########################

doTransformations <- function(c){

    c <- tm_map(c, profanityFilter)
    #Transform to lower case
    c <- tm_map(c,content_transformer(tolower))
    #remove non-ascii characters
    nonAscii<-content_transformer(function(x) {iconv(x, "latin1", "ASCII", sub="")})
    c <- tm_map(c, nonAscii)
    
    #remove non-standard punctuation by converting them to blanks
    #create the toSpace content transformer
    toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, "", x))})
    
    c <- tm_map(c, toSpace, "’")
    c <- tm_map(c, toSpace, "‘")
    c <- tm_map(c, toSpace, " -")
    
    c <- tm_map(c, toSpace, "'")
 
    
    #convert .?! to EOS
    toEOS <- content_transformer(function(x, pattern) {return (gsub(pattern, " EOS ", x))})
    c <- tm_map(c, toEOS, "\\!+")
    c <- tm_map(c, toEOS, "\\?+")
    c <- tm_map(c, toEOS, "\\.")
    
    #remove punctuation
    c <- tm_map(c,removePunctuation)
    
    
    #remove any remaining non-standard punctuation
    #c <- tm_map(c, toSpace, "â€™")
    #c <- tm_map(c, toSpace, "â€œ")
    #c <- tm_map(c, toSpace, "â€")
    
    #we can derive an approximate  percentage of "foreign" words by comparing # unique terms in the corpus before and after removing non-ascii characters
    #how many are not English? We can load a dictionary and verify each word but this will be very processing-intensive so will not implement this for now
    #enWords<- download.file(url='http://www-01.sil.org/linguistics/wordlists/english/wordlist/wordsEn.txt',destfile='wordsEn.txt')
    #enCon<-file("C:\\Users\\Yasneen\\rCoursera\\en_US_samples\\wordsEn.txt")
    #english_words_list<-readLines(enCon,skipNul=TRUE)
    
    #convert back to a corpus: words1 <- Corpus(VectorSource(dat1))
    return (c)
}

#create uni-gram tokenizer using rweka
########################################
unigramTokenize<- function(c){
    unigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1)) # create 1-grams
    # create  DTM
    UniDtm <- TermDocumentMatrix(c, control = list(tokenize = unigramTokenizer, wordLengths = c(1, Inf))) 
    
    UniDtmMatrix<-as.matrix(UniDtm)
    topUniGrams <- rowSums(UniDtmMatrix)
    topUniGrams<-sort(topUniGrams, decreasing = TRUE)
    UniWordFreq=data.frame(term=names(topUniGrams),occurrences=topUniGrams)
    return (UniWordFreq)
}


#create bi-gram tokenizer
########################################
bigramTokenize<-function(c){
    BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2)) # create 2-grams
    # create BiGram DTM
    BiDtm <- TermDocumentMatrix(c, control = list(tokenize = BigramTokenizer,wordLengths = c(1, Inf))) 
    ## EXPLORATORY DATA ANALYSIS: Bi-grams
    ######################################
    BiDtmMatrix<-as.matrix(BiDtm)
    
    #sort bi-grams by frequency. What are the most common? "OF THE", "IN THE"
    topBiGrams <- rowSums(BiDtmMatrix)
    topBiGrams<-sort(topBiGrams, decreasing = TRUE)
    BiWordfreq=data.frame(term=names(topBiGrams),occurrences=topBiGrams)
    #  split the n-grams and store composite words separately as Term1, Term2, Term3
    #Bi-grams
    temp<-str_split(BiWordfreq$term," ")
    mat  <- matrix(unlist(temp), ncol=2, byrow=TRUE)
    Bidf   <- as.data.frame(mat)
    colnames(Bidf) <- c("Term1","Term2")
    BiWordfreq$Term1<-Bidf$Term1
    BiWordfreq$Term2<-Bidf$Term2
    return (BiWordfreq)
}

#create tri-gram tokenizer
########################################
trigramTokenize<-function(c){
    TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3)) # create 3-grams
    # create TriGram DTM
    TriDtm <- TermDocumentMatrix(c, control = list(tokenize = TrigramTokenizer,wordLengths = c(1, Inf))) 
    
    TriDtmMatrix<-as.matrix(TriDtm)
    
    #sort bi-grams by frequency. What are the most common? "OF THE", "IN THE"
    topTriGrams <- rowSums(TriDtmMatrix)
    topTriGrams<-sort(topTriGrams, decreasing = TRUE)
    TriWordfreq=data.frame(term=names(topTriGrams),occurrences=topTriGrams)
    #  split the tri-grams and store composite words separately as Term1, Term2, Term3
    temp<-str_split(TriWordfreq$term," ")
    mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)
    Tridf   <- as.data.frame(mat)
    colnames(Tridf) <- c("Term1","Term2","Term3")
    TriWordfreq$Term1<-Tridf$Term1
    TriWordfreq$Term2<-Tridf$Term2
    TriWordfreq$Term3<-Tridf$Term3
    
    rm (temp)
    rm (Tridf)
    rm (topTriGrams)
    rm (TriDtmMatrix)
    rm (TriDtm)
    gc ()
    return (TriWordfreq)
}



#create four-gram tokenizer
######################################
fourgramTokenize<-function(c){
    #create Four-gram tokenizer
    FourgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4)) # create 4-grams
    # create FourGram DTM
    FourDtm <- TermDocumentMatrix(c, control = list(tokenize = FourgramTokenizer)) 
    
    FourDtmMatrix<-as.matrix(FourDtm)
    
    #sort four-grams by frequency. What are the most common? 
    topFourGrams <- rowSums(FourDtmMatrix)
    topFourGrams<-sort(topFourGrams, decreasing = TRUE)
    FourWordfreq=data.frame(term=names(topFourGrams),occurrences=topFourGrams)
    
    
    #  split the Four-grams and store composite words separately as Term1, Term2, Term3, Term4
    temp<-str_split(FourWordfreq$term," ")
    mat  <- matrix(unlist(temp), ncol=4, byrow=TRUE)
    Fourdf   <- as.data.frame(mat)
    colnames(Fourdf) <- c("Term1","Term2","Term3","Term4")
    FourWordfreq$Term1<-Fourdf$Term1
    FourWordfreq$Term2<-Fourdf$Term2
    FourWordfreq$Term3<-Fourdf$Term3
    FourWordfreq$Term4<-Fourdf$Term4
    
    rm (temp)
    rm (Fourdf)
    rm (topFourGrams)
    rm (FourDtmMatrix)
    rm (FourDtm)
    gc ()
    return (FourWordfreq)
}


#function to create a split  input file into test and training
#this tries to split data partitions in memory but kept runing out of RAM
createSampleSetInMem <- function(inputFileName,outputTrainFile,outputTestFile) {
    #open and read the corpus file
    setwd("C:/Users/Yasneen/rCoursera/en_US_samples")
    con<-file(inputFileName)
    
    #assign line # to each file
    inList<-readLines(con,skipNul=TRUE)
    #inList<-as.list(inList)
    ToSplit<-data.frame()
    for (i in 1:length(inList)){
        ToSplit[i,1]<- inList[i]
        ToSplit[i,2]<-lengthList[i]
        #  write(training[training$index==i,c("phrase")],file=outputTrainFile,append=TRUE)
    }
    
    
    lengthList<-(1:length(inList))
    ToSplit<-data.frame(inList,lengthList)
    ToSplit<-cbind.data.frame(inList,lengthList)
    names(ToSplit)<-c("phrase","index")
    inTrain<-createDataPartition(y=ToSplit$lengthList,p=0.9,list=FALSE)
    training<-ToSplit[inTrain,]
    testing<-ToSplit[-inTrain,]
    
    if (file.exists(outputTrainFile)) file.remove(outputTrainFile)
    if (file.exists(outputTestFile)) file.remove(outputTestFile)
    
    for (i in inTrain){
        #write(training[i,1],file=outputTrainFile,append=TRUE)
        write(training[training$index==i,c("phrase")],file=outputTrainFile,append=TRUE)
    }
    
    
    for (i in -inTrain){
        write(testing[i,1],file=outputTestFile,append=TRUE)
    }
    
}


### prediction functions: old versions
#####################################
#return  most common unigram in the input word list
predUnigram<-function(input){
    matchedTerms<- wf[wf$term %in% input,]
    maxval<-max(matchedTerms["occurrences"])
    retval<-matchedTerms[(matchedTerms$occurrences==maxval),c("term")]
    return (retval[1])
}


## bi-grams (given 1 word input: predict 2nd word)
predSingle<-function(input){
    # given first term, extract second term with highest frequency 
    #BiWf[(BiWf$Term1==k) ,]
    maxval<-max(BiWf[(BiWf$Term1==input) ,c("occurrences")])
    retval<-BiWf[(BiWf$Term1==input) & (BiWf$occurrences==maxval),c("Term2")]
    #if there's only 1, return it 
    if (length(retval)==1){
        return (retval[1])
    }
    #if >1, find most common unigram
    else if (length(retval)>1) {
        return (predUnigram(retval))
    }
    #if 0, find most common unigram
    else if (length(retval)==0) {
        return (predSingle(input))
    }
    
}

#take sentence and predict next word
predFn<-function(sentence){
    sentence<-tolower(sentence)
    #check if input is one word only
    spc<-str_locate(sentence," ")
    if (is.na(spc[1])){
        return (predSingle(mat[1]))
    }
    else {
        #tokenize sentence by spaces
        temp<-str_split(sentence," ")
        mat  <- matrix(unlist(temp), byrow=TRUE)
        
        if (length(mat)==2){
            return (predDouble(mat[1],mat[2]))
        }
        else if (length(mat)==3){
            return(predTriple(mat[1],mat[2],mat[3]))
        }
        else if (length(mat) > 3){
            return(predTriple(mat[length(mat)-2],mat[length(mat)-1],mat[length(mat)]))
        }
    }
}

