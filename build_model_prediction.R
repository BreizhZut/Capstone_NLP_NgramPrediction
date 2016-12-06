source("NLP_Capstone.R")


build_model_prediction <- function(corpus_dir){
    # takes a directory name as arguement 
    # returns a data frame
    
    message("Reading Corpus")
    # Read the corpus and clean it
    mod_corp <- mk_corpus("sample_2")
    
    message("Building dictionaries")
    # Make the 1 gram term document matrix
    tdm      <- TermDocumentMatrix(mod_corp,control=list(wordLengths=c(1,Inf)))
    # Convert into a term frequency vector by adding the documents
    words    <- Terms(tdm)
    wfreq    <- rowSums(as.matrix(tdm))
    # Build the dictionary
    dsel     <- mk_dict(wfreq,0.9)
    dic_mod  <- words[dsel]
    # Derive a new dictionary with profanity removed
    dic_pred <- clean_dict(dic_mod)
    # Reduce the word frequency array to words in the prediction list
    wfreq    <- wfreq[names(wfreq)%in%dic_pred]
    
    message("Finding trigrams")
    # Clean up we keep (mod_corp,dic_mod,dic_pred,wfreq)
    rm(tdm,words,dsel)
    # create a trigram term documet matrix
    tdm <- TermDocumentMatrix(mod_corp,control=list(
        tokenize=TrigramTokenizer,
        wordLengths=c(1,Inf)))
    # combine all 
    trifreq <- rowSums(as.matrix(tdm))
    
    # clean up
    message("Building trigrams data frame")
    rm(mod_corp) # We won't need the corpus anymore
    rm(tdm)      # We won't need the TermDocumentMatrix either
    sptword <- strsplit(names(trifreq)," ")
    df_trifreq <- data.frame(
        word1 = sapply(sptword,function(x){x[1]}),
        word2 = sapply(sptword,function(x){x[2]}),
        wpred = sapply(sptword,function(x){x[3]}),
        freq  = trifreq
    )
    rm(sptword)
    rm(trifreq) # we won't need this anymore
    # print(object.size(df_trifreq),units="Mb")
    # find words that can be selected 
    selpred           <- df_trifreq$wpred %in% dic_pred
    df_trifreq        <- df_trifreq[selpred,]
    df_trifreq$wpred  <- as.factor(as.character(df_trifreq$wpred)) 
    #print(object.size(df_trifreq),units="Mb")
    # Clean up we keep df_trifreq, dic_mod
    rm(selpred,dic_pred)
    # Divide and conqueer
    
    message("Splitting trigrams data frame")
    selwd <- (! df_trifreq$word1 %in% dic_mod) & (! df_trifreq$word2 %in% dic_mod)
    df_trifreq0 <- df_trifreq[selwd,]
    # Sum up frequencies of as a function of predicted words
    df_trifreq0 <- aggregate(df_trifreq0$freq,
                             by=list(df_trifreq0$wpred),FUN="sum")
    names(df_trifreq0) <- c("wpred","freq")
    #print(object.size(df_trifreq0),units="Mb")
    
    selwd              <- df_trifreq$word1 %in% dic_mod & (! df_trifreq$word2 %in% dic_mod)
    df_trifreq1        <- df_trifreq[selwd,]
    df_trifreq1$word1  <- as.factor(as.character(df_trifreq1$word1))
    # Sum up frequencies of as a function of predicted words and first word
    df_trifreq1        <- aggregate(df_trifreq1$freq,
                                    by=list(df_trifreq1$word1,df_trifreq1$wpred),FUN="sum")
    names(df_trifreq1) <- c("word1","wpred","freq")
    #print(object.size(df_trifreq1),units="Mb")
    
    selwd <- (! df_trifreq$word1 %in% dic_mod) & df_trifreq$word2 %in% dic_mod
    df_trifreq2 <- df_trifreq[selwd,]
    df_trifreq2$word2  <- as.factor(as.character(df_trifreq2$word2))
    # Sum up frequencies of as a function of predicted words and first word
    df_trifreq2        <- aggregate(df_trifreq2$freq,
                                    by=list(df_trifreq2$word2,df_trifreq2$wpred),FUN="sum")
    names(df_trifreq2) <- c("word2","wpred","freq")
    #print(object.size(df_trifreq2),units="Mb")
    
    selwd <- df_trifreq$word1 %in% dic_mod & df_trifreq$word2 %in% dic_mod
    df_trifreq3 <- df_trifreq[selwd,]
    df_trifreq3$word1  <- as.factor(as.character(df_trifreq3$word1))
    df_trifreq3$word2  <- as.factor(as.character(df_trifreq3$word2))
    #print(object.size(df_trifreq3),units="Mb")
    rm(selwd)
    rm(df_trifreq)
    
    
    message("Building 0 gram model")
    word_prediction <- data.frame(
        "word1"="*",
        "word2"="*",
        "prediction"=predict_most_frequent(df_trifreq0)
    )
    rm(df_trifreq0)
    
    fetch_predictions <- function(list_freq){
        prd_gram    <-c()
        for(word in names(list_freq))
            prd_gram <- c(prd_gram,predict_most_frequent(list_freq[[word]]))
        prd_gram
    }
    
    
    message("Building 1 gram models")
    df_trifreq1 <- split(df_trifreq1,df_trifreq1$word1)
    new_prediction <- data.frame(
        "word1" = names(df_trifreq1),
        "word2" = "*",
        "prediction" = fetch_predictions(df_trifreq1))
    word_prediction <- rbind(word_prediction,new_prediction)
    rm(word_prediction,df_trifreq1)
    
    message("Building 1 gram models new")
    df_trifreq2 <- split(df_trifreq2,df_trifreq2$word2)
    new_prediction <- data.frame(
        "word1" = "*",
        "word2" = names(df_trifreq2),
        "prediction" = fetch_predictions(df_trifreq2))
    word_prediction <- rbind(word_prediction,new_prediction)
    rm(word_prediction,df_trifreq2)
    
    message("Building 2 gram model")
    count       <- 0
    df_trifreq3 <- split(df_trifreq3,df_trifreq3$word1)
    for(word1 in names(df_trifreq3)){
        if(count %%1000 == 0){message(paste("Done ",count,"words"))}
        count <- count + 1
        df_bifreq <- df_trifreq3[[word1]]
        df_bifreq$word2 <- as.factor(as.character(df_bifreq$word2))
        df_bifreq <- split(df_bifreq,df_bifreq$word2)
        new_prediction <- data.frame(
            "word1" = word1,
            "word2" = names(df_bifreq),
            "prediction" = fetch_predictions(df_bifreq))
        word_prediction <- rbind(word_prediction,new_prediction)
        rm(new_prediction,bi_freq)}
    
    rm(df_trifreq3)
    word_prediction
}