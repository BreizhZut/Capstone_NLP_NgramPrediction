library(tm)
library(RWeka)

remove_emails_hashtags <- function(x){
    x <- gsub("[^ ]{1,}@[^ ]{1,}"," ",x)
    x <- gsub(" @[^ ]{1,}"," ",x)
    x <- gsub("#[^ ]{1,}"," ",x) 
    x
}
remove_symbols <- function(x){
    x <- gsub("[`’‘]","'",x)
    x <- gsub("[^a-z']"," ",x)
    x <- gsub("'{2,}"," '",x)
    x <- gsub("' "," ",x)
    x <- gsub(" '"," ",x)
    x <- gsub("^'","",x)
    x <- gsub("'$","",x)
    x
}

short_forms <- data.frame(
    "sub"=c("'d[^a-z]","'s[^a-z]"),
    "rep"=c(" 'd "," 's "))
short_forms  <- rbind(short_forms,data.frame(
    "sub"=c("'ll[^a-z]","'re[^a-z]","'ve[^a-z]"),
    "rep"=c(" 'll "," 're "," 've ")
))
split_shortforms <- function(x){
    for(isf in seq(1,nrow(short_forms))){x <- gsub(short_forms[isf,"sub"],short_forms[isf,"rep"],x)}
    x
}

mk_corpus <- function(dir){
    
    corp_loc <- VCorpus(DirSource(dir))
    # Apply Preprocessing
    corp_loc <- tm_map(corp_loc,content_transformer(tolower))
    corp_loc <- tm_map(corp_loc,content_transformer(remove_emails_hashtags))
    corp_loc <- tm_map(corp_loc,content_transformer(remove_symbols))
    corp_loc <- tm_map(corp_loc,content_transformer(split_shortforms))
    #corp_loc <- 
    tm_map(corp_loc,stripWhitespace)
    #corp_loc <- tm_map(corp_loc,PlainTextDocument)
}

mk_dict <- function(fvect,frac,verbose=FALSE){
    # fvect is a named vector of frequencies
    # create a list of term representing 
    # a ratio $frac of the terms listed in fvect 
    nvect <- sum(fvect)
    targ  <- frac*nvect
    minf  <- 0.
    maxf  <- max(fvect)
    # Trivial cases 
    if(sum(fvect[fvect>=maxf])>targ){return(names(fvect)[fvect>=maxf])}
    # Otherwise find value by dicchotomie
    it     <- 0
    medf   <- 0.5*(minf+maxf)
    cur_r  <- sum(fvect[fvect>=medf])/nvect
    delta  <- (maxf-minf)/medf
    error  <- cur_r - frac
    lastp  <- minf
    minerror <- -1.
    maxerror <- 1.
    if(verbose){
        info_df <- data.frame(
            "minf" = minf,"maxf"=maxf,"medf"=medf,
            "fout" = cur_r,"ftarg"=frac,"error"=error,"nwd"=sum(fvect>=medf))
        row.names(info_df) <- c(it)}
    while(it < 100 & ((delta > 1e-3 & error > 1e-2 ) | error < 0.)){
        
        it <- it +1
        if(error>0){
            #   lastp <- medf 
            minf  <- medf
        } else {
            #  minf <- lastp
            maxf <- medf
        }
        medf   <- 0.5*(minf+maxf)
        delta  <- (maxf-minf)/medf
        cur_r  <- sum(fvect[fvect>=medf])/nvect
        error  <- cur_r - frac
        if(verbose){
            ver_df <- data.frame(
                "minf" = minf,"maxf"=maxf,"medf"=medf,
                "fout" = cur_r,"ftarg"=frac,"error"=error,"nwd"=sum(fvect>=medf)
            )
            row.names(ver_df) <-c(it)
            info_df <- rbind(info_df,ver_df)
        }
    }
    if(verbose){print(ver_df)}
    fvect>=medf
}

validate_dict<-function(samp_dir,words){
    # Return a vector that display for each document the percentile of text sampled 
    corp_loc  <- mk_corpus(samp_dir)
    tdm_loc   <- TermDocumentMatrix(corp_loc,control=list(wordLengths=c(1,Inf)))
    dtype     <- get_doc_type(tdm_loc)
    # this time we don't need a data frame let's use a matrix instead
    tdm_loc   <- as.matrix(tdm_loc)
    # compute the total frequencies
    tot_wd    <- colSums(tdm_loc)
    # select the part of the TermDocument matrix in the dictionary
    tdm_loc   <- tdm_loc[row.names(tdm_loc) %in% words,]
    # compute the new total 
    tot_sel   <- colSums(tdm_loc)
    # And export the percentiles as a clean data frame
    val           <- as.integer(100.*tot_sel/tot_wd)
    names(val)    <- dtype
    val
}

BigramTokenizer <- function(x){
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)}
TrigramTokenizer <- function(x){
    unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)}
bad_words =c(
    "asshole","^ass$",
    "bitch","bollock",
    "^clit$","^cock$","^crap$","^cum$","^cunt",
    "^dick","doos","dork","douche",
    "^fag$","^fart","^fu$","fuck",
    "gook","jerk","nigga","nigger",
    "omfg","^piss$","^puss$","^pussy",
    "^queeb$","shit","squirt")

# source https://en.wiktionary.org/wiki/Category:English_swear_words
clean_dict <- function(dic_in){
    # search match for profane words in the dictionary 
    # any match is removed from the returned list of words
    # The list is hand wirtten in a seperate undisplayed chunck 
    select <- rep(TRUE,length(dic_in))
    for(bad in bad_words){select <- select & ! grepl(bad,dic_in)}
    dic_in[select]
}

get_doc_type <- function(tdm_loc){
    # strip the names of documents 
    # return the type of each
    dtype     <- Docs(tdm_loc)
    dtype     <- sub("^[^.]{1,}.","",dtype)
    dtype     <- sub(".txt$","",dtype)
    dtype
}
convert_tdm_to_df <- function(tdm_loc, renorm=TRUE){
    # This function create a data frame out of a Term document matrix
    df_loc <-data.frame(as.matrix(tdm_loc))
    names(df_loc) <- get_doc_type(tdm_loc)
    if(renorm){
        cnorm <- colSums(df_loc)
        for(ic in seq(1,3)){
            df_loc[,ic] <-  df_loc[,ic]/cnorm[ic]
        }
        # Add extra column with the mean
        df_loc[,"mean"] <- rowMeans(df_loc)
        # And sort the term by mean frequency
        df_loc[order(df_loc$mean,decreasing=TRUE),]
    } else {
        # Add extra column with the total
        df_loc[,"total"]   <- rowSums(df_loc)
        # And sort the term by total frequency
        df_loc[order(df_loc$total,decreasing=TRUE),]
    }
}

predict_most_frequent <- function(gram_select){
    # return the most frequent third word
    # from a subset of the trigram data frame 
    if(nrow(gram_select)==1) {
        ipredict = 1
    } else{
        tiebreak <- wfreq[gram_select$wpred]
        ipredict <- order(gram_select$freq,tiebreak,decreasing=TRUE)[1]
    }
    as.character(gram_select$wpred[ipredict])
}
fetch_predictions <- function(list_freq){
    # 1 gram model
    # return a list of predicted words as a function
    # of the word used to subset part of the trigram data frame
    prd_gram    <-c()
    for(word in names(list_freq))
        prd_gram <- c(prd_gram,predict_most_frequent(list_freq[[word]]))
    prd_gram
}

