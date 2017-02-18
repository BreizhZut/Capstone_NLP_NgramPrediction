## This module contains the required functions to build dictionaries of the most frequent terms
library(tm)
library(stringr)


select_term <- function(fwvect,fcomp=0.9,verbose=0){
    ## returns a vector of boolean
    ## fvect is a vector of frequencies 
    ## option fcomp, set the fraction of cumulative frequencies to be represented
    ## option verbose for set to TRUE for debugging 
    ## normalise fwvect
    fwvect <- fwvect/sum(fwvect)
    flo    <- min(fwvect)
    fhi    <- max(fwvect)
    ## Trivial cases
    ## Take everything
    if(sum(fwvect<=flo)*flo+fcomp>=1.){
        if(verbose==2){message("In build_dict: Trivial take all words")}
        return(rep(TRUE,length(fwvect)))}
    ## Take only the highest
    if(fhi>fcomp){
        if(verbose==2){print("In build_dict: Trivial take most frequent words")}
        return(fwvect>=fhi)}
    ## Non trivial cases use diccchotomy
    fmed   <- 0.5*(fhi+flo)
    err    <- sum(fwvect[fwvect>=fmed]) - fcomp
    dlt    <- (fhi-flo)/fmed
    it     <- 0
    if(verbose==2){
        info_df <- data.frame(
            "minf" = flo,"maxf"=fhi,"medf"=fmed,
            "error"=err,"nwd"=sum(fwvect>=fmed))
        row.names(info_df) <- c(it)}
    while(it<100 & ((dlt>1e-3 & err > 1e-2)|err<0.)){
        it <- it +1
        if(err>0){flo <- fmed}else{fhi<-fmed}
        fmed <- 0.5*(fhi+flo)
        err  <- sum(fwvect[fwvect>fmed]) - fcomp
        dlt  <- (fhi-flo)/fmed
        if(verbose>=1){
            ver_df <- data.frame(
                "minf" = flo,"maxf"=fhi,"medf"=fmed,
                "error"=err,"nwd"=sum(fwvect>=fmed))
            row.names(ver_df) <- c(it)
            if(verbose==2){info_df <- rbind(info_df,ver_df)}
        }
    }   	
    if(verbose==1){print(ver_df)}
    if(verbose==2){print(info_df)}
    fwvect >=fmed}

filter_dict <- function(dic_in){
    ## search match for profane words in the dictionary 
    ## any match is removed from the returned list of words
    ## also remove words starting with . and '
    select <- !grepl("^['.]",dic_in)
    for(bad in bad_words){select <- select & ! grepl(bad,dic_in)}
    dic_in[select]}

build_dict <- function(sample_corpus,fcomp=0.9,verbose=0,normalize=FALSE,outdict=NULL){
    ## use a corpus as input.
    ## create in the global environement 3 lists of words
    ## create the TermDocumentMatrix first
    ## return the number of words in each dictionary    
    tdm   <- TermDocumentMatrix(sample_corpus,control=list(wordLengths=c(1,Inf)))
    tdm   <- as.matrix(tdm)
    ## remove '.' from list of words 
    tdm["<stop>",] <- rep(0,ncol(tdm))
    ## create a TermVector from it
    if(normalize){
        doc_norms <- colSums(tdm)
	for(doc in seq(1,length(doc_norms))){tdm[,doc]<-tdm[,doc]/doc_norms[doc]}
    }
    qfreq <- rowSums(tdm)
    rm(tdm)
    dict_sel <- select_term(qfreq,fcomp=fcomp,verbose=verbose)
    dict_var <<- names(qfreq)[dict_sel]
    dict_prd <<- filter_dict(dict_var)
    if(is.character(outdict)){save_dict(outdict)}
    c(length(dict_var),length(dict_prd))}

save_dict <- function(output){
    if(!exists(dict_var)){return}
    write.table(c(
        paste(dict_var,collapse=" "),
        paste(my_stopwords,collapse=" "),
        paste(bad_words,collapse=" ")),
        output,row.names=FALSE,col.names=FALSE,quote=FALSE)}

load_dict <- function(indict){
    dict  <- file(indict,"r")
    lines <- readLines(dict)
    close(dict)
    dict_var     <<- unlist(strsplit(lines[1]," "))
    my_stopwords <<- unlist(strsplit(lines[2]," "))
    bad_words    <<- unlist(strsplit(lines[3]," "))
    dict_prd     <<- filter_dict(dict_var)
c(length(dict_var),length(dict_prd))}


get_doc_type <- function(tdm_loc){
    ## strip the names of documents 
    ## return the type of each
    dtype     <- Docs(tdm_loc)
    dtype     <- sub("^[^.]{1,}.","",dtype)
    dtype     <- sub(".txt$","",dtype)
    dtype}


test_dict <- function(sample_corpus){
    ## create term document matrix
    tdm   <- TermDocumentMatrix(sample_corpus,control=list(wordLengths=c(1,Inf)))
    docs  <- get_doc_type(tdm)
    tdm   <- as.matrix(tdm)
    ## add sums accross documents
    tdm   <- cbind(tdm,rowSums(tdm))
    rownames(tdm) <- c(docs,"total")
    ## normalize
    doc_norms <- colSums(tdm)
    for(doc in seq(1,length(doc_norms))){tdm[,doc]<-tdm[,doc]/doc_norms[doc]}
}
