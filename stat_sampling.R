source("processing.R")
source("build_dictionaries.R")

############################################################
## Sampling
############################################################

sample_corpus <- function(input_dir,output_dir,ratio,iseed){
    ## search directory "input_dir" for "blogs, news and twitter files"
    ## sample a fraction of each file
    ## copy the sampled line into new files 
    ## these are ramdomly assigned to the various output directories (if multiple)

    
    if(!dir.exists(input_dir)){stop(paste("Cannot find directory",input_dir))}
    for(output in output_dir){# create directory if it does not exist
        if(!dir.exists(output)){dir.create(output)} 
    }
    files <- list.files(path=input_dir)
    exts  <- c(".blogs.txt",".news.txt",".twitter.txt")
    set.seed(iseed)
    for(fext in exts){
        files_in   <- grep(fext,files,value =TRUE)
        if(length(files_in)==0){stop(paste(fext,": no match in directory",input_dir))}
        file_in    <- paste0(input_dir,"/",gsub("/","",files_in[1]))
        ## read the whole file
        inpcon     <- file(file_in,open='r') # open connection
        input_text <- readLines(inpcon)  # read the full file
        close(inpcon) # close connection
        ## First select a fraction ratio of the text
        select <- rbinom(length(input_text),1,prob=ratio)
        nsamp  <- length(output_dir)
        if(nsamp >1){
            selected <- select==1
            select[selected] <- sample(seq(1,nsamp),sum(selected),replace=TRUE)}
        for(i in seq(1,nsamp)){
            output_text = input_text[select==i]
            file_out <- paste0(output_dir[i],"/",gsub("/","",output_dir[1]),fext)
            outcon     = file(file_out,open="w")
            writeLines(output_text,outcon)
            close(outcon) }}}

mk_dir <- function(x){sprintf("sample_%3.3d",x)}
#output_dirs <- sapply(seq(1,100),mk_dir)
#sample_corpus('final/en_US/',output_dirs,101)

############################################################
## Loading dictionary
############################################################

# first build dictionnaries from the sample_1
if(!exists("dict_var")){
    if(!file.exists("dictionary_1.txt")){
        message("Preparing dictionaries")
        corp   <- mk_corpus("sample_1")
        n_wd   <- build_dict(corp) 
        save_dict("dictionary_1.txt")
    }else{
        message("Loading dictionaries")
        n_wd   <- load_dict("dictionary_1.txt")
    }
    message(paste("Nb of words: for input",n_wd[1],", for prediction",n_wd[2]))
} else{
    message(paste("Nb of words: for input",length(dict_var),", for prediction",length(dict_prd)))
}

############################################################
## Sampling vocabulary
############################################################

get_doc_type <- function(dtype){
    dtype     <- sub("^[^.]{1,}.","",dtype)
    dtype     <- sub(".txt$","",dtype)
    dtype}

sample_voc <- function(output=c("voc.csv","nwords.csv")){

    get_voc<- function(tdm){
        ## Extract fraction of stopwords and badwords
        ## from the TermDocumentMatrix
        stp    <- rownames(tdm) %in% my_stopwords
        bad    <- rownames(tdm) %in% bad_words
        indic  <- rownames(tdm) %in% dict_var
        oth    <- !(stp | bad) & indic
        out    <- ! indic
    
        fstp   <- colSums(tdm[stp,])/colSums(tdm)
        names(fstp) <- sapply(names(fstp),function(x){paste0("stp.",x)})
        fbad   <- colSums(tdm[bad,])/colSums(tdm)
        names(fbad) <- sapply(names(fbad),function(x){paste0("bad.",x)})
        foth   <- colSums(tdm[oth,])/colSums(tdm)
        names(foth) <- sapply(names(foth),function(x){paste0("oth.",x)})
        fout   <- colSums(tdm[out,])/colSums(tdm)
        names(fout) <- sapply(names(fout),function(x){paste0("out.",x)})
        weights <- c(sum(stp),sum(oth & indic),sum(bad),sum(!indic))
        names(weights) <- c("wstp","woth","wbad","wout")
        as.matrix(c(weights,fstp,foth,fbad,fout))}

    voc        <- NULL
    nwords <- NULL
    for(i in seq(1,100)){
        corp   <- mk_corpus(sprintf("sample_%3.3d",i))
        tdm    <- as.matrix(TermDocumentMatrix(corp,control=list(wordLengths=c(1,Inf))))
        tdm["<stop>",] <- rep(0,ncol(tdm))
        colnames(tdm)  <- get_doc_type(colnames(tdm))
        qfreq  <- rowSums(tdm)    
        tdm    <- cbind(tdm,"all"=qfreq)
        voc    <- rbind(voc,t(get_voc(tdm)))
        rmstp  <- !(rownames(tdm) %in% my_stopwords)
        all50  <- sum(select_term(qfreq,fcomp=0.5))
        nonstop50  <- sum(select_term(qfreq[rmstp],fcomp=0.5))
        all90  <- sum(select_term(qfreq,fcomp=0.9))
        nonstop90  <- sum(select_term(qfreq[rmstp],fcomp=0.9))
        nwords <- rbind(nwords,data.frame(
            "all50"=all50,"nonstop50"=nonstop50,
            "all90"=all90,"nonstop90"=nonstop90))
        if(i %% 10 == 0){message(paste0("Done ",i,"%"))}}
    write.csv(voc,file=output[1],row.names=FALSE)
    write.csv(nwords,file=output[2],row.names=FALSE)
}

############################################################
## Sampling frequency variations
############################################################

top_words <- function(dir_corpus,output="topwords.txt"){
    ## This routine compute a list of words from a corpus
    ## Are included:
    ## 10 most frequent stop words
    ## 10 less frequent stop words
    ## 10 most frequent non  stop words
    corp   <- mk_corpus(dir_corpus)
    corp   <- tm_map(corp,content_transformer(apply_dictionary))
    tdm    <- as.matrix(TermDocumentMatrix(corp,control=list(wordLengths=c(1,Inf))))
    # exclude "." and "*" from top words
    tdm["<stop>",] <- rep(0,ncol(tdm))
    tdm["<naw>",] <- rep(0,ncol(tdm))
    qfreq     <- rowSums(tdm)
    qfreq     <- qfreq/sum(qfreq)    
    qfreq     <- sort(qfreq[qfreq>1E-4],decreasing=TRUE)
    stp       <- qfreq[names(qfreq) %in% my_stopwords]
    nstp      <- length(stp)
    nor       <- qfreq[!(names(qfreq) %in% my_stopwords)]
    topwords  <-  c(names(stp)[1:10],names(stp)[seq(nstp-9,nstp)], names(nor)[1:10])
    print(qfreq[topwords])
    if(is.character(output)){
        write.table(paste(topwords,collapse=" "),output,col.names=FALSE,row.names=FALSE,quote=FALSE)}
    topwords}

load_topwords <- function(input){
    topt  <- file(input,"r")
    lines <- readLines(topt)
    close(topt)
    unlist(strsplit(lines[1]," "))
}

stat_topten <- function(output="topten.csv",topdir="sample_1"){
    ## Generate a data frame with the frequency count of selected words 
    if(!exists("topwords")){
        message(paste("Preparing word list from",topdir))
        topwords <- top_words(topdir)
    }
    topten   <- NULL    
    message(paste("Words:",paste(topwords,collapse=" ")))
    message("Scanning samples")
    for(i in seq(1,100)){
        corp  <- mk_corpus(sprintf("sample_%3.3d",i))
        tdm   <- as.matrix(TermDocumentMatrix(corp,control=list(wordLengths=c(1,Inf))))
        tdm["<stop>",] <- rep(0,ncol(tdm))
        if(sum(!(topwords %in% row.names(tdm)))){
            warning(paste("Missing top words",topwords[!(topwords %in% Terms(tdm))]))
            for(mw in topwords[!(topwords %in% Terms(tdm))]){
                tdm <- rbind(tdm,matrix(rep(0.1,3),nrow=1,dimnames=list(mw,NULL)))}}
        qfreq  <- rowSums(tdm)
        qfreq  <- qfreq/sum(qfreq)
        topten <- rbind(topten,qfreq[topwords])
        if(i %% 10 == 0){message(paste0("Done ",i,"%"))}
    }
    topten <- data.frame(topten,check.names=FALSE)
    if(is.character(output)){write.csv(topten,file=output,row.names=FALSE)}
}

############################################################
## Sampling nb of n grams
############################################################

count_ngrams<-function(use_dict=TRUE,rm_stopwords=FALSE,splitsentences=TRUE){

    ng_count <- function(corp,ng=ng,splitsentences=TRUE){
        ## return the number of n-gram of lenght `ng` in corpus `corp`
        ng_tok <- NgramTokenizer(ngr=ng)
        tdm <- TermDocumentMatrix(corp,control=list(tokenizer=ng_tok,wordLengths=c(1,Inf)))
        ngterms <- Terms(tdm)
        if(splitsentences){
            sum(select_ngrams(ngterms))
        }else{
        nTerms(tdm)}}

    nngram <- NULL
    tngram <- NULL
    listg=seq(2,10)
    for(i in seq(1,100)){
        message(sprintf("sample_%3.3d",i))
        corp    <- mk_corpus(sprintf("sample_%3.3d",i),rm_stopwords=rm_stopwords)
        if(use_dict){corp    <- tm_map(corp,content_transformer(apply_dictionary))}
        nng_loc <- NULL
        tng_loc <- NULL
        for(ng in listg){
            ## message(paste0("Searching ",ng,"-grams"))
            timer <- system.time(n_loc <- ng_count(corp,ng=ng,splitsentences=splitsentences))
            nng_loc <- cbind(nng_loc,n_loc)
            t_loc   <- summary(timer)[1]
            tng_loc <- cbind(tng_loc,t_loc)
        }
        if(i %% 10 == 0){message(paste0("Done ",i,"%"))}
        colnames(nng_loc) <- listg
        colnames(tng_loc) <- listg
        nngram <- rbind(nngram,nng_loc)
        tngram <- rbind(tngram,tng_loc)
    }
    if(splitsentences){
        filen <- "scut_nngram"
        filet <- "scut_tngram"
    }else{
        filen <- "ncut_nngram"
        filet <- "ncut_tngram"
    }
        
    if(use_dict){
        if(rm_stopwords){
            filen <- paste0(filen,"_fdicstp.csv")
            filet <- paste0(filet,"_fdicstp.csv")
        }else{
            filen <- paste0(filen,"_fdic.csv")
            filet <- paste0(filet,"_fdic.csv")
        }
    }else{
        if(rm_stopwords){
            filen <- paste0(filen,"_fstp.csv")
            filet <- paste0(filet,"_fstp.csv")
        }else{
            filen <- paste0(filen,".csv")
            filet <- paste0(filet,".csv")
        }
    }
    write.csv(nngram,filen,row.names=FALSE)
    write.csv(tngram,filet,row.names=FALSE)
}

test_apply_dict <- function(dir){
    corpa <- mk_corpus(dir)
    corpb <- tm_map(corpa,content_transformer(apply_dictionary))
    listg=seq(2,10)
    for(ng in listg){
        ng_tok <- NgramTokenizer(ngr=ng)
        message(paste0("Testing for ",ng,"-grams"))
        tdma <- TermDocumentMatrix(corpa,control=list(tokenizer=ng_tok,wordLengths=c(1,Inf)))
        fa   <- rowSums(as.matrix(tdma))
        #fa   <- fa[select_ngrams(names(fa))]
        fa   <- sort(fa,decreasing=TRUE)
        tdmb <- TermDocumentMatrix(corpb,control=list(tokenizer=ng_tok,wordLengths=c(1,Inf)))
        fb   <- rowSums(as.matrix(tdmb))
        #fb   <- fb[select_ngrams(names(fb))]
        fb   <- sort(fb,decreasing=TRUE)
        message(paste("nb of ngrams A:",length(fa),"B:",length(fb)))
        message(paste("nb of ngrams A:",sum(select_ngrams(names(fa))),"B:",sum(select_ngrams(names(fb)))))  
    #    print(head(data.frame("A"=fa)))
    #    print(head(data.frame("B"=fb)))
    }}
