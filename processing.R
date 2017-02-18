library(tm)
library(stringr)

#=========================
# Global variables
#=========================

short_forms <- data.frame(
    "sub"=c("'d[^a-z]","'s[^a-z]"),
    "rep"=c(" 'd "," 's "))
short_forms  <- rbind(short_forms,data.frame(
    "sub"=c("'ll[^a-z]","'re[^a-z]","'ve[^a-z]"),
    "rep"=c(" 'll "," 're "," 've ")))
my_stopwords <- c(stopwords("english"),sapply(short_forms$rep,str_trim))

# source https://en.wiktionary.org/wiki/Category:English_swear_words
bad_words =c("asshole","^ass$","bitch","bollock","^clit$","^cock$","^crap$","^cum$","^cunt",
    "^dick","doos","dork","douche","^fag$","^fart","fuck","gook","jerk","nigga","nigger",
    "omfg","^piss$","^puss$","^pussy","^queeb$","shit","squirt")

#=========================
# Text processing function
#=========================

## Remove web adress, email, unicode ...
remove_junk <- function(inp){
    x <- inp
    x <- gsub("<U\\+[0-9a-fA-F]{1,}>"," ",x)
    x <- gsub("[^ ]{1,}@[^ ]{1,}"," ",x)
    x <- gsub(" @[^ ]{1,}"," ",x)
    x <- gsub("#[^ ]{1,}"," ",x)
    x <- gsub("[^ ]{1,}://[^ ]{1,}"," ",x)
    x <- gsub("www.[^ ]{1,}"," ",x)
    x}
## remove al symbols
remove_symbols <- function(inp){
    inpc <- inp
    x <- edit_apostrophe(inpc)
    x <- gsub("[^a-z'!.?]"," ",x)
    x <- gsub(" '{1,} | '{1,}|'{1,} "," ",x)
    x <- gsub("[.!?]'{1,}[.!?]|[.!?]'{1,}|'{1,}[.!?]",".",x)
    x}
## Edit entries so that it starts with "a-z" and
## ends with "[a-z] <stop>"
edit_line <- function(inp){
    x <- inp
    x <- gsub("^[^a-z]{1,}","",x) 
    x <- gsub("( [^a-z]{1,}){0,}$"," <stop>",x)
    x}
## flag ".", "!" or "?" with <stop>
edit_punctuation <- function(inp){
    gsub("( {0,}[.!?] {0,}){1,}"," <stop> ",inp)}
## make sure apostrophe is "'"
edit_apostrophe <- function(inp){ 
    x <- inp
    x <- gsub("[`’‘]{1,}","'",x)
    x}
## add a space before selected shortforms
split_shortforms <- function(inp){
    x <- inp
    for(isf in seq(1,nrow(short_forms))){
        x <- gsub(short_forms[isf,"sub"],short_forms[isf,"rep"],x)}
    x}
## Replace any word out of the dictionary with "<naw>"
## Edit repetion of unknown words "<naw>"
apply_dictionary <- function(inp){
    wdx <- unlist(strsplit(inp," "))
    wdx[!(wdx %in% c(dict_var,"<stop>",""))] <- "<naw>"
    x <- paste(wdx,collapse=" ")
    x <- gsub("( {0,}<naw> {0,}){1,}"," <naw> ",x)
    x}

#===============================
# Text processing function calls
#===============================

## Apply text processing to a corpus after reading it from a directory
mk_corpus <- function(dir,rm_stopwords=FALSE,lang="english",verbose=FALSE){
    ## Takes a directory of text document as input
    ## return a preprocessed corpus
    corp_loc <- VCorpus(DirSource(dir))
    ## Apply Preprocessing
    if(verbose){message("Remove junk")}
    corp_loc <- tm_map(corp_loc,content_transformer(remove_junk))
    if(verbose){message("Switch to lower cases")}
    corp_loc <- tm_map(corp_loc,content_transformer(tolower))
    if(lang=="english"){
        if(verbose){message("Remove symbols")}
        corp_loc <- tm_map(corp_loc,content_transformer(remove_symbols))}
    if(verbose){message("Edit punctation")}
    corp_loc <- tm_map(corp_loc,content_transformer(edit_line))
    corp_loc <- tm_map(corp_loc,content_transformer(edit_punctuation))
    if(lang=="english"){
        if(verbose){message("split shortforms")}
        corp_loc <- tm_map(corp_loc,content_transformer(split_shortforms))
        if(rm_stopwords) {
            if(verbose){message("remove stopwords")}
            corp_loc <- tm_map(corp_loc,removeWords,my_stopwords)}
    }
    if(verbose){message("Strip white spaces")}
    tm_map(corp_loc,stripWhitespace)
    corp_loc}

## Apply text processing to a text input
process_text <- function(inp_str,rm_stopwords=FALSE,lang="english"){
    ## takes a character string as input
    ## return a character string woth preprocessing applyed
    print(inp_str)
    out_str <- remove_junk(inp_str)
    print(out_str)
    out_str <- tolower(out_str)
    print(out_str)
    if(lang=="english"){
        out_str  <- remove_symbols(out_str)
        print(out_str)
    }
    out_str <- edit_punctuation(out_str)
    print(out_str)
    if(lang=="english"){
        out_str  <- split_shortforms(out_str)
        print(out_str)
        if(rm_stopwords) {out_str <- removeWords(out_str,my_stopwords)}}
    out_str}

NgramTokenizer <- function(ngr=4){
    ## take a number as input
    ## return a tokenizing function for ngram
    if(ngr < 2){stop("In NgramTokenizer, can't build tokenizer with less than 2 words")}
    return(function(x){unlist(lapply(ngrams(words(x),ngr), paste, collapse = " "), use.names = FALSE)})}

select_ngrams <- function(ngrams){
    ## identify ngrams spaning sentences
    cross_sent <- grepl("(<naw>|[a-z']{1,}) <stop>",ngrams)
    ## identify ngrams ending a snetance or finishing with no known word
    no_predict <- grepl("(<naw>|<stop>)$",ngrams)
    ## Return boolean array with TRUE if none of these conditions is fullfiled
    !(cross_sent | no_predict)}
