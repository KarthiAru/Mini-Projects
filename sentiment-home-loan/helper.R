#function to get top 25 ngrams
top_words <- function(text,n){
    x <- tolower(text)
    x <- unlist(stri_split(x, regex = "(?<!\\w\\.\\w.)(?<![A-Z][a-z]\\.)(?<=[!?.])\\s", omit_empty=T))
    x <- stri_replace_all(x, fixed=".", replacement = "")
    words <- strsplit(x, " ")
    words <- lapply(words, function(y) y[!y %in% c(bank_words,stopwords())])
    txt <- unlist(lapply(words, function(x) paste(x,collapse = " ")))
    txt <- txt[unlist(lapply(txt, function(y) word_count(y)>=4))]
    txt <- txt[!is.na(txt)]
    x <- get.phrasetable(ngram(txt,n))
    return(x[1:15,])
}

#function to plot ngrams
plot_gram <- function(neg_gram,pos_gram,n){
    p1 <- ggplot(neg_gram, aes(x = reorder(ngrams, freq), y = freq)) +
        geom_bar(stat="identity") + 
        coord_flip() +
        labs(title = '-ve reviews' ,y = "Frequency", x = '') +
        theme(axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15))
    p2 <- ggplot(pos_gram, aes(x = reorder(ngrams, freq), y = freq)) +
        geom_bar(stat="identity") + 
        coord_flip() +
        labs(title = '+ve reviews' ,y = "Frequency", x = '') +
        theme(axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 15))
    #combine both the plots together
    p <- plot_grid(p1, p2, ncol=2)
    p <- ggdraw(p) + draw_label(sprintf('%d-gram',n), 0.05, 0.98, fontface='bold')
    return(p)
}

#function to plot plot n-grams for bank-city
bank_gram <- function(frame,bank,city,n){
    x <- frame[which(frame$bank==bank & frame$city==city),]
    #split data into +ve and -ve reviews based on sentiment score
    pos_reviews <- x[x$sentiment>0,]$review
    neg_reviews <- x[x$sentiment<0,]$review
    neg_gram <- top_words(neg_reviews,2)
    pos_gram <- top_words(pos_reviews,2)
    return(list(neg_gram,pos_gram))
}