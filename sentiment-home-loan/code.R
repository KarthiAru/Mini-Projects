setwd("~/Documents/Projects/Projects/sentiment-home-loan")
source('helper.R')
library(stringi)
library(stringr)
library(sentimentr)
library(ngram)
library(ggplot2)
library(cowplot)

#load data
df <- read.csv('bb_reviews.csv')
df$date <- as.Date(df$date, '%b%d,%Y')
#remove all punctuation except EOL characters
df$review <- stri_replace_all(df$review, regex = "(?<=[!/,-.:;#?])[!/,-.:;#?]+|<+[^a-zA-Z0-9]", replacement = " ")
#get sentiment score
x <- sentiment_by(df$review)
df$sentiment <- x$ave_sentiment

#bank related keywords to be ignored
bank_words <- c('bank', 'allahabad', 'andhra', 'axis', 'bajaj', 'finserv',
                'baroda', 'maharashtra', 'canara', 'canfin', 'citi', 'central',
                'india', 'corporation', 'dena', 'dhfl', 'federal', 'gic', 'hdfc',
                'hsbc', 'icici', 'idbi', 'indiabulls', 'karur', 'vysya', 'kotak', 'mahindra', 'lic',
                'oriental', 'pnb', 'punjab', 'national', 'chartered', 'sbi', 'state', 'bikaner',
                'jaipur', 'hyderabad', 'mysore', 'sundaram', 'syndicate', 'uco', 'union', 'united')

#split data into +ve and -ve reviews based on sentiment score
pos_reviews <- df[df$sentiment>0,]$review
neg_reviews <- df[df$sentiment<0,]$review

#get ngrams
neg_gram1 <- top_words(neg_reviews,1)
neg_gram2 <- top_words(neg_reviews,2)
neg_gram3 <- top_words(neg_reviews,3)
pos_gram1 <- top_words(pos_reviews,1)
pos_gram2 <- top_words(pos_reviews,2)
pos_gram3 <- top_words(pos_reviews,3)

#Aggregate Analysis
png(file=sprintf('gram1.png'), width=1280, height=720)
plot_gram(neg_gram1,pos_gram1,1)
dev.off()
png(file=sprintf('gram2.png'), width=1280, height=720)
plot_gram(neg_gram2,pos_gram2,2)
dev.off()
png(file=sprintf('gram3.png'), width=1280, height=720)
plot_gram(neg_gram3,pos_gram3,3)
dev.off()

#Individual Bank Analysis

#plot the ngrams for individual banks
gram <- bank_gram(df,'AXIS BANK','bangalore',2)
png(file=sprintf('%s-%s.png','axis','bangalore'), width=1280, height=720)
plot_gram(gram[[1]],gram[[2]],1)
dev.off()

gram <- bank_gram(df,'AXIS BANK','chennai',2)
png(file=sprintf('%s-%s.png','axis','chennai'), width=1280, height=720)
plot_gram(gram[[1]],gram[[2]],1)
dev.off()

gram <- bank_gram(df,'HDFC LIMITED','bangalore',2)
png(file=sprintf('%s-%s.png','hdfc','bangalore'), width=1280, height=720)
plot_gram(gram[[1]],gram[[2]],1)
dev.off()