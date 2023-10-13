library(quanteda)
library(dplyr)
library(tm)
library(stringr)
library(tidyverse)

#read .rds files
n_2gram <- readRDS("n_2gram.rds")
n_3gram <- readRDS("n_3gram.rds")
n_4gram <- readRDS("n_4gram.rds")
n_5gram <- readRDS("n_5gram.rds")


clean_data<-function(text){
        
        cleanText <- tolower(text)
        cleanText <- removePunctuation(cleanText)
        cleanText <- removeNumbers(cleanText)
        cleanText <- str_replace_all(cleanText, "[^[:alnum:]]", " ")
        cleanText <- stripWhitespace(cleanText)
        
        return(cleanText)
}

clean_input <- function(text){
        textInput <- unlist(strsplit(text, " "))
        textInput <- clean_data(textInput)
        
        return(textInput)
}

get_NgramTable <- function(input_ngram){
        
        slpit_ngrams <- strsplit(as.character(input_ngram$word)," ")
        input_ngram$nminusgram <- NA
        input_ngram$lastword <- NA
        for (i in 1:nrow(input_ngram)){
                new_table <- c()
                for (j in 1:(length(slpit_ngrams[[i]])-1)){
                        new_table <- c(new_table,slpit_ngrams[[i]][j])
                }  
                input_ngram$nminusgram[i] <- paste( new_table, collapse = " ") 
                input_ngram$lastword[i] <- tail(slpit_ngrams[[i]],1)
        }
        return(as.data.frame(input_ngram,row.names =NULL, stringsAsFactors=FALSE ))
}

twogramTable <- get_NgramTable(n_2gram)
threegramTable <- get_NgramTable(n_3gram)
fourgramTable <- get_NgramTable(n_4gram)
fivegramTable <- get_NgramTable(n_5gram)

get_word_count <- function(text){
        clean_text <- clean_data(text)
        text_input <- clean_input(clean_text)
        word_count2 <- length(text_input)
        return(word_count2)
}

get_prediction <- function(text) {
        wordcount <- get_word_count(text)
        clean_text <- clean_data(text)
        text_input <- clean_input(clean_text)
        
        input4<- paste(tail(unlist(strsplit(text_input,' ')),4), collapse=" ")
        input3<- paste(tail(unlist(strsplit(text_input,' ')),3), collapse=" ")
        input2<- paste(tail(unlist(strsplit(text_input,' ')),2), collapse=" ")
        input1<- paste(tail(unlist(strsplit(text_input,' ')),1), collapse=" ")
        
        
        if(wordcount >3){
                if(input4 %in% fivegramTable$nminusgram) {
                        df_possible <- fivegramTable %>% filter(nminusgram == input4) 
                        
                        #return(df_possible[1])
                }
                else if(input3 %in% fourgramTable$nminusgram){
                        df_possible <- fourgramTable %>% filter(nminusgram == input3) 
                        #return(df_possible[1])
                }
                else if(input2 %in% threegramTable$nminusgram){
                        df_possible <- threegramTable %>% filter(nminusgram == input2) 
                        #return(df_possible[1])
                }
                else if(input1 %in% twogramTable$nminusgram){
                        df_possible <- twogramTable %>% filter(nminusgram == input1) 
                        #return(df_possible[1])
                }else { return("the")}
                
                guess_1 <- as.character(df_possible$lastword[1])
                guess_2 <- as.character(df_possible$lastword[2])
                guess_3 <- as.character(df_possible$lastword[3])
                guess_4 <- as.character(df_possible$lastword[4])
                return(c(guess_1,guess_2,guess_3,guess_4))
                #else {return ("the")}
        }
        
        else if(wordcount ==3){
                if(input3 %in% fourgramTable$nminusgram) {
                        df_possible <- fourgramTable %>% filter(nminusgram == input3)
                        
                        #return(df_possible[1])
                }
                else if(input2 %in% threegramTable$nminusgram){
                        df_possible <- threegramTable %>% filter(nminusgram == input2)
                        #return(df_possible[1])
                }
                else if(input1 %in% twogramTable$nminusgram){
                        df_possible <- twogramTable %>% filter(nminusgram == input1) 
                        #return(df_possible[1])
                }else { return("the")}
                
                guess_1 <- as.character(df_possible$lastword[1])
                guess_2 <- as.character(df_possible$lastword[2])
                guess_3 <- as.character(df_possible$lastword[3])
                guess_4 <- as.character(df_possible$lastword[4])
                
                return(c(guess_1,guess_2,guess_3,guess_4))
                #else {return ("the")
        }
        
        else if(wordcount==2){
                if(input2 %in% threegramTable$nminusgram) {
                        df_possible <- threegramTable %>% filter(nminusgram == input2)
                        #return(df_possible[1])
                }
                else if(input1 %in% twogramTable$nminusgram){
                        df_possible <- twogramTable %>% filter(nminusgram == input1) 
                        #return(df_possible[1])
                } else { return("wrong input")}
                guess_1 <- as.character(df_possible$lastword[1])
                guess_2 <- as.character(df_possible$lastword[2])
                guess_3 <- as.character(df_possible$lastword[3])
                guess_4 <- as.character(df_possible$lastword[4])
                
                return(c(guess_1,guess_2,guess_3,guess_4))
                #else {return ("the")
        }
        
      else if(wordcount==1){
                if(input1 %in% twogramTable$nminusgram) {
                        df_possible <- twogramTable %>% filter(nminusgram == input1)
                } else { return("the")}
                #df_possible <- twogramTable[twogramTable$nminusgram[1] == input1[1],]
                guess_1 <- as.character(df_possible$lastword[1])
                guess_2 <- as.character(df_possible$lastword[2])
                guess_3 <- as.character(df_possible$lastword[3])
                guess_4 <- as.character(df_possible$lastword[4])
                
                #guess_2 <- as.character(df_possible$twogramTable[2]== twogramTable$lastword[2])
                
                return(c(guess_1,guess_2,guess_3, guess_4))
               # 
        } else {return(c(guess_1="Wrong Input", guess_2='', guess_3="", guess_4=""))}
}

        