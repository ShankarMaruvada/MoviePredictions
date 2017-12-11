install.packages("devtools")
library(devtools)
install_github("Rfacebook", "pablobarbera", subdir="Rfacebook")
require("Rfacebook")
library(ggplot2)
library(NLP)
library(tm)
library(RColorBrewer)
library(syuzhet)


help("getPage")

token <- 'EAACEdEose0cBAB04Xcat0mVwOYxPKgU2KZCoHNCKT2XsODsu8qpNtYnjIootIJWyzEm5OJhDhlZAtcfpdoWLp7oMBVVRzFhteIinlDGnGzvxMjfmqZCkZC5ZBPr3mWflrRkEDzmwNA8B2myilgrGBdTpplxuZB5YsjzzB4WhjCp39gWngEpvcRNjMzSc2AI2QZD'

movie_page <- getPage("MeBeforeYouMovie", token, n=3000, until = "2016-06-03")

#movie_page <- getPage("Captain.America.Civil.War.Film", token, n=3000)

View(movie_page)


for (i in 1:nrow(movie_page)){
        movie_post <- getPost(movie_page$id[i], token, n=20000)
        movie_post <- as.data.frame(movie_post$comments)
        if((nrow(movie_post)) >= 1){
                movie_post$postid <- movie_page$id[i]
        }
        if (i == 1){
                movie_post_comments <- movie_post
        }
        else{
                movie_post_comments <- rbind(movie_post_comments,movie_post)
        }
}

movie_post_msg <- movie_page$message

movie_post_msg <- as.data.frame(movie_post_msg)

names(movie_post_msg) <- c('messages')

movie_comments_msg <- movie_post_comments$message


movie_comments_msg <- as.data.frame(movie_comments_msg)

names(movie_comments_msg) <- c('messages')

rm(movie_text)

movie_text <- rbind(movie_post_msg,movie_comments_msg)


## Need to change the CSV name for each movie, I am keeping the movie name as CSV so that we can track.
write.csv(movie_text, file = 'MeBeforeYouMovie.csv')


r_text_corpus <- Corpus(VectorSource(movie_text$messages))

r_text_cleansing <- r_text_corpus

r_text_cleansing <- tm_map(r_text_corpus, stripWhitespace)

r_text_cleansing <- tm_map(r_text_cleansing, removeNumbers)


r_text_cleansing <- tm_map(r_text_cleansing, removePunctuation)


r_text_cleansing <- tm_map(r_text_cleansing, content_transformer(tolower))

#install.packages("syuzhet")


isNull <- function(data) {
        if(is.null(data))
                return(0)
        else
                return(data)
}

text_vec = c()
anger = c() ; anticipation=c() ; disgust=c() ; fear=c() ; joy=c() ;

sadness=c() ; surprise=c() ; trust=c() ; nrc_negative=c() ; nrc_positive=c();


for(i in 1:length(r_text_cleansing)){
        text <- lapply(r_text_cleansing[i], as.character)
        text <- gsub("http\\w+", "", text)
        nrc_emotions <- get_nrc_sentiment(text)
        
        text_vec[i] <- text
        anger[i] <- isNull(nrc_emotions$anger)
        anticipation[i] <- isNull(nrc_emotions$anticipation)
        disgust[i] <- isNull(nrc_emotions$disgust)
        fear[i] <- isNull(nrc_emotions$fear)
        joy[i] <- isNull(nrc_emotions$joy)
        sadness[i] <- isNull(nrc_emotions$sadness)
        surprise[i] <- isNull(nrc_emotions$surprise)
        trust[i] <- isNull(nrc_emotions$trust)
        nrc_negative[i] <- isNull(nrc_emotions$negative)
        nrc_positive[i] <- isNull(nrc_emotions$positive)
}


nrc_df <- data.frame(text_vec,anger,anticipation,disgust,fear,joy,sadness,surprise,
                     trust,nrc_negative,nrc_positive)

nrc_df[1:6,1:11]

par(mar=c(5.1,5,4.1,2.1))

sort(colSums(prop.table(nrc_df[, 2:9])))

sort(colSums(prop.table(nrc_df[, 10:11])))

#barplot(
#       sort(colSums(prop.table(nrc_df[, 2:9]))), 
#       horiz = TRUE, 
#       cex.names = 0.7,
#        las = 1, 
#        main = "Emotions", 
#        xlab="Percentage",
#        col="lightblue"
#)

#barplot(
#        sort(colSums(prop.table(nrc_df[, 10:11]))), 
#        horiz = TRUE, 
#        cex.names = 0.7,
#        las = 1, 
#        main = "Emotions", 
#        xlab="Percentage",
#        col="lightblue"
#)

#help("prop.table")

#write.csv(nrc_df, file = 'spectre_emotions.csv')

