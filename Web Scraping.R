#Libraries
library(tidyverse)
library(rvest)
library(xml2)
library(dplyr)
library(lubridate)

#URL
website <- "https://www.imdb.com/search/keyword/?ref_=kw_ref_gnr&mode=detail&page=1&sort=num_votes,desc&genres=Action"
page <- read_html(website)

#Movie Title
Movie_Name <-  page %>% 
  html_nodes(".lister-item-header a") %>%
  html_text() 

#Movie Year
Movie_Year <- page %>% 
  html_nodes(".lister-item-content h3 .lister-item-year") %>%
  html_text() %>%
str_sub(start = 2, end = 5) %>%
  as.Date(format = "%Y") %>%
  year() -> Movie_Year

#Movie Gross
Movie_Gross <- page %>%
  html_nodes(".ghost~ .text-muted+ span") %>%
  html_text()
  
#Replacing Missing Values woth NA  
for (i in c(3)){
  a<-Movie_Gross[1:(i-1)]
  b<-Movie_Gross[i:length(Movie_Gross)]
  Movie_Gross<-append(a,list("NA"))
  Movie_Gross<-append(Movie_Gross,b)
}
Movie_Gross<-as.character(Movie_Gross)

#Movie Length
Movie_length <- page %>%
  html_nodes(".runtime") %>%
  html_text()
Movie_length<-gsub("min","",Movie_length)

#Movie Rating
Movie_Rating <- page %>%
  html_nodes(".ratings-imdb-rating strong") %>%
  html_text() %>% as.numeric()

#Movie Summary
Movie_summary <- page %>%
  html_nodes(".ratings-bar+ p") %>%
  html_text()

##Movie Metascore
Movie_Metascore <- page %>%
  html_nodes(".metascore") %>%
  html_text()
Movie_Metascore<-gsub(" ","",Movie_Metascore)
for (j in c(3)){
  x<-Movie_Metascore[1:(j-1)]
  y<-Movie_Metascore[j:length(Movie_Metascore)]
  Movie_Metascore<-append(x,list("NA"))
  Movie_Metascore<-append(Movie_Metascore,y)
  Movie_Metascore<-as.numeric(Movie_Metascore)
}

#Movie Votes
Movie_Votes <- html_nodes(page,
                    '.text-muted+ span:nth-child(2)') %>% 
  html_text()
Movie_Votes<- gsub(",", "", 
              Movie_Votes) %>% as.numeric()
Movie_Votes = na.omit(Movie_Votes)

#Movie Link
Movie_links = page %>% html_nodes(".lister-item-header a") %>%
  html_attr("href") %>% paste("https://www.imdb.com", ., sep="")

#Scrape data set.
Action_movies = data.frame( Movie_Name,  Movie_length, Movie_Rating,  Movie_summary, Movie_Gross, Movie_Metascore, Movie_Year, Movie_Votes, Movie_links,stringsAsFactors = FALSE)

# Relationship between rating and number of votes according to IMDb.com data
Movies <- Action_movies %>% group_by(Movie_Name) %>% summarise(Movie_Rating, Movie_Votes, Movie_Gross, Movie_Metascore)
Movies %>% 
  ggplot(aes(x = reorder(Movie_Name, Movie_Votes), y = Movie_Votes, fill = Movie_Rating)) +
  geom_histogram(stat = "identity") +
  scale_fill_distiller(name="Rating", palette = "Set1") +
  scale_y_continuous(labels = scales::comma) + 
  coord_flip() +
  xlab("Action Movies") + ylab("Total number of votes") + 
  ggtitle("Relationship between rating and number of votes according to IMDd.com data")

#Relationship between Gross Earnings and Metascore according to IMDd.com data
Movies %>% 
  ggplot(aes(x = reorder(Movie_Name, Movie_Gross), y = Movie_Gross, fill = Movie_Metascore)) +
  geom_histogram(stat = "identity") +
  scale_fill_distiller(name="Metascore", palette = "blues") +
  theme(axis.text.x = element_text(angle = 45))+
  xlab("Action Movies") + ylab("Gross Earnings per Movie") + 
  ggtitle("Relationship between Gross Earnings and Metascore according to IMDd.com data")

#Movies with the highest rating
Movies %>% 
  ggplot(aes(x = reorder(Movie_Name, Movie_Rating), y = Movie_Rating, fill = Movie_Metascore)) +
  geom_histogram(stat = "identity") +
  scale_fill_distiller(name="Metascore", palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45))+
  xlab("Action Movies") + ylab("Movie rating") + 
  ggtitle("Movies with the highest rating")

#Movies with the highest Metascore
Movies %>% 
  ggplot(aes(x = reorder(Movie_Name, Movie_Metascore), y = Movie_Metascore, fill = Movie_Rating)) +
  geom_histogram(stat = "identity") +
  scale_fill_distiller(name="Movie Rating", palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45))+
  coord_flip() +
  xlab("Action Movies") + ylab("Movie rating") + 
  ggtitle("Movies with the highest Metascore")

#End Program





