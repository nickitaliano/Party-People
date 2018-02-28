# Author:Nick | namii
# Date: 06/21/2017
# Problem:
# Solution:Script scrapes essential core music for mobile djs
# Description:
# Objective:
# Domain/Tools:
# I/O:
# Results/Benefit:
# Publish/Blog:
# Acknowledgements: "Jasev/Teachers" and "Mentors/Code"
# More Info:
# Contact:


# Genre-based recomendation implementing last FM API collaborative filtering 
library(RLastFM)
install.packages("rlist")
install.packages("rvest")
install.packages("plyr")
install.packages("jsonlite")

library("rlist")
library("rvest")
library("plyr")
library("jsonlite")

if (!require(XML)) {install.packages('XML');require(XML)}
if (!require(RCurl)) {install.packages('RCurl');require(RCurl)}
if (!require(knitr)) {install.packages('knitr');require(knitr)}
if (!require(recommenderlab)) {install.packages('recommenderlab');require(recommenderlab)}
if (!require(jsonlite)) {install.packages('jsonlite');require(jsonlite)}
if (!require(plyr)) {install.packages('plyr');require(plyr)}
if (!require(tidyr)) {install.packages('tidyr');require(tidyr)}
if (!require(RMySQL)) {install.packages('RMySQL');require(RMySQL)}
if (!require(getPass)) {install.packages('getPass');require(getPass)}

# manual install of RLastFM package
if (!require(RLastFM)) {
  download.file('http://cran.r-project.org/src/contrib/Archive/RLastFM/RLastFM_0.1-5.tar.gz','RLastFM_0.1-5.tar.gz')
  install.packages('RLastFM_0.1-5.tar.gz', repos = NULL, type = "source")
  require(RLastFM)
}

#NOTE: install RLastFM_0.1-5.tar.gz from CMD line; go to directory 
# you have package and type R CMD"

#lsp function return names of all functions in pacakge
lsp <- function(package, all.names = FALSE, pattern)
{
  package <- deparse(substitute(package))
  ls(
    pos = paste("package", package, sep = ":"),
    all.names = all.names,
    pattern = pattern
  )
}

lsp(RSpotify)
lsp(dplyr)
lsp(stringr)
lsp(base, TRUE)
lsp(base, pattern = "^is")

# LastFM API test
my_key <- "your lastfm API key"

artInfo=artist.getInfo(artist = "Porter Robinson" )# Data points of favorite artists or genre
simInfo=artist.getSimilar(artist = "Porter Robinson" )

# Genre-based Music Reccomendation 
create_artist_query_url_lfm <- function(artist_name){
  prefix <- "http://ws.audioscrobbler.com/2.0/?method=artist.gettoptags&artist="
  postfix <- "&api_key=ddf1896b9d1f7a03bbb51d55ea509876&format=json"
  encoded_artist <- URLencode(artist_name)
  return(paste0(prefix, encoded_artist, postfix))
}

# Obtain names and genre tags of top US artists from meta
artist_df<-geo.getTopArtists("United States")

an_artist<-data.frame(
  lNames = rep(names(artist_df), lapply(artist_df, length)),
  lVal = unlist(artist_df))

an_artist$lVal<-as.character(an_artist$lVal)
artist_name<-an_artist$lVal[1:50]

# Create function to call API for tags"i.e,{{electronic}:{jazz}:{indie}:{folk}:{rock}:{pop}}"...)
get_tag_frame_lfm <- function(an_artist){
  print(paste0("Attempting to fetch: ", an_artist))
  artist_url <- create_artist_query_url_lfm(an_artist)
  json <- fromJSON(artist_url)
  return(as.vector(json$toptags$tag[,"name"]))
}


# Use (httr) to handle multiple API fetches and (memoise) cache data within functions
if (!require(httr)) {install.packages('httr');require(httr)}
if (!require('memoise')) {install.packages('memoise');require(memoise)}
library(memoise)

# Apply memoised tag function over US artists list
mem_get_tag_frame_lfm <- memoise(get_tag_frame_lfm)
ARTIST_LIST<-as.list(artist_name)
ARTIST_LIST

artists_tags <- sapply(ARTIST_LIST, mem_get_tag_frame_lfm)
artists_tags
names(artists_tags) <- ARTIST_LIST


# Compute Jaccard Index (similarity/dissimilarity measure for the data) by determining distance
# between artists with similiar tag lengths
cmbs <- combn(ARTIST_LIST, 2)
comparisons <- data.frame(t(cmbs))

jaccard_index <- function(tags1, tags2){
  length(intersect(tags1, tags2))/length(union(tags1, tags2))
}

comparisons$similarity <- apply(comparisons, 1,
                                function(arow){
                                  jaccard_index(artists_tags[[unlist(arow[1])]],
                                                artists_tags[[unlist(arow[2])]])
                                }) 
names(comparisons) <- c("artist1", "artist2","similarity")

# Create function to return top 3 similiar artists by genre (filter comparisons)
get_top_n <- function(comparisons, N, artist, threshold){
  comparisons %<>%
    filter(artist1==artist | artist2==artist) %>%
    arrange(desc(similarity))
  other_artist <- ifelse(comparisons$similarity>threshold,
                         ifelse(comparisons$artist1==artist,
                                comparisons$artist2, comparisons$artist1),
                         "None")
  return(other_artist[1:N])
}

if (!require('dplyr')) {install.packages('dplyr');require(dplyr)}


# Create data frame "name,group,first,second,third" attributes to create JSON object
nodes <- sapply(ARTIST_LIST, function(x) get_top_n(comparisons, 3, x, 0.25))
nodes <- data.frame(t(nodes))
names(nodes) <- c("first", "second", "third")
nodes$name <- row.names(nodes)
row.names(nodes) <- NULL
nodes$group <- 1

# find the 0-indexed index
lookup_number <- function(name) which(name==ARTIST_LIST)-1

strong_links <- comparisons %>%
  filter(similarity > 0.25) %>%
  rename(node1 = artist1, node2 = artist2, weight=similarity)
strong_links$source <- sapply(strong_links$node1, lookup_number)
strong_links$target <- sapply(strong_links$node2, lookup_number)

object <- list("nodes"=nodes,
               "links"=strong_links)

# Create JSON object
sink("us_artists.json")
toJSON(object, dataframe="rows", pretty=TRUE)
sink()


