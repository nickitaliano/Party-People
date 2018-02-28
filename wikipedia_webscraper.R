# HTML/XML web scraping in R for music genres, artists names, etc.

#Packages
install.packages("httr")
install.packages("XML")
install.packages("RCurl")
install.packages("rlist")
install.packages("rvest")
install.packages("plyr")
install.packages("dplyr")
install.packages("magrittr")

# Libraries
library("httr")
library("XML")
library("RCurl")
library("rlist")
library("rvest")
library("plyr")
library("dplyr")
library("magrittr")

# Scrape list of unique tracks from top 200 most requested party songs   
url <- "http://djeventplanner.com/mostrequested.htm"
table.all <- readHTMLTable(url) #attach 
all.df <- ldply(table.all, data.frame)
all.df$.id<-NULL
all.df$Rank<-NULL
Song.Artist <- all.df$Song
Song.Title <- all.df$Song.Title
scraped.data<-data.frame(Song.Title,Song.Artist)
app.data<-scraped.data[!duplicated(scraped.data$Song.Title),]
top200_artists<-as.character(app.data$Song.Artist)
row.names(app.data)<-NULL
write.table(app.data,"party_music.txt", sep="\t", row.names = FALSE)# Note: can export for firefox mp3 sub application...


# Scrape genre tags and artists from wikipedia 
path1<-"https://en.wikipedia.org/wiki/List_of_house_music_artists"
webpage1 <- getURL(path1)
webpage1 <- readLines(tc1 <- textConnection(webpage1)); close(tc1)
pagetree1 <- htmlTreeParse(webpage1, error=function(...){}, useInternalNodes = TRUE)

path2<-"https://en.wikipedia.org/wiki/List_of_electronic_music_genres"
webpage2 <- getURL(path2)
webpage2 <- readLines(tc2 <- textConnection(webpage2)); close(tc2)
pagetree2 <- htmlTreeParse(webpage2, error=function(...){}, useInternalNodes = TRUE)

# Extract table/list header and contents
clean.text <- function(x, lowercase=TRUE, numbers=TRUE, punctuation=TRUE, spaces=TRUE){
{
  # x: character string
  
  # # lower case
  # if (lowercase)
  #   x = tolower(x)
  # remove numbers
  # if (numbers)
  #   x = gsub("[[:punct:]]", "", x)
  # remove punctuation symbols
  if (punctuation)
    x = gsub("[[:punct:]]*[[:digit:]]*", "", x)
  # # remove extra white spaces
  # if (spaces) {
  #   x = gsub("[ \t]{2,}", " ", x)
  #   x = gsub("^\\s+|\\s+$", "", x)
  }
  # return
  x
}

# tablehead1<- xpathSApply(pagetree1, "//*//a[@href]", xmlValue) # links
tablehead1 <- xpathSApply(pagetree1, "//li", xmlValue)
artists<-clean.text(tablehead1[34:557])
formats<-tablehead1[571:610]
combined_artists<-c(artists, top200_artists)

#tablehead2 <- xpathSApply(pagetree2, "//*/table/tr", xmlValue)
tablehead2 <- xpathSApply(pagetree2, "//li", xmlValue)
genres<-tablehead2
genres<-strsplit(genres,"\n")
all_genres<-unlist(genres)
combined_genres<-c(all_genres,formats)
unique_genres<-all_genres[!duplicated(all_genres)]
types<-unique_genres[287:302]

# count number of elements in each
length(artists)
length(unique_genres)
length(formats)
length(types)


#write to .txt
write.table(combined_artists,"combined_artists.txt", sep="\t", row.names = FALSE)# For word cloud and corpus
write.table(combined_genres,"combined_genres.txt", sep="\t", row.names = FALSE)# For word cloud and corpus

# Expand to many URL pages, page list links/nodes, and analyze/publish...


# theurl <- getURL("https://en.wikipedia.org/wiki/List_of_house_music_artists",.opts = list(ssl.verifypeer = FALSE) )
# tables <- readHTMLTable(theurl)
# tables <- list.clean(tables, fun = is.null, recursive = FALSE)
# n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
# 
# # rvest unstructered text "" 
# scraping_wiki <- read_html("https://en.wikipedia.org/wiki/List_of_house_music_artists")
# scraping_wiki %>%
#   html_nodes("h1")
# ## {xml_nodeset (1)}
# ## [1] <h1 id="firstHeading" class="firstHeading" lang="en">Web scraping</h1>
# 
# scraping_wiki %>%
#   html_nodes("h2") %>%
#   html_text()
# 
# p_nodes <- scraping_wiki %>% 
#   html_nodes("p")
# 
# length(p_nodes)
# p_nodes[1]
# 
# ul_text <- scraping_wiki %>%
#   html_nodes("ul") %>%
#   html_text()
# 
# length(ul_text)
# 
# ul_text[21]
# 
# li_text <- scraping_wiki %>%
#   html_nodes("li") %>%
#   html_text()
# 
# length(li_text)
# 
# 
# list_artist <-li_text[29:557]
# list_artist
# 
# li_text %>%
#   gsub(pattern = "[|]", replacement = " ") %>%
#   gsub(pattern = "\"", replacement = " ") %>%
#   gsub(pattern = "\\s+", replacement = " ")
# 
# scraping_wiki %>%
#   html_nodes("#mw-content-text > div:nth-child(3)") %>% 
#   html_text()
# 
# li_text2<-grep("[[:alnum:][:punct:]\\s]", li_text, value = TRUE)
# 
# url <- "https://en.wikipedia.org/wiki/List_of_house_music_artists"
# population <- url %>%
#   read_html("h1") %>%
#   html_nodes(xpath='//*[@id="mw-content-text"]/div/table[3]') %>%
#   html_table()
# population <- population[[1]]

# ToDo: Scrape referenced links...
# https://en.wikipedia.org/wiki/Lists_of_musicians
# https://en.wikipedia.org/wiki/List_of_electro_house_artists
# List of progressive house artists
# https://en.wikipedia.org/wiki/List_of_club_DJs


