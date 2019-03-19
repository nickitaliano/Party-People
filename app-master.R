rm(list = ls())

## genrify.R ##
# Import required libraries and data
if (!require(shinydashboard)) {install.packages('shinydashboard');require(shinydashboard)}
if (!require(shiny)) {install.packages('shiny');require(shiny)}
if (!require(networkD3)) {install.packages('networkD3');require(networkD3)}
if (!require(d3Network)) {install.packages('d3Network');require(d3Network)}
if (!require(jsonlite)) {install.packages('jsonlite');require(jsonlite)}
if (!require(shiny.semantic)) {install.packages('shiny.semantic');require(shiny.semantic)}
if (!require(dplyr)) {install.packages('dplyr');require(dplyr)}
if (!require(caret)) {install.packages('caret');require(caret)}
if (!require(highcharter)) {install.packages('highcharter');require(highcharter)}
if (!require(treemap)) {install.packages('treemap');require(treemap)}
if (!require(rlist)) {install.packages('rlist');require(rlist)}
if (!require(Rtsne)) {install.packages('Rtsne');require(Rtsne)}
if (!require(purrr)) {install.packages('purrr');require(purrr)}
if (!require(gridBase)) {install.packages('gridBase');require(gridBase)}



# Load analyses and app data
load("rtists_tags.RData")
res<-read_json("/Users/nami/Projects/rtist-recommendations-master/us_artists.json",simplifyVector = TRUE)
res_links<-as.data.frame(res$links)
res_nodes<-as.data.frame(res$nodes)
##server##
server <- function(input, output) {
  
  # Add artists top tags (lastfm)
  i<-0
  for(i in 1:length(artists_tags)) {
    
    genre_tag1<-artists_tags[[i]][1]
    genre_tag2<-artists_tags[[i]][2]
    
    res_nodes$genre1[i]<-genre_tag1
    res_nodes$genre2[i]<-genre_tag2
    
    i<-i+1
  }
  
  
  # distance between probabilities...
  #assign color to genre after validation(and analysis)
  # Rename a column in R
  res_nodes$genre1<- gsub("'", "", res_nodes$genre1) %>% tolower()
  res_nodes$genre2<- gsub("'", "", res_nodes$genre2) %>% tolower()
  colnames(res_nodes)[colnames(res_nodes)=="group"] <- "genre"
  
  res_nodes1 <- res_nodes[-5]


#names(links)[1]<-"value"
# result<-list(links,nodes)
# names(result)<-c("links","nodes")
# links$source=as.numeric(as.factor(links$source))-1
# links$target=as.numeric(as.factor(links$target))-1

# match color of network
links<-select(res_links,-1:-2)
nodes<-select(res_nodes1,-1:-3)  

# with a simple click action - make the circles bigger when clicked
MyClickScript <- 'd3.select(this).select("circle").transition().duration(500).attr("r", 40)'

output$force <- renderForceNetwork({
  forceNetwork(Links = links, Nodes = nodes, Source = "source",
               Target = "target", Value = "weight", NodeID = "name",
               Group = "genre1", opacity = .75, colourScale = JS('force.alpha(1); force.restart(); d3.scaleOrdinal(d3.schemeCategory20);'), zoom = T, fontSize=12,
               fontFamily = "serif", clickAction = MyClickScript, legend =T,
               linkColour = "gray",bounded = F)
})


output$plot_tm <- renderPlot({

  res_nodes2<-res_nodes1 %>% 
    mutate(res_nodes$genre1, color = ifelse(grepl("alternative", genre1), "#FF0000",
                                            ifelse(grepl("alternative rock", genre1),"#FF4500",
                                                   ifelse(grepl("classic rock", genre1),"#FFA500",
                                                          ifelse(grepl("dream pop", genre1),"#FFFF00",
                                                                 ifelse(grepl("electronic", genre1),"#ADFF2F",
                                                                        ifelse(grepl("female vocalists", genre1),"#008000",
                                                                               ifelse(grepl("folk", genre1),"#20B2AA",
                                                                                      ifelse(grepl("grunge", genre1),"#0000FF",
                                                                                             ifelse(grepl("hip-hop", genre1),"#6A5ACD",
                                                                                                    ifelse(grepl("indie", genre1),"#4B0082",
                                                                                                           ifelse(grepl("pop", genre1),"#8A2BE2",
                                                                                                                  ifelse(grepl("post-punk", genre1),"#EE82EE",
                                                                                                                         ifelse(grepl("progressive rock", genre1),"#FA8072",
                                                                                                                                ifelse(grepl("pychedelic Rock", genre1),"#FF00FF",
                                                                                                                                       ifelse(grepl("rap", genre1),"#F4A460",
                                                                                                                                              ifelse(grepl("rnb", genre1),"#00FF00",
                                                                                                                                                     ifelse(grepl("rock", genre1),"#000000",
                                                                                                                                                            ifelse(grepl("seen live", genre1),"#00BFFF",
                                                                                                                                                                   ifelse(grepl("soul",genre1),"#FFC0CB","other"))))))))))))))))))))%>%  
    select(genre1 = genre1, color)
  
  #bar chart & tree map
    res_nodes3 <- res_nodes2 %>% 
    group_by(genre1,color) %>% tally()%>%#count(genre1) 
    ungroup()%>% 
    arrange(desc(n))%>% 
    mutate(x = row_number()) %>% 
    rename(name = genre1,
           color = color,
           y = n) %>% 
    select(y, name, color) %>% 
    list.parse()
  
  hcbar <- highchart() %>% 
    hc_xAxis(categories = unlist(pluck(res_nodes3, i = 2))) %>% 
    hc_yAxis(title = NULL) %>% 
    hc_add_series(data = res_nodes3, type = "bar", showInLegend = FALSE,
                  name = "Number Artists by Genre")
  
  hcbar
  
  
  
  set.seed(3514)
  
  par(mar=c(0,0,0,0), xaxs='i', yaxs='i') 
  plot(c(0,1), c(0,1),axes=F, col="white")
  vps <- baseViewports()
  
  res_tm <- res_nodes %>% 
    mutate(genre2 = ifelse(is.na(genre2), paste("only", genre1), genre2),
           genre1 = genre1) %>% 
    group_by(genre1, genre2) %>%
    summarise(n = n()) %>% 
    ungroup() %>% 
    treemap(index = c("genre1", "genre2"),
          vSize = "n", vColor = "genre1")
  # .tm <<- treemap(business, 
  #                 index=c("NACE1", "NACE2"),
  #                 vSize=input$size, vp=vps$plot)

  
})



}
# test d3 network formatting
#input$opacity<-.7,linkDistance = 250, charge = -50

##ui##
ui <- dashboardPage(
  dashboardHeader(title = "Rtists"),
  dashboardSidebar(
    #sidebarSearchForm(label = "Enter a number", "searchText", "searchButton"),
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      #id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", icon = icon("th"), tabName = "widgets", badgeLabel = "new",
                badgeColor = "green")
      #,
      #menuItem("Charts", icon = icon("bar-chart-o"),
      #         menuSubItem("Sub-item 1", tabName = "subitem1"),
      #         menuSubItem("Sub-item 2", tabName = "subitem2")
      )
    #),
    #selectInput("d_userID", u_hdr,
    #            choices = c(Enter_Artist_ID='', artistIDs ))
    # 
    # radioButtons("Rec_Choices", label=strong("Select A Recommendation Method:"),
    #              choices = list("By Similar Artists (Top 5)" = "art_sim", 
    #                             "By Genre (Top 5)" = "ag_mat", 
    #                             "10 Artists Recommended by Similar Users" = "tenrecs"),
    #              selected = "tenrecs"),
    # strong("Select an Artist You Have Previously Listened To:"),
    # DT::dataTableOutput("profileTable")
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    HTML('<footer> © 2018 Nick Italiano </footer>'
    ),
    tags$style(type="text/css", "footer{
               position:absolute;
               bottom:0;
               width:100%;
               height:40px; /* Height of the footer */
               color: white;
               padding: 10px;
               background-color: black;
               z-index: 1000;}"
    ),
              fluidRow(
                box("Artist Similarity by Genre Tags", forceNetworkOutput("force", height = 450)
                ),
                box(title = "Genre Tree", plotOutput("plot_tm", height = 450)
                ),
                box(
                  title = "Controls",solidHeader = TRUE,
                  sliderInput("opacity", "Opacity (not for Sankey)", 0.6, min = 0.1,
                              max = 1, step = .1))
      )
  
  ))

shinyApp(ui, server)
