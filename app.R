
library(shinydashboard)
library(plotly)
library(reactable)
library(shiny)
library(shinythemes)
library(tidyverse)
library(readr)
library(visdat)
library(ggplot2)
library(dplyr)




data <- read_csv("data/data.csv")

artist <- read_csv("data/data_by_artist.csv")

artist_genre <- read_csv("data/data_w_genres.csv")

by_genre <- read_csv("data/all_genre.csv")

year <- read_csv("data/year.csv")
billboard <- read_csv("data/top10s.csv")

billboard <- billboard %>%
  rename(genres=`top genre`)%>%
  select(-X1)

billboard <- billboard %>%
  mutate(maingenre=case_when(str_detect(genres,"pop") ~ "pop",
                             str_detect(genres,"indie")~ "indie",
                             str_detect(genres,"folklore")~ "folk",
                             str_detect(genres,"rock")~ "rock",
                             str_detect(genres,"metal")~ "metal",
                             str_detect(genres,"Indian")~ "Indian",
                             str_detect(genres,"hindustani")~ "Indian",
                             str_detect(genres,"bollywood")~ "Indian",
                             str_detect(genres,"cello")~ "classical",
                             
                             str_detect(genres,"electronica")~ "EDM",
                             str_detect(genres,"electric")~ "EDM",
                             str_detect(genres,"jazz")~ "jazz",
                             str_detect(genres,"worship")~ "worship",
                             str_detect(genres,"hip")~ "hip hop",
                             str_detect(genres,"hop")~ "hip hop",
                             str_detect(genres,"folk")~ "folk",
                             str_detect(genres,"punk")~ "punk",
                             str_detect(genres,"classical")~ "classical",
                             str_detect(genres,"house")~ "house",
                             str_detect(genres,"blues")~ "blues",
                             str_detect(genres,"soul")~ "soul",
                             str_detect(genres,"capella")~ "capella",
                             str_detect(genres,"trap")~ "trap",
                             str_detect(genres,"funk")~ "funk",
                             str_detect(genres,"electronic")~ "EDM",
                             str_detect(genres,"dance")~ "dance",
                             str_detect(genres,"r&b")~ "R&B",
                             str_detect(genres,"techno")~ "EDM",
                             str_detect(genres,"circuit")~ "EDM",
                             str_detect(genres,"soundtrack")~ "soundtrack",
                             str_detect(genres,"baroque")~ "baroque",
                             str_detect(genres,"metalcore")~ "metal",
                             str_detect(genres,"trance")~ "trance",
                             str_detect(genres,"disco")~ "dance",
                             str_detect(genres,"edm")~ "EDM",
                             str_detect(genres,"electropop")~ "EDM",
                             str_detect(genres,"electro")~ "EDM",
                             str_detect(genres,"korean")~ "korean",
                             str_detect(genres,"dancehall")~ "dancehall",
                             str_detect(genres,"salsa")~ "salsa",
                             TRUE ~ as.character(genres)
                             
                             
  ))



new <- data %>% 
  mutate(artist_name=str_replace_all(artists,"\\[|\\]", " "))
new$artist_name <- gsub("\\'|\\'","" , new$artist_name ,ignore.case = TRUE) 
new$artist_name <- gsub("\"", "", new$artist_name)


#names <- as_tibble(unique(data$artists))



artist_genre <- artist_genre %>%
  mutate(genres=str_replace_all(genres,"\\[|\\]", ""))
artist_genre$genres <- gsub("\\'|\\'","" , artist_genre$genres ,ignore.case = TRUE) 
artist_genre$genres <- gsub("\"", "", artist_genre$genres)


artist_genre$genres[artist_genre$genres==""] <- NA

#gnames <- as_tibble(unique(new_artist_genre$genre_name)) 
options(scipen=999)
out_clean<- function(songs) { with_outliers <- songs %>%
  ggplot(aes(y = duration_ms)) +
  geom_boxplot(coef = 4) +
  coord_flip() +
  labs(title = 'Duration in ms') 

duration_outliers <- boxplot(songs$duration_ms, 
                             plot = FALSE, range = 4)$out
as.tibble(duration_outliers)

songs <- songs %>%
  filter(!duration_ms %in% duration_outliers) 

return(songs)

}

new_no_out <- out_clean(new)
year_no_out<-out_clean(year)#no change 
bygenre_no_out<- out_clean(by_genre)
artistgenre_no_out<- out_clean(artist_genre)
artist_no_out<- out_clean(artist) #no change



new_no_out$name <- gsub("\\s*\\([^\\)]+\\)","",as.character(new_no_out$name))
new_no_out$name <- gsub("\\s*\\[[^\\)]+\\]","",as.character(new_no_out$name))
new_no_out$artists <- gsub("\\s*\\[[^\\)]+\\]","",as.character(new_no_out$artists))

bygenre_no_out <- bygenre_no_out %>%
  filter(genres!="[]")

library(stringr)
bygenre_no_out<- bygenre_no_out %>%
  mutate(maingenre=case_when(str_detect(genres,"pop") ~ "pop",
                             str_detect(genres,"indie")~ "indie",
                             str_detect(genres,"folklore")~ "folk",
                             str_detect(genres,"rock")~ "rock",
                             str_detect(genres,"metal")~ "metal",
                             str_detect(genres,"Indian")~ "Indian",
                             str_detect(genres,"hindustani")~ "Indian",
                             str_detect(genres,"bollywood")~ "Indian",
                             str_detect(genres,"cello")~ "classical",
                             
                             str_detect(genres,"electronica")~ "EDM",
                             str_detect(genres,"electric")~ "EDM",
                             str_detect(genres,"jazz")~ "jazz",
                             str_detect(genres,"worship")~ "worship",
                             str_detect(genres,"hip")~ "hip hop",
                             str_detect(genres,"hop")~ "hip hop",
                             str_detect(genres,"folk")~ "folk",
                             str_detect(genres,"punk")~ "punk",
                             str_detect(genres,"classical")~ "classical",
                             str_detect(genres,"house")~ "house",
                             str_detect(genres,"blues")~ "blues",
                             str_detect(genres,"soul")~ "soul",
                             str_detect(genres,"capella")~ "capella",
                             str_detect(genres,"trap")~ "trap",
                             str_detect(genres,"funk")~ "funk",
                             str_detect(genres,"electronic")~ "EDM",
                             str_detect(genres,"dance")~ "dance",
                             str_detect(genres,"r&b")~ "R&B",
                             str_detect(genres,"techno")~ "EDM",
                             str_detect(genres,"circuit")~ "EDM",
                             str_detect(genres,"soundtrack")~ "soundtrack",
                             str_detect(genres,"baroque")~ "baroque",
                             str_detect(genres,"metalcore")~ "metal",
                             str_detect(genres,"trance")~ "trance",
                             str_detect(genres,"disco")~ "dance",
                             str_detect(genres,"edm")~ "EDM",
                             str_detect(genres,"electropop")~ "EDM",
                             str_detect(genres,"electro")~ "EDM",
                             str_detect(genres,"korean")~ "korean",
                             str_detect(genres,"dancehall")~ "dancehall",
                             str_detect(genres,"salsa")~ "salsa",
                             TRUE ~ as.character(genres)
                             
                             
  ))
bygenre_no_out <- bygenre_no_out %>%
  group_by(maingenre)%>%
  mutate(popscore=sum(popularity)) %>%
  arrange(desc(popscore)) 


bygenre_no_out<- bygenre_no_out %>%
  rename(subgenre=genres)
classify <- bygenre_no_out %>%
  group_by(maingenre,popscore) %>%
  
  summarise(acousticness=mean(acousticness),
            danceability=mean(danceability),
            energy=mean(energy),
            instrumentalness=mean(instrumentalness),
            liveness=mean(liveness),
            loudness=mean(loudness),
            speechiness=mean(speechiness),
            valence=mean(valence),
            tempo=mean(tempo)
  ) %>%
  arrange(desc(popscore))

g <- bygenre_no_out %>%
  
  
  group_by(maingenre) %>%
  summarise(popscore=sum(popularity))  %>%
  mutate(poper=(popscore/sum(popscore))*100) %>%
  arrange(desc(poper))  %>%
  head(10)

gplot<- g%>%
  ggplot(aes(x=poper,y=,reorder(maingenre,poper),fill=maingenre))+
  geom_col()

new_no_out <- new_no_out %>%
  mutate(decade = case_when(   between(year,1920,1930) == TRUE ~"1920s",
                               between(year,1930,1940) == TRUE ~"1930s",
                               between(year,1940,1950) == TRUE ~"1940s",
                               between(year,1950,1960) == TRUE ~"1950s",
                               between(year,1960,1970) == TRUE ~"1960s",
                               between(year,1970,1980) == TRUE ~"1970s",
                               between(year,1980,1990) == TRUE ~"1980s",
                               between(year,1990,2000) == TRUE ~"1990s",
                               between(year,2000,2010) == TRUE ~"2000s",
                               between(year,2010,2020) == TRUE ~"2010s"))


audiochar <- new_no_out %>%
  select(loudness,danceability,energy,acousticness)%>%
  pivot_longer(cols=c(1:4),names_to = "characteristics",values_to = "values")%>%
  distinct(characteristics)

year_no_out <- year_no_out %>% 
  mutate(decade = case_when(   between(year,1920,1930) == TRUE ~"1920s",
                               between(year,1930,1940) == TRUE ~"1930s",
                               between(year,1940,1950) == TRUE ~"1940s",
                               between(year,1950,1960) == TRUE ~"1950s",
                               between(year,1960,1970) == TRUE ~"1960s",
                               between(year,1970,1980) == TRUE ~"1970s",
                               between(year,1980,1990) == TRUE ~"1980s",
                               between(year,1990,2000) == TRUE ~"1990s",
                               between(year,2000,2010) == TRUE ~"2000s",
                               between(year,2010,2020) == TRUE ~"2010s"))
  


interestedgenre <-   billboard %>%
  filter(maingenre %in% c(g$maingenre))
box <-  year_no_out %>%
  # filter(decade == input$chosendecade)%>%
  select(-c(key,mode,decade))%>%
  
  filter(year%in% c(min(year),max(year)))%>%
  pivot_longer(cols=2:12,names_to = "characteristics",values_to = "values")

box <- box%>%
  group_by(characteristics)%>%
  group_by(characteristics)%>%
  mutate(change =((values-lag(values))/values)*100) %>%
  filter(year=="2020")
cols2 <- c("#233d4d", "#fe7f2d", "#fcca46","#a1c181","#083d77","#513b56","#98ce00","#348aa7","#840032","#db3a34")


ui <- fluidPage(

  navbarPage(
    theme = shinytheme("cyborg"),
    "Business of Music : Where do you want to bet your money ?",
    id = "main",
    
    tabPanel(
      
      "About",fluidRow(
        h2("About the App"),
        h3("Creater : Aarathy Babu"),
        fluidRow(
          align="center",width="450px",
          img(src = "picture.png", width = "450px", height = "250px" ,alt="Image Source: The Economic Times",style="text-align: center;"),
          verbatimTextOutput("source")),
        br(),
        p("With the rise of streaming services like Spotify, the target audience can be tracked and Millenials are around
55 % of total Spotify users. This gives music industry investors an opportunity to invest in genre and artists which cater to the majority of the streaming device user population. This web application aims to assist the industry investors in deciding where to bet their money on by exploring the audio features of different music and genre by analysing the audio features data on the music released over 100 years that is available on Spotify USA.",
          style = "text-align:justify;color:black;background-color:lightgreen;padding:50px;border-radius:20px"
        ),
        br()
      )
      
      
      
      
    ),
    
    
    tabPanel(
      "Evolution of Music",
      
  
          fluidRow(
            h3("1921 & 2020 : A comparison"),
           
            column(4,infoBoxOutput("Box1"),
                   infoBoxOutput("progressBox"),
                   infoBoxOutput("approvalBox"),
                   infoBoxOutput("Box5")),
            column(4,
                   infoBoxOutput("Box2"),
                   infoBoxOutput("Box3"),
                   infoBoxOutput("Box4"),
                   infoBoxOutput("Box6"))),
      fluidRow(
            br(),
            p("Over the years, Audio features in music have made fascinating changes. Music have become more danceable and energetic as compared to 1921. It is quite interesting to see that songs have become of shorter duration, this is most likely the effect of streaming platforms like Spotify. Instead of getting paid by physical sales, artists are getting paid in a stream, which only counts if someone listens to 30 seconds of a song. For artists to have more songs streamed at a time, you would want to pack your album full of much shorter songs. Choose a particular decade from the menu below to further explore the changes in audio features through each decade."),
            br(),
            selectInput("chosendecade","Choose a decade",unique(new_no_out$decade))
         
                      
                      #selectInput("audio","Choose a feature",audiochar$characteristics)
                       #varSelectInput("audio", "Choose a feature", year_no_out%>% select(-c(year,decade,key,mode)))
                       ),
          
          fluidRow(
            
               
              column( 9,   
                  plotOutput("evolution")),
              column( 3,   br(),
                      p("Throughout the past few decades,acousticness of music fell where as the energy level increased. Music became louder and faster. Therefore, it is likely that the trend could continue for the next few years too.",
                        style = "text-align:justify;color:black;background-color:lightgreen;padding:50px;border-radius:10px"
                      ),
                      br())
                          
          
      )
      
      
    ),
    navbarMenu(
      "What's Popular?",
      tabPanel(
        "Genre",
        headerPanel("What's popping? All about popular genre"),
        br(),
        p("The most popular genre is Pop followed by Indie. Other popular genres include Rock,Metal, Hip Hop etc. To strengthen evidence, the BillBoard Hits of the past 10 years were analyzed and the number of hits each genre has contributed is displayed. The user can explore using the drop down menu with the genres common to the Billboard list as well."),
        br(),
        sidebarLayout(
         
          sidebarPanel(plotOutput("barchart", height = "600px")),
          mainPanel(
            selectInput(
              "genrename",
              "Choose a genre",
              unique(interestedgenre$maingenre),
              selected = "pop", multiple = FALSE
              
            ),
            plotOutput("releasedsongs"),
            br(),
            p(" The genre with highest number of songs in Billboard Top 10 Hits is definitely Pop,contributing more than 10 songs each year for the past 10 years. This proves that investing in Pop genre is assuring.", style = "text-align:justify;color:black;background-color:lightgreen;padding:20px;border-radius:10px"),
            br()
          )
        )
      ),
      tabPanel(
        "Secret Sauces for popularity",
        headerPanel("What are the secret sauces for popularity?"),
        br(),
        p("There is a close relationship between various audio features and how popular a song is. This aspect can be incorporated during the making of a track which would enable the investors to know how popular the track would be. There are four major features that heavily influence popularity. Less acoustic but more energetic,loud and danceable songs are getting more popular, therefore incorporating these could help in getting traction for a music track. To explore various audio features in a genre, the user can focus on the radar chart and can either choose the interested genre from the menu or simply click on the desired genre that is shaded. The user can also select and deselect genres by clicking on the genre in the legend as well. "),
        br(),
       column(6,
        checkboxGroupInput("audio", "Choose the audio characteristics",audiochar$characteristics,selected = c("acousticness","danceability","energy")),
        plotOutput("linechart")
        ),
        
        column(6,
             #  selectInput("selectgenre","Select a genre",interestedgenre$maingenre,selected = "pop"),
          plotlyOutput("radarchart")
                 ),
       fluidRow(
         br(),
         p(" Among the different genres, the most danceable is Hip-Hop,
           followed by House where as the most energetic is EDM. The most popular genre is Pop. 
           Pop is somewhat the middle ground of all genre, given an opposite relationship between acousticness and popularity, 
           Pop genre has a certain level of acoustic and valence features to it. Therefore, pop genre is most likely to speak to majority of the user population which makes investment in Pop genre a safe bet.", style = "text-align:justify;color:black;background-color:lightgreen;padding:50px;border-radius:10px"),
         br()
         )
       
       )),
      tabPanel(
        "Whom should you invest in?",
          fluidRow(
            sidebarLayout(
              sidebarPanel(
                br(),
                p("Through the analysis, we can come to a decision that Pop Music is worth investing in. However knowing which genre to invest in is not enough,therefore we can look into the artists with most number of hits on Billboard Top 10 Hits. The user can explore the genre they are interested in and the possible artists to invest on in that genre by using the drop down menu.", style = "text-align:justify;color:black;background-color:lightgreen;padding:25px;border-radius:10px"),
                br(),
              selectInput("selectgenre","Select a genre",unique(interestedgenre$maingenre),selected = "pop"),
              img(src="katy.gif", align = "left",height='250px',width='400px')
              ),
      
              mainPanel( 
               h3("Artists worth investing on") ,
              reactableOutput("finallist"),
              br(),
              p("As per the analysis, it is advisable that Pop music is the right genre to invest in and that Katy Perry is a top artist who gives consistent hits, therefore an approporiate candidate to invest in. ", style = "text-align:justify;color:black;background-color:lightgreen;padding:25px;border-radius:10px"),
              br()
              )))
        ),
      
      tabPanel("References",
               includeMarkdown(here::here("data/about.md"))),
    includeCSS(here::here("style.css"))
    
  
))



server <- function(input, output, session) {
  
  output$source<- renderText({
    source <- paste("Image Source: fortune.com")
    source})
 
  #tab 1
  output$evolution <-  renderPlot({
    library(lubridate)
   decade_change <-  year_no_out %>%
    filter(decade == input$chosendecade)%>%
     select(-c(key,mode,decade))%>%
     mutate(year=as.Date(as.character(year), format = "%Y")) %>%
     mutate(year=year(year))
   
   decade_change <- decade_change%>%
     pivot_longer(cols=2:10,names_to = "characteristics",values_to = "values")%>%
     group_by(characteristics)%>%
      mutate(change =((values-lag(values))/values)*100) 
   
  
   decade_change%>%
      ggplot(aes(x=(year),y=change,color=characteristics))+
      geom_point()+geom_line()+
     facet_wrap(~characteristics,scales = "free")+ 
     theme(panel.background = element_rect(fill = "#dbfeb8",
                                           color = "black", size = 1))+
     xlab("Year")+
     ylab("Percentage Change in feature")+
     scale_color_manual(values = cols2)+
     theme(legend.position = "none")+
     ggtitle("Change in Audio Features through the decade")
    
    
  })
  
  output$progressBox <- renderInfoBox({
    
  
    infoBox(
      "Acousticness", paste0(round(box$change[box$characteristics == "acousticness"],2), "%"), icon = icon("list"),
      color = "yellow"
    )
  })
  output$approvalBox <- renderInfoBox({
    infoBox(
      "Danceability",paste0(round(box$change[box$characteristics == "danceability"],2), "%"), icon =icon("list") ,
      color = "yellow"
    )
  })
  output$Box1 <- renderInfoBox({
    
    infoBox(
      "Loudness",paste0(round(box$change[box$characteristics == "loudness"],2), "%"), icon =icon("list") ,
      color = "yellow"
    )})
  
    output$Box2 <- renderInfoBox({
      
      infoBox(
        "Duration",paste0(round(box$change[box$characteristics == "duration_ms"],2), "%"), icon =icon("list") ,
        color = "yellow"
      )
      
    
    
  })
  
    output$Box3 <- renderInfoBox({
      
      infoBox(
        "Energy",paste0(round(box$change[box$characteristics == "energy"],2), "%"), icon =icon("list") ,
        color = "yellow"
      )
      })
    
    output$Box4 <- renderInfoBox({
      
      infoBox(
        "Instrumental",paste0(round(box$change[box$characteristics == "instrumentalness"],2), "%"), icon =icon("list") ,
        color = "yellow"
      )
    })
    output$Box5 <- renderInfoBox({
      
      infoBox(
        "Speechiness",paste0(round(box$change[box$characteristics == "speechiness"],2), "%"), icon =icon("list") ,
        color = "yellow"
      )
    })
    output$Box6 <- renderInfoBox({
      
      infoBox(
        "Tempo",paste0(round(box$change[box$characteristics == "tempo"],2), "%"), icon =icon("list") ,
        color = "yellow"
      )
    })
  #tab 2
  
  output$barchart <-  renderPlot({
    
    gplot+ theme_bw()+
      ylab("Genre")+
      scale_fill_manual(values = cols2)+
      theme(legend.position = "none")+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())+
      ggtitle("Popular Music Genre")
    
  }) 
  
  output$releasedsongs <-  renderPlot({
 
     
      
     billboardhits <-  interestedgenre %>%
        group_by(year,maingenre)%>%
        count()
    p <- ggplot(billboardhits%>%filter(maingenre==input$genrename), 
                aes(x=as.factor(year),y=n))
   
    p + geom_col(aes(fill=as.factor(year)))+
      theme_bw()+
      ylab("Number of Billboard Hits")+
      xlab("Year")+
      scale_fill_manual(values = cols2)+
      theme(legend.position = "none")+
      ggtitle("Contribuition of Genre to Billboard Hits")
    
  })
  
  #tab 3
  output$linechart <-  renderPlot({
    
    
    year_no_out %>%
      select(loudness,danceability,energy,acousticness,popularity)%>%
      pivot_longer(cols=c(1:4),names_to = "characteristics",values_to = "values")%>%
      filter(characteristics %in% input$audio)%>%
      ggplot(aes(x=popularity,y=values,color=characteristics))+
      geom_smooth(se=F)+
      scale_color_manual(values=cols2)+
      theme_bw()+
      ggtitle("Relationship between Audio features and Popularity")+
      xlab("Popularity")+
      ylab("Audio Features")
    
  })
  
  output$radarchart<-  renderPlotly({
    
    
 
    
    
  radardata <-   classify%>%
      filter(maingenre %in% interestedgenre$maingenre)
  sum <- sum(radardata$popscore)
  
  radardata <- radardata%>%
    
    mutate(popularity=(popscore/sum)) %>%
    select(c("acousticness", 
             "popularity",
             "danceability",
             "energy",
             "valence"))
  
  
  
  save <- radardata%>%
    pivot_longer(cols=2:6,names_to = "characteristics",values_to = "values") %>%
    filter(characteristics%in%c("acousticness", 
                                "popularity",
                          
                             "danceability",
                              "energy",
                              "valence"
    ))

  
  
  
  
  tx <- highlight_key(save, ~maingenre)

  base <-  plot_ly( tx,
    type = 'scatterpolar',
    fill = 'toself'
  ) 
    
  
  
 basefig <-  base  %>%
group_by(maingenre) %>%
    add_trace(
      r = save$values,
      theta = save$characteristics,
      name = save$maingenre,
      colors = "Dark2"
    )
 highlight(
   basefig, 
   on = "plotly_click", 
   selectize = TRUE, 
   dynamic = F, 
   persistent = F,
   off = "plotly_doubleclick"
 )
  })
  
  
  # tab 4
  output$finallist <- renderReactable({
    library(reactable)
    library(htmltools)
topartists <-   billboard %>%
    filter(maingenre==input$selectgenre) %>%
    group_by(artist) %>%
    count()%>%
    arrange(desc(n))%>%
 head(10)
topartists$position <- 1:nrow(topartists)
topartists <- topartists %>%
  rename(Hits=n)%>%
  select(position,artist,Hits)
#reactable(topartists)
tracks_table <- function(data) {
  
reactable(
  topartists,
  searchable = TRUE,
  highlight = TRUE,
  wrap = FALSE,
  paginationType = "simple",
  columns = list(
    position = colDef(
      header = tagList(
        span("#", "aria-hidden" = "true", title = "Position"),
        span("Position", class = "sr-only")
      ),
      width = 40
    ),
    artist = colDef(
      name = "Artist",
      resizable = TRUE,
      html = TRUE,
      minWidth = 100
    ),
    Hits = colDef(
      name = "Number of Hits in Billboard",
      resizable = TRUE,
      html = TRUE,
      minWidth = 60
    )
    ),
    theme = spotify_theme()
  )
}
spotify_theme <- function() {
  search_icon <- function(fill = "none") {

    svg <- sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24"><path fill="%s" d="M10 18c1.85 0 3.54-.64 4.9-1.69l4.4 4.4 1.4-1.42-4.39-4.4A8 8 0 102 10a8 8 0 008 8.01zm0-14a6 6 0 11-.01 12.01A6 6 0 0110 4z"/></svg>', fill)
    sprintf("url('data:image/svg+xml;base64,%s')", jsonlite::base64_enc(svg))
  }
  
  text_color <- "hsl(0, 0%, 95%)"
  text_color_light <- "hsl(0, 0%, 70%)"
  text_color_lighter <- "hsl(0, 0%, 55%)"
  bg_color <- "hsl(0, 0%, 10%)"
  
  reactableTheme(
    color = text_color,
    backgroundColor = bg_color,
    borderColor = "hsl(0, 0%, 16%)",
    borderWidth = "1px",
    highlightColor = "rgba(255, 255, 255, 0.1)",
    cellPadding = "10px 8px",
    style = list(
      fontFamily = "Work Sans, Helvetica Neue, Helvetica, Arial, sans-serif",
      fontSize = "14px",
      "a" = list(
        color = text_color,
        "&:hover, &:focus" = list(
          textDecoration = "none",
          borderBottom = "1px solid currentColor"
        )
      ),
      ".number" = list(
        color = text_color_light,
        fontFamily = "Source Code Pro, Consolas, Monaco, monospace"
      ),
      ".tag" = list(
        padding = "2px 4px",
        color = "hsl(0, 0%, 40%)",
        fontSize = "12px",
        border = "1px solid hsl(0, 0%, 24%)",
        borderRadius = "2px"
      )
    ),
    headerStyle = list(
      color = text_color_light,
      fontWeight = 400,
      fontSize = "12px",
      letterSpacing = "1px",
      textTransform = "uppercase",
      "&:hover, &:focus" = list(color = text_color)
    ),
    rowHighlightStyle = list(
      ".tag" = list(color = text_color, borderColor = text_color_lighter)
    ),
    searchInputStyle = list(
      paddingLeft = "30px",
      paddingTop = "8px",
      paddingBottom = "8px",
      width = "100%",
      border = "none",
      backgroundColor = bg_color,
      backgroundImage = search_icon(text_color_light),
      backgroundSize = "16px",
      backgroundPosition = "left 8px center",
      backgroundRepeat = "no-repeat",
      "&:focus" = list(backgroundColor = "rgba(255, 255, 255, 0.1)", border = "none"),
      "&:hover, &:focus" = list(backgroundImage = search_icon(text_color)),
      "::placeholder" = list(color = text_color_lighter),
      "&:hover::placeholder, &:focus::placeholder" = list(color = text_color)
    ),
    paginationStyle = list(color = text_color_light),
    pageButtonHoverStyle = list(backgroundColor = "hsl(0, 0%, 20%)"),
    pageButtonActiveStyle = list(backgroundColor = "hsl(0, 0%, 24%)")
  )
  
}


tracks_table(topartists)

})

  
  
}

# Run the application
shinyApp(ui = ui, server = server)
