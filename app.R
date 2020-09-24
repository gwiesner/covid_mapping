library(shiny)

library(ggplot2)
library(sf)
library(rgeos)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)

#creat world dataset for mapping
world <- ne_countries(scale = 50, returnclass = "sf") # sp class default; scale=50 for medium rsolution (and more countries!)
class(world)

# get coronavisrus data from URL
coronavirus_data<- read.csv(url("https://raw.githubusercontent.com/RamiKrispin/coronavirus-csv/master/coronavirus_dataset.csv"))

#Recode coronavirus_data country names to match world df names
coronavirus_data$Country.Region <- dplyr::recode(coronavirus_data$Country.Region, "Mainland China" = "China",
                                                 "US" = "United States",
                                                 "UK" = "United Kingdom",
                                                 "Macau" = "Macao",
                                                 "South Korea" = "Korea")

#recode date var
coronavirus_data <- coronavirus_data %>% 
  dplyr::mutate(date = as.Date(date))

# Summarise cases by country
cv_country_count <- coronavirus_data %>% 
  dplyr::group_by(Country.Region) %>% 
  dplyr::summarise(cases = sum(cases))

#Heatmap colour coding based on incidence
world_mod <- dplyr::left_join(world, cv_country_count, by = c("name" = "Country.Region"))

#With country labels
world_points<- st_centroid(world_mod)
world_points <- cbind(world_mod, st_coordinates(st_centroid(world_mod$geometry)))

#add bounds
names <- world_mod$name

bounds <- purrr::map_dfr(1:nrow(world_mod), 
                         ~ bind_rows(st_bbox(world_mod$geometry[.x]))) %>% 
  cbind(name = names)


world_mod <- left_join(world_mod, bounds)


# Contiguous USA (without Alaska and Hawaii) and Mainland France (i.e. minus French Guiana)
world_points <- world_points %>% 
  dplyr::mutate(X = ifelse(name == "United States",
                           -98.585522,
                           X),
                Y = ifelse(name == "United States",
                           39.8333333,
                           Y)) 

world_points <- world_points %>% 
  dplyr::mutate(X = ifelse(name == "France",
                           2.00,
                           X),
                Y = ifelse(name == "France",
                           46.00,
                           Y))

totals_table <- coronavirus_data %>% 
  dplyr::group_by(`Case Type` = type) %>% 
  summarise(`No. of Cases` = sum(cases))

top_10 <- head(coronavirus_data %>% 
  dplyr::group_by(`Country` = Country.Region) %>% 
 summarise(`No. of Cases` = sum(cases)) %>% arrange(desc(`No. of Cases`)), 10)

options(scipen = 10000)

ui <- fluidPage(
  titlePanel("Global nCOVID-19"),
  selectInput(inputId = "continent", "Region", c("Global", "Africa", "Asia", "Europe", "North America",          
                                                 "Oceania", "South America")),
  mainPanel( 
    plotOutput(outputId = "mymap", click = "plot_click"),
    verbatimTextOutput("info"),
    #this allows me to put the checkmarks ontop of the map 
    absolutePanel(top = 375, left = 25, 
                  checkboxInput("markers", "Country names", FALSE),
                  tableOutput('table1'),
                  tableOutput('table2')
    )
  )
  
)

server <- function(input, output) {
 
  output$table1 <- renderTable(totals_table, caption = "<b> <span style='color:#000000'> Global Summary: </b>",  
                               caption.placement = getOption("xtable.caption.placement", "top"))
  output$table2 <- renderTable(top_10, caption = "<b> <span style='color:#000000'> Top 10 Affected Nations </b>",  
                               caption.placement = getOption("xtable.caption.placement", "top"))
  #create the map
  output$mymap <- renderPlot({
    if (input$markers) {
       if (input$continent %in% c("Africa", "Asia", "Europe", "North America",          
                                 "Oceania", "South America")) {
        if(input$continent == "Europe" ) {
          world_mod[world_mod$continent== input$continent,] %>% 
            ggplot() +
            geom_sf(aes(fill = cases)) + 
            theme(panel.background = element_rect(fill = "aliceblue")) +
            scale_fill_gradient(low="seashell", high = "red", trans = "log10") +  
            labs(fill = "No. of Cases") +
            coord_sf(xlim= c(-25, 58), ylim = c(35, 80)) + #Arbitrary cutoff for "European Russia"
            theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),
                  axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
            ggtitle("Global coronavirus incidence", subtitle = max(coronavirus_data$date)) +
            geom_text(data= world_points[world_points$continent== input$continent,],aes(x=X, y=Y, label=ifelse(!is.na(cases),
                                                                    abbrev,
                                                                    "")),
                      color = "blue", fontface = "bold", size=3, check_overlap = F)
        } else {
          if(input$continent == "North America") {
            world_mod[world_mod$continent== input$continent,] %>% 
              ggplot() +
              geom_sf(aes(fill = cases)) + 
              theme(panel.background = element_rect(fill = "aliceblue")) +
              scale_fill_gradient(low="seashell", high = "red", trans = "log10") +  
              labs(fill = "No. of Cases") +
              coord_sf(xlim= c(-180, 0)) +
              theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),
                    axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
              ggtitle("Global coronavirus incidence", subtitle = max(coronavirus_data$date)) +
              geom_text(data= world_points[world_points$continent== input$continent,],aes(x=X, y=Y, label=ifelse(!is.na(cases),
                                                                      abbrev,
                                                                      "")),
                        color = "blue", fontface = "bold", size=4, check_overlap = F)}
          else {
            if(input$continent == "Oceania") {
              world_mod[world_mod$continent== input$continent,] %>% 
                ggplot() +
                geom_sf(aes(fill = cases)) + 
                theme(panel.background = element_rect(fill = "aliceblue")) +
                scale_fill_gradient(low="seashell", high = "red", trans = "log10") +  
                labs(fill = "No. of Cases") +
                coord_sf(xlim= c(90, 180)) +
                theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),
                      axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
                ggtitle("Global coronavirus incidence", subtitle = max(coronavirus_data$date)) +
                geom_text(data= world_points[world_points$continent== input$continent,],aes(x=X, y=Y, label=ifelse(!is.na(cases),
                                                                                                                   abbrev,
                                                                                                                   "")),
                          color = "blue", fontface = "bold", size=4, check_overlap = F)}
              else{
              world_mod[world_mod$continent== input$continent,] %>% 
              ggplot() +
              geom_sf(aes(fill = cases)) + 
              theme(panel.background = element_rect(fill = "aliceblue")) +
              scale_fill_gradient(low="seashell", high = "red", trans = "log10") +  
              labs(fill = "No. of Cases") +
                  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),
                        axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
              ggtitle("Global coronavirus incidence", subtitle = max(coronavirus_data$date))+
              geom_text(data= world_points[world_points$continent== input$continent,],aes(x=X, y=Y, label=ifelse(!is.na(cases),
                                                                      abbrev,
                                                                      "")),
                        color = "blue", fontface = "bold", size=4, check_overlap = F)}}}}
          else {
            ggplot(world_mod) +
              geom_sf(aes(fill = cases)) + 
              theme(panel.background = element_rect(fill = "aliceblue")) +
              #theme(legend.position = "right", legend.key.height = unit(0.75, "ncp")) + 
              scale_fill_gradient(low= "seashell", high = "red", trans = "log10") +  
              labs(fill = "No. of Cases") +
              coord_sf(ylim= c(-55, 90)) +
              theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),
                    axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
              ggtitle("Global coronavirus incidence", subtitle = max(coronavirus_data$date)) +
              geom_text(data= world_points,aes(x=X, y=Y, label=ifelse(!is.na(cases) & cases>quantile(cases, 0.80, na.rm = T),
                                                                      abbrev,
                                                                      "")),
                        color = "blue", fontface = "bold", size=3, check_overlap = F)
             
          }}  
      else {
          if (input$continent %in% c("Africa", "Asia", "Europe", "North America",          
                                     "Oceania", "South America")) {
            if(input$continent == "Europe" ) {
              world_mod[world_mod$continent== input$continent,] %>% 
                ggplot() +
                geom_sf(aes(fill = cases)) + 
                theme(panel.background = element_rect(fill = "aliceblue")) +
                scale_fill_gradient(low="seashell", high = "red", trans = "log10") +  
                labs(fill = "No. of Cases") +
                coord_sf(xlim= c(-25, 58), ylim = c(35, 80)) + #Arbitrary cutoff for "European Russia"
                theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),
                      axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
                ggtitle("Global coronavirus incidence", subtitle = max(coronavirus_data$date)) 
            } else {
              if(input$continent == "North America") {
                world_mod[world_mod$continent== input$continent,] %>% 
                  ggplot() +
                  geom_sf(aes(fill = cases)) + 
                  theme(panel.background = element_rect(fill = "aliceblue")) +
                  scale_fill_gradient(low="seashell", high = "red", trans = "log10") +  
                  labs(fill = "No. of Cases") +
                  coord_sf(xlim= c(-180, 0)) +
                  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),
                        axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
                  ggtitle("Global coronavirus incidence", subtitle = max(coronavirus_data$date)) }
              else {
                if(input$continent == "Oceania") {
                  world_mod[world_mod$continent== input$continent,] %>% 
                    ggplot() +
                    geom_sf(aes(fill = cases)) + 
                    theme(panel.background = element_rect(fill = "aliceblue")) +
                    scale_fill_gradient(low="seashell", high = "red", trans = "log10") +  
                    labs(fill = "No. of Cases") +
                    coord_sf(xlim= c(90, 180)) +
                    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),
                          axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
                    ggtitle("Global coronavirus incidence", subtitle = max(coronavirus_data$date))}
                else {
                  world_mod[world_mod$continent== input$continent,] %>% 
                  ggplot() +
                  geom_sf(aes(fill = cases)) + 
                  theme(panel.background = element_rect(fill = "aliceblue")) +
                  scale_fill_gradient(low="seashell", high = "red", trans = "log10") +  
                  labs(fill = "No. of Cases") +
                    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),
                          axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
                  ggtitle("Global coronavirus incidence", subtitle = max(coronavirus_data$date))}}}}
            else {
                  ggplot(world_mod) +
                    geom_sf(aes(fill = cases)) + 
                    theme(panel.background = element_rect(fill = "aliceblue")) +
                    scale_fill_gradient(low="seashell", high = "red", trans = "log10") +  
                    labs(fill = "No. of Cases") +
                    coord_sf(ylim= c(-55, 90)) +
                    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
                    ggtitle("Global coronavirus incidence", subtitle = max(coronavirus_data$date))}
            }
        }
     
    )
 
}


# Run the application 
shinyApp(ui = ui, server = server)
