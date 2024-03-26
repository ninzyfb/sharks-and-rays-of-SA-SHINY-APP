# helper websites
# https://towardsdatascience.com/putting-the-shine-in-shiny-apps-e3de370b5661

# packages
library(shiny)
library(leaflet)
library(leafpop)
library(tidyverse)
library(shinythemes)
library(reactable)
library(stringr)
library(sf)
library(terra)
library(dplyr)
library(DT)
library(htmlwidgets)
library(leaflegend)
#library(shinyalert)

##################### IUCN COLORS
# Define a color palette for each IUCN Red List status
status_colors <- c(
  "VU" = "rgba(255, 255, 0, 0.7)",     # Vulnerable
  "LC" = "rgba(0, 128, 0, 0.7)",     # Least Concern
  "EN" = "rgba(255, 165, 0, 0.7)",  # Endangered
  "DD" = "rgba(128, 128, 128, 0.7)", # Data Deficient
  "CR" = "rgba(255, 0, 0, 0.7)",     # Critically Endangered
  "NT" = "#B0E57C"    # Near Threatened
)
##################### IUCN COLORS


##################### ENDEMIC COLORS
# Define a color palette for each IUCN Red List status
endemic_colors <- c(
  "South Africa" = "#800080",   # Gold
  "Southern Africa" = "#9370DB", # Silver
  "Not endemic" = "#D8BFD8"     # Bronze
)
##################### ENDEMIC COLORS


##################### ICONS
# Define custom icons
icon_blue <- makeIcon(
  iconUrl = list.files(pattern = "boundary_cross.png",recursive=TRUE,full.names=TRUE),  # Replace with the path to your custom icon image
  iconWidth = 32,  # Adjust the width and height as needed
  iconHeight = 32
)
##################### ICONS


##################### MPA DATA
shapefile_data_simple = readRDS(list.files(pattern = "shapefile_data_simple.RDS",recursive = TRUE))
##################### MPA DATA


##################### MPA OVERVIEW DATA
mpa_data_overview = readRDS(list.files(pattern = "mpa_data_overview.RDS",recursive=TRUE))
##################### MPA OVERVIEW DATA


##################### EEZ DATA
eez_sa = st_read(list.files(pattern = "eez.shp",recursive = TRUE, full.names = TRUE))
##################### EEZ DATA


##################### MASTER SHEET
# Path
#file_path <- list.files(pattern = "data_summary_master.xlsx", recursive=TRUE,full.names = TRUE)
# File
master = readxl::read_xlsx( list.files(pattern = "data_summary_master.xlsx", recursive=TRUE,full.names = TRUE))
# remove variables that i added previously
master = master[,c(1:4,12)]
##################### MASTER SHEET


##################### SPECIES PER MPA DATA
# File
species_overlapdata = read.csv(list.files(pattern = "species_permpa_byiusnandsdms.csv", recursive=TRUE,full.names = TRUE))
##################### SPECIES PER MPA DATA


##################### RASTER DATA
# Read the data
#all_distributions = stack(list.files(path = "/home/nina/Dropbox/6-WILDOCEANS/1-ConservationPlan/wildoceans-scripts_github/Outputs/distribution_rasters/", pattern = "ensemblemean.tiff",full.names = TRUE))
#saveRDS(all_distributions,"all_distributions.RDS")
all_distributions = readRDS(list.files(pattern = "all_distributions.RDS",recursive = TRUE,full.names=TRUE))
##################### RASTER DATA


##################### POLYGON DATA FROM IUCN
iucn_file_list = readRDS(list.files(pattern = "iucn_file_list.RDS",recursive = TRUE))
##################### POLYGON DATA FROM IUCN


##################### PREPARE MASTER TABLE FOR FIRST PAGE
overlap_shortened = unique(species_overlapdata[,c(1,6,7)])
master$iucn = NA
master$iucn[which(master$SPECIES_SCIENTIFIC %in% overlap_shortened$SPECIES_SCIENTIFIC[overlap_shortened$type == "iucn"])] = "Yes"
master$iucn[which(is.na(master$iucn))] = "No"
master$sdm = NA
master$sdm[which(master$SPECIES_SCIENTIFIC %in% overlap_shortened$SPECIES_SCIENTIFIC[overlap_shortened$type == "sdm"])] = "Yes"
master$sdm[which(is.na(master$sdm))] = "No"
master = master %>%
  mutate(ENDEMIC.STATUS = ifelse(ENDEMIC.STATUS == 1,"South Africa", ifelse(ENDEMIC.STATUS == 2,"Southern Africa", "Not endemic")))
##################### PREPARE MASTER TABLE FOR FIRST PAGE


##################### add some info to overlap data
species_overlapdata = left_join(species_overlapdata,master)
##################### add some info to overlap data


##################### Expert extents
load(list.files(pattern = "points.RData", recursive = TRUE, full.names = TRUE))
expert_extent = points
rm(points)
colnames(expert_extent)[1] = "SPECIES_SCIENTIFIC"
expert_extent$SPECIES_SCIENTIFIC = toupper(expert_extent$SPECIES_SCIENTIFIC)
expert_extent = st_as_sf(expert_extent)
expert_extent = left_join(overlap_shortened,expert_extent)
##################### Expert extents


##################### EAST AND WEST BOUNDARIES PER SPECIES
boundaries = read.csv(list.files(pattern = "boundaries.csv",recursive = TRUE, full.names = TRUE))
#####################  EAST AND WEST BOUNDARIES PER SPECIES


##################### DATA POINTS PER HIGH RES GRID CELL
highres_gps = readRDS(list.files(pattern = "mostrecent_highres.RDS",recursive = TRUE, full.names = TRUE))
#####################  DATA POINTS PER HIGH RES GRID CELL


#####################  INITIAL MAP BOUNDARIES
# Define the initial map bounds (you can adjust these values)
initialBounds <- list(
  lng1 = 17,
  lat1 = -30,
  lng2 = 33,
  lat2 = -40
)
##################### INITIAL MAP BOUNDARIES


##################### MOST RECENT SIGHTING PER MPA
mostrecent_sighting = readRDS(list.files(pattern = "sightings_mostrecent.RDS",recursive = TRUE, full.names = TRUE))
mostrecent_sighting$SPECIES_SCIENTIFIC = as.character(mostrecent_sighting$SPECIES_SCIENTIFIC )
mostrecent_sighting = ungroup(mostrecent_sighting)
colnames(mostrecent_sighting)[which(colnames(mostrecent_sighting) == "SPECIES_SCIENTIFIC")] = "Scientific name"
mostrecent_sighting$`Scientific name` = str_to_sentence(mostrecent_sighting$`Scientific name`)
##################### MOST RECENT SIGHTING PER MPA


##################### DATA POINTS PER HIGH RES GRID CELL
lifehistory = read.csv(list.files(pattern = "lifehistory_parameters.csv",recursive = TRUE, full.names = TRUE))
#####################  DATA POINTS PER HIGH RES GRID CELL


##################### DATA POINTS PER HIGH RES GRID CELL
contours = readRDS(list.files(pattern = "contours.RDS",recursive = TRUE))
#####################  DATA POINTS PER HIGH RES GRID CELL

#####################  DEFINING THE UI (USER INTERFACE)
# Define UI for application 
ui <- fluidPage(
  
  theme = shinytheme("united"),
  
  #Jumbotrons are pretty, they make nice headers
  tags$div(class = "jumbotron text-center", style = "margin-bottom:0px;margin-top:0px",
           tags$h2(class = 'jumbotron-heading', stye = 'margin-bottom:0px;margin-top:0px', 'Sharks, Rays and MPAs'),
           p('Interactive website to visualise spatial data on sharks, rays and MPAs in SA')
  ),
  
  tabsetPanel(
    
    # WELCOME TAB
    navbarMenu("About",
               tabPanel("Purpose",
                        fluidRow(
                          column(width = 2),
                          column(width = 8,
                                 h1("Purpose"),
                                 p("South Africa is a global hotspot for shark and ray species. There is, unsurprisingly, a long-standing history of shark and ray research in South Africa, and with every new study, a different dataset and new spatial information emerge. This information is invaluable for understanding the distribution of these species throughout South Africa's waters. The purpose of this web app is an attempt to consolidate the different datasets that have gathered spatial information on these species throughout South Africa and provide up-to-date and verified information on where these species are found, with a particular focus on South Africa's Marine Protected Areas."),
                                 h1("Why is this useful?"),
                                 h2("Collaboration and Knowledge Exchange:"),
                                 p("This app could provide a platform for researchers to share their findings in addition to scientific publications. Whilst publications are important, they are not always the most accessible and are not the most suited tool to disseminate spatial information."),
                                 h2("A resource for park managers and policy makers:"),
                                 p("Managers and policymakers can utilize these consolidated data to get a quick and reliable overview of which species are found in their MPA and when these were last seen. These species lists are invaluable for ensuring data-driven decisions and that conservation efforts and management strategies are based on the most up-to-date, accurate, and comprehensive information, ultimately increasing their effectiveness."),
                                 h2("Educational Resource:"),
                                 p("Whilst this app's main goal is not intended to be educational, it can still serve as a useful tool to learn about the distribution of various species. It can also be used to disseminate information on how zonation in MPAs work and how these vary by MPA and what this means in relation to sharks and rays.")
                          )       
                        )
               ),
               tabPanel("Data",
                        fluidRow(
                          column(width = 2),
                          column(width = 8,
                                 h1("Data"),
                                 p("A wide variety of data collection methods can be used to gather spatial information on sharks and rays. Any information which is accompanied by a date, species identification and GPS coordinates can be inputed here provided it comes from a verified source."),
                                 h1("Data Privacy and Data sharing"),
                                 p("Sharing hard-earned datasets is often a daunting prospect. It is important to know that this app does not allow data downloads of any kind nor does it provide storage of datasets. The aim is to provide a platform designed exclusively for visualization and information-sharing purposes."),
                                 h1("Data resolution"),
                                 p("This app allows collaborators to remain in control of the resolution at which they wish to display their spatial information. Some datasets may be more sensitive than others but even coarser scales can still contribute valuable insights to the collective research community. Please see the species spatial information tab to visualise examples of this.")
                                 
                          )
                        ))),
    
    # FIRST TAB
    tabPanel("Marine Protected Areas",
             
             # FIRST ROW
             fluidRow(
               column(3,
                      h4(""),
                      HTML("<h4><b4>This is an interactive map of South Africa's Marine Protected Area (MPA) network.<br><br>
                           Please refer to the definition boxes below for information on activities permitted in each zone.
                           <br><br>You can also click to turn the two major zonation types (No-take and Mixed-use) on and off at the top right of the map.
                           <br><br>Finally, If you scroll down to the bottom of this page, you can get the list of zone names for individual MPAs.</b></h4>"),
                      h4(tags$b("Source of spatial information below:")),
                      a("South Africa Marine Protected Area Zonations (SAMPAZ_OR_2023_Q2), Department of Environmental Affairs",href ="https://egis.environment.gov.za/data_egis/data_download/current")
               ),
               column(9, align = "center", column(12,leafletOutput("mpas_sa",width = "80%", height = "50vh"),
                                                  actionButton("resetButton", "Reset Zoom")))
             ),
             
             # NEW SECOND ROW
             fluidRow(
               tags$head(
                 tags$style(
                   HTML("
      .custom-well {
        max-height: 260px; /* Adjust the maximum height as needed */
        overflow-y: auto; /* Add a vertical scrollbar if content exceeds max-height */
      }
    ")
                 )
               ),
               column(width = 3,
                      br(), br(), br(), br(),
                      wellPanel(
                        class = "custom-well",
                        div(style = "background-color: lightblue; padding: 10px;",
                            HTML("<h4><b>Wilderness, Sanctuary, Restricted</b></h4>"),
                            HTML("<p>These are <b>no-take</b> zones and no extraction is permitted (fishing, mining). Each MPA's indivudal gazette will detail which (if any) non-extractive activities may be permitted i.e. SCUBA.</p>")
                        ))),
               column(width = 3,
                      br(), br(), br(), br(),
                      wellPanel(
                        class = "custom-well",
                        div(style = "background-color: rgba(128,0,128,0.7); padding: 10px;",
                            HTML("<h4><b>Controlled</b></h4>"),
                            HTML("<p>Fishing is permitted. Gear type: specified in MPA gazette. Target species: specified in MPA gazette. Some of the MPAs designated prior to 2019 do not have more specific names for their controlled zones.</p>")
                        ))),
               column(width = 3,
                      br(), br(), br(), br(),
                      wellPanel(
                        class = "custom-well",
                        div(style = "background-color: hotpink; padding: 10px;",
                            HTML("<h4><b>Controlled-Pelagic Linefish with List</b></h4>"),
                            HTML("<p> Fishing is permitted. Gear type: pelagic linefishing (handline <b>or</b> manually operating a rod, reel and line <b>or</b> one or more separate lines to which no more than ten hooks are attached per line). Target species: specified in MPA gazette.</p>")
                        ))),
               column(width = 3,
                      br(), br(), br(), br(),
                      wellPanel(
                        class = "custom-well",
                        div(style = "background-color: lightpink; padding: 10px;",
                            HTML("<h4><b>Controlled Large Pelagic</b></h4>"),
                            HTML("<p> Fishing is permitted. Gear type: pelagic longline. Target species: specified in MPA gazette.</p>")
                        )))
             ),
             fluidRow(
               column(width = 3,
                      br(), br(), br(), br(),
                      wellPanel(
                        class = "custom-well",
                        div(style = "background-color: green; padding: 10px;",
                            HTML("<h4><b>Controlled Catch and Release</b></h4>"),
                            HTML("<p> Fishing is permitted. Gear type: linefishing using barbless hooks only. Target species: specified in MPA gazette. Important: All fish must be carefully handled and released alive and unharmed back into the water from which it was caught.</p>")
                        )))),
             
             # SECOND ROW
             fluidRow(
               column(4,  # list of MPA inputs
                      selectInput("filtermpa", 
                                  "Select an MPA:", 
                                  sort(unique(shapefile_data_simple$CUR_NME))))
             ),
             
             # THIRD ROW
             fluidRow( # row 2
               column(4, style='padding-left:10px; padding-right:10px; padding-top:10px; padding-bottom:10px',
                      #This is the mpa mpa
                      leafletOutput("mpa_single")),
               column(8, DT::dataTableOutput("tabletest"))
             )),
    
    # SECOND TAB
    tabPanel("Sharks and Rays in South Africa - overview",
             # Create a new Row in the UI for selectInputs
             fluidRow(
               column(12,
                      h4(""),
                      HTML("<h4><b4> Below is a list of all 194 species found in South Africa.<br>The information used to create this table has come from different sources and is referenced at the bottom of the page.<br>For each species, information on its IUCN Red List status as well as endemic status is provided.</b></h4>")
               )),
             fluidRow(
               column(4,
                      selectInput("group",
                                  "Group:",
                                  c("All",
                                    unique(as.character(master$group))))
               ),
               column(4,
                      selectInput("redlist",
                                  "Red List Status:",
                                  c("All",
                                    unique(as.character(master$STATUS))))
               ),
               column(4,
                      selectInput("endemic",
                                  "Endemic Status:",
                                  c("All","Southern Africa","South Africa","Not endemic"))
               )
             ),
             DT::dataTableOutput("table"),
             
             
             fluidRow(
               column(12,
                      h4(""),
                      HTML("<h4><b4>Sources of information:</b></h4>"),
                      HTML("<p>Species list:<p>"),
                      a("Cliff, G. 2022. South African Shark and Ray Species Database. WILDTRUST, Shark Attack Campaign, Link. Accessed on 18/10/2023.",href ="http://sharksunderattackcampaign.co.za/resources/"),
                      HTML("<p>Species Distribution Models:<p>"),
                      a("Faure-Beaulieu N.,(2023). A systematic conservation plan identifying critical areas for improved chondrichthyan protection in South Africa. Biological Conservation, 284, 110163. https://doi.org/10.1016/j.biocon.2023.110163",href ="https://doi.org/10.1016/j.biocon.2023.110163"),
                      HTML("<p>IUCN ranges:<p>"),
                      a("IUCN. 2021. The IUCN Red List of Threatened Species. Version 2021-2.",href ="https://www.iucnredlist.org")
               ))
    ),
    
    # THIRD TAB
    tabPanel("MPA species list", 
             
             fluidRow(
               column(12,
                      h4(""),
                      HTML("<h4>Use the dropdown below to get a list of species per Marine Protected Area.<br><br>The list is built by using three different sources of information to assess the presence of a species within the MPA:<br>1. Species Distribution Model Ranges<br>2.IUCN Red List Ranges<br>3.True occurrence records from verified research sources (row highlighted in green)
                             <br><br><b>IMPORTANT:</b> This does not represent a complete and comprehensive list of species for an MPA, but is rather meant to show the most recent (or provided) sighting information in a any particular MPA</h4>")
                      
               )),
             
             fluidRow(
               column(3,# list of MPA inputs
                      selectInput("selectmpa2",
                                  "Select an MPA:",
                                  c(sort(unique(shapefile_data_simple$CUR_NME)))))),
             
             
             fluidRow( # row 2
               column(3, style='padding-left:10px; padding-right:10px; padding-top:10px; padding-bottom:10px',
                      #This is the mpa mpa
                      leafletOutput("mpa_single2")),
               column(9,DT::dataTableOutput("species_permpa")),
               style = "margin-bottom: 20px;" 
               
             ),
             fluidRow(
               column(3,DT::dataTableOutput("species_permpasummary")),
               column(9,
                      wellPanel(
                        class = "custom-well",
                        div(style = "background-color: rgba(173,216,230,0.7); padding: 5px;",
                            HTML("<h4>On the left is a summary of the total number of shark and ray species by data type.<br> Below are bar plots shwoing number of species per IUCN red list status or endemic status per data type</h4>")))
               )),
             fluidRow(
               column(12, plotOutput("species_permpa_barchart1"))
             ),
             fluidRow(
               column(12, plotOutput("species_permpa_barchart2"))
             ),
             fluidRow(
               column(12,
                      h4(""),
                      HTML("<h4><b4>Sources of information:</b></h4>"),
                      HTML("<p>Species Occurence data:<p>"),
                      HTML("Currently only BRUV and ACOUSTIC TAGGING data is included as the app is being piloted"),
                      HTML("<p>Species Distribution Models:<p>"),
                      a("Faure-Beaulieu N.,(2023). A systematic conservation plan identifying critical areas for improved chondrichthyan protection in South Africa. Biological Conservation, 284, 110163. https://doi.org/10.1016/j.biocon.2023.110163",href ="https://doi.org/10.1016/j.biocon.2023.110163"),
                      HTML("<p>IUCN ranges:<p>"),
                      a("IUCN. 2021. The IUCN Red List of Threatened Species. Version 2021-2.",href ="https://www.iucnredlist.org")
               ))),
    
    # FOURTH TAB
    tabPanel("Species spatial information", 
             fluidRow(
               column(12,
                      h4(""),
                      HTML("<h4><b4>This is an interactive map displaying spatial information for South Africa's shark and ray species.<br><br>
                           Data available for the species is currently shown at a <b>10 x 10 km resolution</b>. This means that the species occurrance record is located somewhere within the 10 x 10 km cell.
                           <br><br> For each green cell, information on the most recent sighting within that cell is shown, as well as date and owner. The darker the green the more recent the sighting.
                             <br><br><b>IMPORTANT:</b> This does not represent the complete set of occurrence data for this species, but is rather meant to show the most recent (or provided) sighting information for a species occurrence in a any particular cell </b></h4>")
                      
               )),
             fluidRow(column(4,selectizeInput("selectspecies", h4("Select a species"),choices = sort(str_to_sentence(unique(overlap_shortened$SPECIES_SCIENTIFIC))), multiple=FALSE)
             )),
             fluidRow(align = "center",
                      
                      column(2, # Adjust the column width as needed
                             # Text
                             HTML("<h4><b>West/East extents<br>(from data)</b></h4>"),
                             # Image
                             div(img(src = "boundary_cross.png"))),
                      column(10,leafletOutput("rasters_sa", width = "80%", height = "50vh"),
                             actionButton("resetButton2", "Reset Zoom")
                      )),
             # life history information and species images
             fluidRow(
               column(4,
                      selectInput("illustration",
                                  "Select a species:",
                                  choices = sort(str_to_sentence(unique(overlap_shortened$SPECIES_SCIENTIFIC)))
                      ))),
             fluidRow(align = "left",
                      column(4,imageOutput("display_illustration")),
                      column(8,DT::dataTableOutput("species_LH"))
             ),
             fluidRow(
               column(12,
                      h4(""),
                      HTML("<h4><b4>Sources of information:</b></h4>"),
                      HTML("<p><b>Shark and Ray Illustrations:</b><p>"),
                      HTML("All illustrations were kindly provided by Ann Hecht"),
                      HTML("<p><b>Species Occurence data:</b><p>"),
                      HTML("Currently only BRUV and ACOUSTIC TAGGING data is included as the app is being piloted"),
                      HTML("<p><b>Species Distribution Models:</b><p>"),
                      a("Faure-Beaulieu N.,(2023). A systematic conservation plan identifying critical areas for improved chondrichthyan protection in South Africa. Biological Conservation, 284, 110163. https://doi.org/10.1016/j.biocon.2023.110163",href ="https://doi.org/10.1016/j.biocon.2023.110163"),
                      HTML("<p><b>IUCN ranges:</b><p>"),
                      a("IUCN. 2021. The IUCN Red List of Threatened Species. Version 2021-2.",href ="https://www.iucnredlist.org")
               ))
    )
    
  ),
  # Add inline CSS rule to align legend items to the left
  tags$style(HTML(".info { text-align: left }"))  
)
#####################  DEFINING THE UI (USER INTERFACE)


#####################  DEFINING THE SERVER LOGIC
# Define server logic
server <- function(input, output) {
  
  
  #shinyalert("Hello! Please read:", "Please note that this app is in development. It will be live during South Africa's 2023 Shark and Ray Symposium to allow delegates to test it. The server used to host it has a maximum free allowance of 25h per month, once that is exceeded the app will no longer be online so please close the app from your browser once you are finished :)", type = "info")
  
  
  # FIRST TAB
  output$mpas_sa <- renderLeaflet({
    
    mpas_sa <- leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng=20.16181,lat=-33, zoom = 5) %>%
      addPolygons(data = eez_sa,weight = 1, color = "grey",fillColor = "white",fillOpacity = 0)%>%
      addPolylines(data = contours,weight = 1, color = "grey",label = ~DEPTH,popup = ~DEPTH,fillOpacity = 0)%>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Restricted","Sanctuary","Wilderness")),],weight = 1,popup = ~CUR_NME,label = ~CUR_NME, color = "Blue",fillColor = "skyblue",fillOpacity = 0.6, group = "No-take zones")%>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Controlled Catch and Release")),],weight = 1,label = ~CUR_NME,popup = ~CUR_NME, color = "green4",fillColor = "green",fillOpacity = 0.6,group = "Mixed-use zones")%>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Controlled")),],weight = 1,label = ~CUR_NME,popup = ~CUR_NME, color = "Purple",fillColor = "purple",fillOpacity = 0.6,group = "Mixed-use zones")%>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Controlled-Pelagic Linefish with list")),],weight = 1,label = ~CUR_NME,popup = ~CUR_NME, color = "Purple",fillColor = "hotpink",fillOpacity = 0.6,group = "Mixed-use zones")%>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Controlled Large Pelagic")),],weight = 1,label = ~CUR_NME, popup = ~CUR_NME,color = "Purple",fillColor = "lightpink",fillOpacity = 0.6,group = "Mixed-use zones")%>%
      addLegend("bottomleft",title = "Zone Types", colors = c("skyblue","purple","hotpink","lightpink","green"), opacity = 1,
                labels = c("Wilderness, Sanctuary or Restricted (No-take zones)","Controlled (Mixed-use zones)","Controlled-Pelagic Linefish with List (Mixed-use zones)","Controlled Large Pelagic (Mixed-use zones)","Controlled Catch and Release (Mixed-use zone, only in iSimangaliso MPA - KZN)")) %>%
      addLayersControl(overlayGroups = c("No-take zones","Mixed-use zones"), options = layersControlOptions(collapsed = FALSE,autoZIndex = TRUE))%>%
      fitBounds(initialBounds$lng1, initialBounds$lat1, initialBounds$lng2, initialBounds$lat2)
  })
  
  # Reset the map bounds when the button is clicked
  observeEvent(input$resetButton, {
    leafletProxy("mpas_sa") %>%
      fitBounds(initialBounds$lng1, initialBounds$lat1, initialBounds$lng2, initialBounds$lat2)
  })
  
  # FIRST TAB
  # this looks for the MPA outline
  output$mpa_single <- renderLeaflet({
    
    # extract single mpa
    single_mpa = shapefile_data_simple[which(shapefile_data_simple$CUR_NME == input$filtermpa),]
    # extract centroid for that mpa
    mpa_centroid = st_centroid(st_union(single_mpa))
    mpa_centroid = unlist(mpa_centroid)
    # get all other mpas to add thin opaque layer above them
    mpa_single = shapefile_data_simple[which(shapefile_data_simple$CUR_NME == input$filtermpa),]
    
    mpa_single <- leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(mpa_centroid[1],mpa_centroid[2],8) %>%
      addPolylines(data = contours,weight = 1, color = "grey",label = ~DEPTH,popup = ~DEPTH,fillOpacity = 0)%>%
      addPolygons(data = mpa_single,fillColor = "white",color = "white", opacity =1) %>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Restricted","Sanctuary","Wilderness")),],weight = 1,label = ~CUR_ZON_NM, color = "Blue",fillColor = "skyblue",fillOpacity = 0.6 )%>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Controlled Catch and Release")),],weight = 1,label = ~CUR_ZON_NM, color = "green4",fillColor = "green",fillOpacity = 0.6 )%>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Controlled")),],weight = 1,label = ~CUR_ZON_NM,fillColor = "purple",fillOpacity = 0.6 )%>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Controlled-Pelagic Linefish with list")),],weight = 1,label = ~CUR_ZON_NM, color = "Purple",fillColor = "hotpink",fillOpacity = 0.6 )%>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Controlled Large Pelagic")),],weight = 1,label = ~CUR_ZON_NM, color = "Purple",fillColor = "lightpink",fillOpacity = 0.6 )
    
    
  })
  
  # FIRST TAB
  output$tabletest <- DT::renderDataTable(DT::datatable({
    shapefile_data_no_geom <- st_drop_geometry(shapefile_data_simple)
    shapefile_data_no_geom %>%
      filter(CUR_NME == input$filtermpa)%>%
      rename("Zone Name" = CUR_ZON_NM)%>%
      rename("Zone Type" = CUR_ZON_TY)%>%
      rename("Zone Code" = CUR_ZON_CD)%>%
      rename("Declaration date" = D_DCLAR)%>%
      mutate(GIS_AREA = round(GIS_AREA,0))%>%
      mutate(area_km2 = round(area_km2,0))%>%
      rename("Area (Ha)" = GIS_AREA)%>%
      rename("Area (Km2)" = area_km2)%>%
      dplyr::select(-CUR_NME)
    
  }))
  
  
  # SECOND TAB
  # master table of species
  output$table = DT::renderDataTable({
    
    data = master%>%
      rename("IUCN range available?" = iucn) %>%
      rename("Species Distribution Model produced?" = sdm)%>%
      mutate(SPECIES_SCIENTIFIC = str_to_sentence(SPECIES_SCIENTIFIC))%>%
      rename("Common name" = Species_common)%>%
      rename("Scientific name" = SPECIES_SCIENTIFIC)%>%
      rename("IUCN Red List Status" = STATUS)%>%
      rename("Endemic status" = ENDEMIC.STATUS)%>%
      rename("Group" = group)
    
    if(input$group != "All"){ data = data[data$Group == input$group,]}
    if(input$redlist != "All"){ data = data[data$`IUCN Red List Status` == input$redlist,]}
    if(input$endemic != "All"){ data = data[data$`Endemic status`== input$endemic,]}
    
    datatable(data, filter = "top",options = list(pageLength = 194, searching = TRUE)) %>%
      formatStyle(
        'IUCN Red List Status',
        backgroundColor = styleEqual(unique(master$STATUS), status_colors)
        
      ) %>%
      formatStyle(
        'Endemic status',
        backgroundColor = styleEqual(unique(master$ENDEMIC.STATUS), endemic_colors)
      )
    
  })
  
  # SECOND TAB
  # Create a reactive that dynamically changes the illustration source
  selected_illustration <- reactive({
    selected_value <- input$illustration
    
    # Define a list of illustration file names corresponding to the choices
    illustration_files <- list.files(path = "data_objects/ILLUSTRATIONS/",recursive = TRUE,full.names = TRUE)
    
    # Get the path of the selected illustration
    selected_illustration_path <-  illustration_files[str_detect(illustration_files,selected_value)]
    
    return(selected_illustration_path)
  })
  
  output$display_illustration <- renderImage({
    list(src = selected_illustration(),height = "auto", width = "70%")
  },deleteFile=F)
  
  output$species_LH <- DT::renderDataTable({
    
    temp  = lifehistory %>%
      filter(SPECIES_SCIENTIFIC == input$illustration)
    
  })
  
  # THIRD TAB
  # summary of species per mpa
  output$species_permpasummary <- DT::renderDataTable({
    temp = species_overlapdata  %>%
      filter(CUR_NME == input$selectmpa2)%>% 
      dplyr::select(-iucn) %>%
      dplyr::select(-sdm)%>%
      dplyr::select(-range_in_area) %>%
      dplyr::select(-speciesrange) %>%
      pivot_wider(values_from = perc_in_area,
                  names_from = type)
    
    # only remove if NA in both
    temp = temp %>%
      group_by(CUR_NME,SPECIES_SCIENTIFIC,Species_common,STATUS,ENDEMIC.STATUS,group)%>%
      summarize(across(starts_with("iucn"),~na.omit(.)[1]),
                across(starts_with("sdm"),~na.omit(.)[1]))
    
    temp = ungroup(temp)
    temp = temp %>%
      mutate(SPECIES_SCIENTIFIC = str_to_sentence(SPECIES_SCIENTIFIC))%>%
      rename("Scientific name" = SPECIES_SCIENTIFIC)%>%
      rename("Common name" = Species_common)%>%
      rename("IUCN status" = STATUS)%>%
      rename("Endemic status" = ENDEMIC.STATUS)%>%
      rename("Group" = group)%>%
      dplyr::select(-CUR_NME)%>%
      filter(!(iucn == 0 & sdm == 0)) %>%
      mutate(iucn = ifelse(iucn>0, "Yes","No"))%>%
      mutate(sdm = ifelse(sdm>0, "Yes","No")) %>%
      mutate(sdm = ifelse(is.na(sdm),"No model",sdm))
    
    # add in most recent sightings AFTER filtering sightings by MPA also
    mostrecent_sighting_temp = mostrecent_sighting %>%
      filter(CUR_NME== input$selectmpa2)
    mostrecent_sighting_temp$CUR_NME = NULL
    mostrecent_sighting_temp$GENUS = NULL
    mostrecent_sighting_temp$SPECIES = NULL
    
    # clean up column names
    colnames(mostrecent_sighting_temp) = str_to_sentence(colnames(mostrecent_sighting_temp))
    
    # join to species list
    temp = left_join(temp,mostrecent_sighting_temp)
    
    test1 = temp %>%
      group_by(Group,iucn) %>%
      summarise(n())%>%
      filter(iucn == "Yes")%>%
      ungroup()
    test1$iucn = test1$`n()`
    test1$`n()` = NULL
    test2 = temp %>%
      group_by(Group,sdm) %>%
      summarise(n())%>%
      filter(sdm == "Yes")%>%
      ungroup()
    test2$sdm = test2$`n()`
    test2$`n()` = NULL
    test3 = temp %>%
      group_by(Group,Datatype) %>%
      summarise(count = n())%>%
      filter(!is.na(Datatype))%>%
      group_by(Group)%>%
      summarise(sum(count))
    test3$record = test3$`sum(count)`
    test3$`sum(count)` = NULL
    all = left_join(test1,test2)
    all = left_join(all,test3)
    
    datatable(all,rownames = FALSE,options = list(searching = FALSE,autoWidth=TRUE, lengthChange = FALSE,info=FALSE,paging=FALSE))
    
  })
  
  
  # MPA overview info
  output$species_permpa <- DT::renderDataTable({
    
    temp = species_overlapdata  %>%
      filter(CUR_NME == input$selectmpa2)%>% 
      dplyr::select(-iucn) %>%
      dplyr::select(-sdm)%>%
      dplyr::select(-range_in_area) %>%
      dplyr::select(-speciesrange) %>%
      #mutate(perc_in_area = round(100*perc_in_area,2))%>%
      pivot_wider(values_from = perc_in_area,
                  names_from = type)
    
    # only remove if NA in both
    temp = temp %>%
      group_by(CUR_NME,SPECIES_SCIENTIFIC,Species_common,STATUS,ENDEMIC.STATUS,group)%>%
      summarize(across(starts_with("iucn"),~na.omit(.)[1]),
                across(starts_with("sdm"),~na.omit(.)[1]))
    
    temp = ungroup(temp)
    temp = temp %>%
      mutate(SPECIES_SCIENTIFIC = str_to_sentence(SPECIES_SCIENTIFIC))%>%
      rename("Scientific name" = SPECIES_SCIENTIFIC)%>%
      rename("Common name" = Species_common)%>%
      rename("IUCN status" = STATUS)%>%
      rename("Endemic status" = ENDEMIC.STATUS)%>%
      rename("Group" = group)%>%
      dplyr::select(-CUR_NME)%>%
      filter(!(iucn == 0 & sdm == 0)) %>%
      mutate(iucn = ifelse(iucn>0, "Yes","No"))%>%
      mutate(sdm = ifelse(sdm>0, "Yes","No")) %>%
      mutate(sdm = ifelse(is.na(sdm),"No model",sdm))
    temp
    
    # add in most recent sightings AFTER filtering sightings by MPA also
    mostrecent_sighting_temp = mostrecent_sighting %>%
      filter(CUR_NME== input$selectmpa2)
    mostrecent_sighting_temp$CUR_NME = NULL
    mostrecent_sighting_temp$GENUS = NULL
    mostrecent_sighting_temp$SPECIES = NULL
    
    # clean up column names
    colnames(mostrecent_sighting_temp) = str_to_sentence(colnames(mostrecent_sighting_temp))
    
    # join to species list
    temp = left_join(temp,mostrecent_sighting_temp)
    
    # find columns with non empty values
    options = unique(temp$Datatype)
    naposition = which(is.na(options))
    options = options[-naposition]
    
    datatable(temp, filter = "top", options = list(pageLength=10,autoWidth=TRUE,searching=FALSE))%>%
      formatStyle(
        columns = 'Datatype',
        target = 'row',
        valueColumns = 'Datatype',
        backgroundColor = styleEqual(options,'lightgreen')
      )
  }) 
  
  # FIRST TAB
  # this looks for the MPA outline
  output$mpa_single2 <- renderLeaflet({
    
    # extract single mpa
    single_mpa = shapefile_data_simple[which(shapefile_data_simple$CUR_NME == input$selectmpa2),]
    # extract centroid for that mpa
    mpa_centroid = st_centroid(st_union(single_mpa))
    mpa_centroid = unlist(mpa_centroid)
    # get all other mpas to add thin opaque layer above them
    mpa_single = shapefile_data_simple[which(shapefile_data_simple$CUR_NME == input$selectmpa2),]
    
    mpa_single <- leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(mpa_centroid[1],mpa_centroid[2],8) %>%
      addPolygons(data = mpa_single,fillColor = "white",color = "white", opacity =1) %>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Restricted","Sanctuary","Wilderness")),],weight = 1,label = ~CUR_ZON_NM, color = "Blue",fillColor = "skyblue",fillOpacity = 0.6 )%>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Controlled Catch and Release")),],weight = 1,label = ~CUR_ZON_NM, color = "green4",fillColor = "green",fillOpacity = 0.6 )%>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Controlled")),],weight = 1,label = ~CUR_ZON_NM,fillColor = "purple",fillOpacity = 0.6 )%>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Controlled-Pelagic Linefish with list")),],weight = 1,label = ~CUR_ZON_NM, color = "Purple",fillColor = "hotpink",fillOpacity = 0.6 )%>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Controlled Large Pelagic")),],weight = 1,label = ~CUR_ZON_NM, color = "Purple",fillColor = "lightpink",fillOpacity = 0.6 )
    #addLegend("bottomleft",title = "Zone Types", colors = c("skyblue","purple","hotpink","lightpink","green"), opacity = 1,
    #        labels = c("Wilderness, Sanctuary or Restricted (No-take zones)","Controlled (Mixed-use zones)","Controlled-Pelagic Linefish with List (Mixed-use zones)","Controlled Large Pelagic (Mixed-use zones)","Controlled Catch and Release (Mixed-use zone, only in iSimangaliso)"))
    
    
  })
  
  # THIRD TAB
  # bar chart
  output$species_permpa_barchart1 <- renderPlot({ 
    
    # Specify the desired order of levels
    temp = species_overlapdata  %>%
      filter(CUR_NME == input$selectmpa2)%>% 
      dplyr::select(-iucn) %>%
      dplyr::select(-sdm)%>%
      dplyr::select(-range_in_area) %>%
      dplyr::select(-speciesrange) %>%
      filter(perc_in_area>0)
    
    # total for the MPA
    totals <- temp%>%
      group_by(type) %>%
      summarise(total_spp2 = n())
    
    # Specify the desired order of levels
    desired_order <- c("CR", "EN", "VU","NT","LC","DD")
    # Use the factor() function to reorder the levels
    temp$STATUS <- factor(temp$STATUS, levels = desired_order)
    
    # Specify the desired order of levels
    desired_order <- c("South Africa", "Southern Africa", "Not endemic")
    # Use the factor() function to reorder the levels
    temp$ENDEMIC.STATUS <- factor(temp$ENDEMIC.STATUS, levels = desired_order)
    
    
    # GRAPH 1: NUMBER BY GROUP AND STATUS
    # Create a bar plot using ggplot2
    temp1 = temp %>%
      group_by(STATUS,group,type,ENDEMIC.STATUS)%>%
      summarise(total_spp2 = n())
    
    ggplot(temp1, aes(x = STATUS,y = total_spp2)) +
      geom_col(aes(fill = group))+
      facet_grid(~type)+
      labs(title = "Number of species who's ranges overlap with the MPA by IUCN status", y = "Number of species") +
      scale_fill_manual(values = c("grey", "lightblue","darkblue"),name = "")+
      xlab("")+
      theme_classic()+
      #scale_y_continuous(breaks = seq(0, 100, 5))+
      theme(text = element_text(size = 20))
  })
  
  # THIRD TAB
  # bar chart
  output$species_permpa_barchart2 <- renderPlot({ 
    
    # Specify the desired order of levels
    temp = species_overlapdata  %>%
      filter(CUR_NME == input$selectmpa2)%>% 
      dplyr::select(-iucn) %>%
      dplyr::select(-sdm)%>%
      dplyr::select(-range_in_area) %>%
      dplyr::select(-speciesrange) %>%
      filter(perc_in_area>0)
    
    # total for the MPA
    totals <- temp%>%
      group_by(type) %>%
      summarise(total_spp2 = n())
    
    # Specify the desired order of levels
    desired_order <- c("CR", "EN", "VU","NT","LC","DD")
    # Use the factor() function to reorder the levels
    temp$STATUS <- factor(temp$STATUS, levels = desired_order)
    
    # Specify the desired order of levels
    desired_order <- c("South Africa", "Southern Africa", "Not endemic")
    # Use the factor() function to reorder the levels
    temp$ENDEMIC.STATUS <- factor(temp$ENDEMIC.STATUS, levels = desired_order)
    
    
    # GRAPH 2: NUMBER BY GROUP AND ENDEMIC STATUS
    # Create a bar plot using ggplot2
    temp1 = temp %>%
      group_by(STATUS,group,type,ENDEMIC.STATUS)%>%
      summarise(total_spp2 = n())
    
    ggplot(temp1, aes(x = ENDEMIC.STATUS,y = total_spp2)) +
      geom_col(aes(fill = group))+
      facet_grid(~type)+
      labs(title = "Number of species who's ranges overlap with the MPA by Endemic status", y = "Number of species") +
      scale_fill_manual(values = c("grey", "lightblue","darkblue"),name = "")+
      xlab("")+
      theme_classic()+
      #scale_y_continuous(breaks = seq(0, 100, 5))+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
      theme(text = element_text(size = 20))
    
  })
  
  # FOURTH TAB
  output$rasters_sa <- renderLeaflet({
    
    # extract raster
    names_all = str_replace(names(all_distributions),"\\."," ")
# piece of logic to make sure temp object is created only if there is a distribution model
        if(length(unique(str_detect(names_all,toupper(input$selectspecies))))>1){
      temp = all_distributions[[which(str_detect(names_all,toupper(input$selectspecies)))]]
    }
    
    # extract IUCN polygon
    temp2 = iucn_file_list[[which(str_detect(names(iucn_file_list), toupper(input$selectspecies)))]]

    # extract expert extent
    temp3 = expert_extent %>%
      filter(SPECIES_SCIENTIFIC == toupper(input$selectspecies)) %>% 
      group_by(SPECIES_SCIENTIFIC,geometry,Boundary_west,Boundary_east)%>%
      summarise()
    
    # extract boundaries
    temp4 = boundaries[which(boundaries$SPECIES_SCIENTIFIC == toupper(input$selectspecies)),]
    
    # extract high res locations
    temp5 = highres_gps[which(highres_gps$SPECIES_SCIENTIFIC == toupper(input$selectspecies)),]
    
    # get common name
    name = master %>%
      filter(SPECIES_SCIENTIFIC == toupper(input$selectspecies))
    name = name$Species_common
    
    # plot
    map = leaflet() %>% 
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng=20.16181,lat=-33, zoom = 5) %>%
      # EEZ
      addPolygons(data = eez_sa,weight = 1, color = "grey",fillColor = "white",fillOpacity = 0)%>%
      addPolylines(data = contours,weight = 1, color = "grey",label = ~DEPTH,popup = ~DEPTH,fillOpacity = 0)%>%
      # MPA ZONES
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Restricted","Sanctuary","Wilderness")),],weight = 1,label = ~CUR_NME,popup = ~CUR_NME, color = "Blue",fillColor = "skyblue",fillOpacity = 0.6, group = "No-take zones")%>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Controlled Catch and Release")),],weight = 1,label = ~CUR_NME,popup = ~CUR_NME, color = "green4",fillColor = "green",fillOpacity = 0.6,group = "Mixed-use zones")%>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Controlled")),],weight = 1,label = ~CUR_NME,popup = ~CUR_NME, color = "Purple",fillColor = "purple",fillOpacity = 0.6,group = "Mixed-use zones")%>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Controlled-Pelagic Linefish with list")),],weight = 1,label = ~CUR_NME,popup = ~CUR_NME, color = "Purple",fillColor = "hotpink",fillOpacity = 0.6,group = "Mixed-use zones")%>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Controlled Large Pelagic")),],weight = 1,label = ~CUR_NME, popup = ~CUR_NME,color = "Purple",fillColor = "lightpink",fillOpacity = 0.6,group = "Mixed-use zones")%>%
      # IUCN
      addPolygons(data = temp2, weight = 2,color = "Yellow", group = "IUCN range",opacity = 1) %>%
      # LEGEND
      addLegend("bottomleft",title = "MPA zone types", colors = c("skyblue","purple","hotpink","lightpink","green"), opacity = 1,
                labels = c("Wilderness, Sanctuary or Restricted (No-take zones)","Controlled (Mixed-use zones)","Controlled-Pelagic Linefish with List (Mixed-use zones)","Controlled Large Pelagic (Mixed-use zones)","Controlled Catch and Release (Mixed-use zone, only in iSimangaliso)")) %>%
      addLegend(colors = c("yellow"),labels = c("IUCN range"))%>%
      # LAYER CONTROL
      addLayersControl(overlayGroups = c("No-take zones","Mixed-use zones","IUCN range","Distribution models"),options = layersControlOptions(collapsed = FALSE,autoZIndex = TRUE))%>%
      fitBounds(initialBounds$lng1, initialBounds$lat1, initialBounds$lng2, initialBounds$lat2)
    
    # SDM if it exists
    if(exists("temp")){
      values(temp) = values(temp)/1000
      pal <- colorBin("Reds", domain = 0:1,na.color = "#00000000")
      map = 
        map %>%
        addRasterImage(temp, colors = pal, opacity = 0.5,group = "Distribution models")%>%
        addLegend(title = "Distrubion model (probability of occurrence)",values = values(temp),pal = pal)}else{map}
    
    # expert extent if it exists
    if(nrow(temp3)>0){
      map = 
        map %>%
        addMarkers(lng = st_coordinates(st_as_sf(temp3))[,1], lat = st_coordinates(st_as_sf(temp3),colors = "green")[,2])}else{map}
    
    # boundaries if exists
    if(nrow(temp4)>0){
      map = 
        map %>%
        addMarkers(lng = st_coordinates(st_as_sf(temp4, coords = c("LONGITUDE","LATITUDE")))[,1], lat = st_coordinates(st_as_sf(temp4,coords = c("LONGITUDE","LATITUDE")))[,2], icon = icon_blue, popup = paste0("Year: ",temp4$YEAR,"<Br>Data type: ",temp4$DATATYPE,"<Br>Owner: ",temp4$OWNER))}else{map}
    
    # if data per cells exist
    if(nrow(temp5)>0){
      
      # convert to sf object
      temp5  = st_as_sf(temp5)
      
      # get palette for years
      pal <- colorNumeric(
        palette = "Greens",
        domain = temp5$most_recent)
      
      map = map %>%
        addPolygons(
          data = temp5,
          color = ~pal(most_recent),
          weight = 5,
          opacity = 1,
          popup = paste0("Year: ",temp5$most_recent,"<Br>Data type: ",temp5$DATATYPE,"<Br>Owner: ",temp5$OWNER))}else{map}
    
    
  })
  
  # Reset the map bounds when the button is clicked
  observeEvent(input$resetButton2, {
    leafletProxy("rasters_sa") %>%
      fitBounds(initialBounds$lng1, initialBounds$lat1, initialBounds$lng2, initialBounds$lat2)
  })
  
  
}
#####################  DEFINING THE SERVER LOGIC

# Run the application 
shinyApp(ui = ui, server = server)
#library(rsconnect)
#deployApp()
