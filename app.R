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
library(raster)
library(dplyr)
library(DT)
library(htmlwidgets)
library(leaflegend)

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
shapefile_data_simple = st_read(list.files(pattern = "shapefile_data_simple.shp",recursive = TRUE, full.names = TRUE))
correct_crs <- st_crs("+proj=longlat +datum=WGS84") # reproject to correct datum, syntax error
shapefile_data_simple <- st_transform(shapefile_data_simple, correct_crs)
rm(correct_crs)
##################### MPA DATA


##################### MPA OVERVIEW DATA
# Remove the geometry column
mpa_data_overview<- st_drop_geometry(shapefile_data_simple)

mpa_data_overview = shapefile_data_simple  %>%
  group_by(CUR_NME,D_DCLAR) %>%
  summarise(total_area_ha = round(sum(GIS_AREA),0),
            total_area_km2 = round(sum(area_km2),0))%>%
  rename("MPA Name" = CUR_NME)%>%
  rename("Declaration Date" = D_DCLAR)%>%
  rename("Total area (Ha)" = total_area_ha)%>%
  rename("Total area (Km2)" = total_area_km2)
mpa_data_overview$longitude = st_coordinates(st_centroid(mpa_data_overview))[,1]
mpa_data_overview$latitude = st_coordinates(st_centroid(mpa_data_overview))[,2]
mpa_data_overview = st_drop_geometry(mpa_data_overview)
##################### MPA OVERVIEW DATA


##################### EEZ DATA
eez_sa = st_read(list.files(pattern = "eez.shp",recursive = TRUE, full.names = TRUE))
##################### EEZ DATA


##################### MASTER SHEET
# Path
#file_path <- list.files(pattern = "data_summary_master.xlsx", recursive=TRUE,full.names = TRUE)
# File
master = readxl::read_xlsx( list.files(pattern = "data_summary_master.xlsx", recursive=TRUE,full.names = TRUE))
rm(file_path)
# remove variables that i added previously
master = master[,c(1:4,12)]
##################### MASTER SHEET


##################### SPECIES PER MPA DATA
# Path
file_path <- list.files(pattern = "species_permpa_byiusnandsdms.csv", recursive=TRUE,full.names = TRUE)
# File
species_overlapdata = read.csv(file_path)
rm(file_path)
##################### SPECIES PER MPA DATA


##################### RASTER DATA
# Path to data
files = list.files(path = "/home/nina/Dropbox/6-WILDOCEANS/1-ConservationPlan/wildoceans-scripts_github/Outputs/distribution_rasters/", pattern = "ensemblemean.tiff",full.names = TRUE)
# Read the data
all_distributions = stack(files)
##################### RASTER DATA


##################### POLYGON DATA FROM IUCN
# Path to data
files_iucn = list.files(path = "/home/nina/Dropbox/6-WILDOCEANS/wildoceans-scripts/IUCN/Sharks_rays_SA_raw/", pattern = ".gpkg",full.names = TRUE)
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


# Define UI for application 
ui <- fluidPage(
  
  theme = shinytheme("united"),
  
  #Jumbotrons are pretty, they make nice headers
  tags$div(class = "jumbotron text-center", style = "margin-bottom:0px;margin-top:0px",
           tags$h2(class = 'jumbotron-heading', stye = 'margin-bottom:0px;margin-top:0px', 'Sharks, Rays and MPAs'),
           p('Interactive website to visualise spatial data on sharks, rays and MPAs in SA')
  ),
  
    tabsetPanel(
      
      # FIRST TAB
      tabPanel("Marine Protected Areas",
               
               # FIRST ROW
               fluidRow(align = "center", column(12,leafletOutput("mpas_sa",width = "80%", height = "50vh"),
                                                 actionButton("resetButton", "Reset Zoom"))),
               
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
               DT::dataTableOutput("table")),
      
      # THIRD TAB
      tabPanel("MPA species list", 
               
               fluidRow(
                 column(3,# list of MPA inputs
                        selectInput("selectmpa2",
                                    "Select an MPA:",
                                   c(sort(unique(shapefile_data_simple$CUR_NME))))),
                 column(9,DT::dataTableOutput("species_permpa"))
                 ),
               fluidRow(
                 column(12, plotOutput("species_permpa_barchart1"))
               ),
               fluidRow(
                 column(12, plotOutput("species_permpa_barchart2"))
               )),
      
      # FOURTH TAB
      tabPanel("Species spatial information", 
               fluidRow(column(12,selectInput("selectspecies", h4("Select a species"),choices = sort(str_to_sentence(unique(overlap_shortened$SPECIES_SCIENTIFIC))), selected = "1")
               )),
               fluidRow(align = "center",column(12,leafletOutput("rasters_sa", width = "80%", height = "50vh")
                               ))
               )
      
               ),
  # Add inline CSS rule to align legend items to the left
  tags$style(HTML(".info { text-align: left }"))  
  )

# Define server logic
server <- function(input, output) {
  
  # FIRST TAB
  output$mpas_sa <- renderLeaflet({
    
    mpas_sa <- leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng=20.16181,lat=-33, zoom = 5) %>%
      addPolygons(data = eez_sa,weight = 1, color = "grey",fillColor = "white",fillOpacity = 0)%>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Restricted","Sanctuary","Wilderness")),],weight = 1,popup = ~CUR_NME,label = ~CUR_NME, color = "Blue",fillColor = "skyblue",fillOpacity = 0.6, group = "No-take zones")%>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Controlled Catch and Release")),],weight = 1,label = ~CUR_NME,popup = ~CUR_NME, color = "green4",fillColor = "green",fillOpacity = 0.6,group = "Mixed-use zones")%>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Controlled")),],weight = 1,label = ~CUR_NME,popup = ~CUR_NME, color = "Purple",fillColor = "purple",fillOpacity = 0.6,group = "Mixed-use zones")%>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Controlled-Pelagic Linefish with list")),],weight = 1,label = ~CUR_NME,popup = ~CUR_NME, color = "Purple",fillColor = "hotpink",fillOpacity = 0.6,group = "Mixed-use zones")%>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Controlled Large Pelagic")),],weight = 1,label = ~CUR_NME, popup = ~CUR_NME,color = "Purple",fillColor = "lightpink",fillOpacity = 0.6,group = "Mixed-use zones")%>%
      addLegend("bottomleft",title = "Zone Types", colors = c("skyblue","purple","hotpink","lightpink","green"), opacity = 1,
                labels = c("Wilderness, Sanctuary or Restricted (No-take zones)","Controlled (Mixed-use zones)","Controlled-Pelagic Linefish with List (Mixed-use zones)","Controlled Large Pelagic (Mixed-use zones)","Controlled Catch and Release (Mixed-use zone, only in iSimangaliso MPA - KZN)")) %>%
      addLayersControl(overlayGroups = c("No-take zones","Mixed-use zones"), options = layersControlOptions(collapsed = FALSE,autoZIndex = TRUE))%>%
      fitBounds(initialBounds$lng1, initialBounds$lat1, initialBounds$lng2, initialBounds$lat2)
      #addMarkers(lng = mpa_data_overview$longitude,lat = mpa_data_overview$latitude,popup = popupTable(mpa_data_overview)) 
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
      addPolygons(data = mpa_single,fillColor = "white",color = "white", opacity =1) %>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Restricted","Sanctuary","Wilderness")),],weight = 1,label = ~CUR_ZON_NM, color = "Blue",fillColor = "skyblue",fillOpacity = 0.6 )%>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Controlled Catch and Release")),],weight = 1,label = ~CUR_ZON_NM, color = "green4",fillColor = "green",fillOpacity = 0.6 )%>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Controlled")),],weight = 1,label = ~CUR_ZON_NM,fillColor = "purple",fillOpacity = 0.6 )%>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Controlled-Pelagic Linefish with list")),],weight = 1,label = ~CUR_ZON_NM, color = "Purple",fillColor = "hotpink",fillOpacity = 0.6 )%>%
      addPolygons(data = shapefile_data_simple[which(shapefile_data_simple$CUR_ZON_TY %in% c("Controlled Large Pelagic")),],weight = 1,label = ~CUR_ZON_NM, color = "Purple",fillColor = "lightpink",fillOpacity = 0.6 )
      #addLegend("bottomleft",title = "Zone Types", colors = c("skyblue","purple","hotpink","lightpink","green"), opacity = 1,
      #        labels = c("Wilderness, Sanctuary or Restricted (No-take zones)","Controlled (Mixed-use zones)","Controlled-Pelagic Linefish with List (Mixed-use zones)","Controlled Large Pelagic (Mixed-use zones)","Controlled Catch and Release (Mixed-use zone, only in iSimangaliso)"))
  

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
  output$table = DT::renderDataTable(DT::datatable({
    
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
    
    data
    
  }, 
  # set number of rows that are shown by default, i want the whole table to show so that's 194
  options = list(pageLength = 194))%>%
    # Color rows based on STATUS variable
    formatStyle(
      'IUCN Red List Status',
      backgroundColor = styleEqual(unique(master$STATUS), status_colors)
    )  %>% 
    # Color rows based on ENDEMIC STATUS variable
    formatStyle(
      'Endemic status',
      backgroundColor = styleEqual(unique(master$ENDEMIC.STATUS),endemic_colors)
    ) 
  )
  
  
  # THIRD TAB
  # MPA overview info
  output$species_permpa <- DT::renderDataTable(DT::datatable({
    
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
    
            }) )
   
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
      theme_minimal()+
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
      theme_minimal()+
      #scale_y_continuous(breaks = seq(0, 100, 5))+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
      theme(text = element_text(size = 20))
    
  })
  
  # FOURTH TAB
  output$rasters_sa <- renderLeaflet({
    
    # extract raster
    temp = all_distributions[[which(str_detect(files,toupper(input$selectspecies)))]]
   
    # extract IUCN polygon
    temp2 = files_iucn[str_detect(toupper(files_iucn), toupper(input$selectspecies))]
    temp2 = st_read(temp2)
    
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
      fitBounds(initialBounds$lng1, initialBounds$lat1, initialBounds$lng2, initialBounds$lat2)%>%
      addLabelOnlyMarkers(
        lng = initialBounds$lng1-10,  # Replace with your longitude
        lat =  initialBounds$lat1+1,  # Replace with your latitude
        label = paste("Common name:",name),
        labelOptions = labelOptions(noHide =TRUE)
      )
    
    # SDM if it exists
    if(nlayers(temp)>0){
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
   
}

# Run the application 
shinyApp(ui = ui, server = server)
