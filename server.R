# packages
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
library(readxl)
library(flextable)
library(shinyalert)


#####################  DEFINING THE SERVER LOGIC
# Define server logic
server <- function(input, output) {
  
  
  shinyalert("Hello! Please read:", "Please note that this app is in development and maps take about 5min to appear", type = "info")
  
  # HOME TAB
  output$Providers <-  renderUI({
    
    htmltools_value(flextable(providers)%>%
                      merge_v(j=c("Data type"))%>%
                      autofit()%>%
                      bg(i = ~`Data type`=="Acoustic tagging",bg = "thistle1")%>%
                      bg(i = ~`Data type`=="Linefishing (rod and reel)",bg = "snow")%>%
                      bg(i = ~`Data type`=="Remote Underwater Video (RUV)",bg = "wheat")%>%
                      bg(i = ~`Data type`=="Underwater Visual Survey (SCUBA or Freedive)",bg = "linen")%>%
                      bg(i = ~`Data type`=="Baited Remote Underwater Video (BRUV)",bg = "beige")%>%
                      border_inner_h()
    )
    
  })
  output$Institutions <-  renderUI({htmltools_value(institutions)})
  
  # FIRST TAB
  output$mpas_sa <- renderLeaflet({
    
   leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng=20.16181,lat=-33, zoom = 5) %>%
      addPolygons(data = eez_sa,weight = 1, color = "grey",fillColor = "white",fillOpacity = 0)%>%
      addPolylines(data = contours,weight = 1, color = "grey",label = ~DEPTH,popup = ~DEPTH,fillOpacity = 0)%>%
      addPolygons(data = notake,weight = 1,popup = ~CUR_NME,label = ~CUR_NME, color = "Blue",fillColor = "skyblue",fillOpacity = 0.6, group = "No-take zones")%>%
      addPolygons(data = ccr,weight = 1,label = ~CUR_NME,popup = ~CUR_NME, color = "green4",fillColor = "green",fillOpacity = 0.6,group = "Mixed-use zones")%>%
      addPolygons(data = c,weight = 1,label = ~CUR_NME,popup = ~CUR_NME, color = "Purple",fillColor = "purple",fillOpacity = 0.6,group = "Mixed-use zones")%>%
      addPolygons(data = cpl,weight = 1,label = ~CUR_NME,popup = ~CUR_NME, color = "Purple",fillColor = "hotpink",fillOpacity = 0.6,group = "Mixed-use zones")%>%
      addPolygons(data = clp,weight = 1,label = ~CUR_NME, popup = ~CUR_NME,color = "Purple",fillColor = "lightpink",fillOpacity = 0.6,group = "Mixed-use zones")%>%
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
  # SPECIES PER MPA
  output$species_permpa <- DT::renderDataTable({
    
    temp = compiled_species_list  %>%
      filter(CUR_NME == input$selectmpa2)
    
    # find rows with non empty values
    options = unique(temp$`Record type`)
    naposition = which(is.na(options))
    options = options[-naposition]
    
    datatable(temp, filter = "top", options = list(pageLength=50,autoWidth=TRUE,searching=TRUE))%>%
      formatStyle(
        columns = 'Record type',
        target = 'row',
        valueColumns = 'Record type',
        backgroundColor = styleEqual(options,'lightgreen')
      )
  }) 
  
  # THIRD TAB
  # MPAS PER SPECIES
  output$mpas_perspecies <- DT::renderDataTable({
    
    temp = compiled_species_list  %>%
      filter(`Scientific name` == input$select_a_species)
    
    temp$`Scientific name` = NULL
    temp$`Common name` = NULL
    temp$`IUCN status` = NULL
    temp$`Endemic status` = NULL
    temp$Group = NULL
    
    
    # find rows with non empty values
    options = unique(temp$`Record type`)
    naposition = which(is.na(options))
    options = options[-naposition]
    
    datatable(temp, filter = "top", options = list(pageLength=50,autoWidth=TRUE,searching=TRUE))
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
  
  # FOURTH TAB
  output$rasters_sa <- renderLeaflet({
    
    if(input$selectspecies != ""){
      
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
        addLayersControl(overlayGroups = c("No-take zones","Mixed-use zones","IUCN range","Distribution models","Occurrences"),options = layersControlOptions(collapsed = FALSE,autoZIndex = TRUE))%>%
        fitBounds(initialBounds$lng1, initialBounds$lat1, initialBounds$lng2, initialBounds$lat2)
      
      # SDM if it exists
      if(exists("temp")){
        values(temp) = values(temp)/1000
        pal <- colorBin("Reds", domain = 0:1,na.color = "#00000000")
        map = 
          map %>%
          addRasterImage(temp, colors = pal, opacity = 0.5,group = "Distribution models")%>%
          addLegend(title = "Species Distribution Model (SDM)",values = values(temp),pal = pal)}else{map}
      
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
            popup = paste0("Year: ",temp5$most_recent,"<Br>Data type: ",temp5$DATATYPE,"<Br>Owner: ",temp5$OWNER),
            group = "Occurrences")}else{map}
      
      
    }})
  
  # Reset the map bounds when the button is clicked
  observeEvent(input$resetButton2, {
    leafletProxy("rasters_sa") %>%
      fitBounds(initialBounds$lng1, initialBounds$lat1, initialBounds$lng2, initialBounds$lat2)
  })
  
  
}