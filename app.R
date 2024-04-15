##################### PACKAGES
library(leaflet)
library(leafpop)
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
library(feather)
library(rmapshaper)
#library(mapboxer)
#library(plotly)
##################### PACKAGES

# map box token
#Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoibmluenlmYiIsImEiOiJjbHVxc3NzcjYwMXVkMnNxcWpjcGxpZHU0In0.e91nxemBTH3tAcwtoAroIw')

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


##################### MPA OVERVIEW DATA
mpa_data_overview = readRDS(list.files(pattern = "mpa_data_overview.RDS",recursive=TRUE))
##################### MPA OVERVIEW DATA


##################### EEZ DATA
eez_sa =  ms_simplify(st_read(list.files(pattern = "eez.shp",recursive = TRUE, full.names = TRUE)))
##################### EEZ DATA


##################### SPECIES PER MPA DATA
# File
species_overlapdata = read_feather(list.files(pattern = "species_permpa_byiusnandsdms.feather", recursive=TRUE,full.names = TRUE))
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


##################### add some info to overlap data
species_overlapdata = read_feather(list.files(pattern = "species_overlapdata",recursive=TRUE,full.names=TRUE))
##################### add some info to overlap data


##################### EAST AND WEST BOUNDARIES PER SPECIES
boundaries = read_feather(list.files(pattern = "boundaries.feather",recursive=TRUE,full.names=TRUE))
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
mostrecent_sighting = read_feather(list.files(pattern = "mostrecent_sighting.feather",recursive=TRUE,full.names = TRUE))
##################### MOST RECENT SIGHTING PER MPA


##################### DATA POINTS PER HIGH RES GRID CELL
lifehistory = read_feather(list.files(pattern = "lifehistory_parameters.feather",recursive = TRUE, full.names = TRUE))
#####################  DATA POINTS PER HIGH RES GRID CELL


##################### DATA POINTS PER HIGH RES GRID CELL
contours =  ms_simplify(readRDS(list.files(pattern = "contours.RDS",recursive = TRUE)))
#####################  DATA POINTS PER HIGH RES GRID CELL


##################### list of data providers and institutions
providers = read_xlsx(list.files(pattern = "data_providers_updated",recursive=TRUE,  full.names=TRUE),sheet = 1)
providers = unique(providers)
institutions = read_xlsx(list.files(pattern = "data_providers_updated",recursive=TRUE,  full.names=TRUE),sheet = 2)
institutions = unique(institutions)
institutions <- flextable(data = institutions, col_keys = c("Dataset affiliation (not necessarily current owner's affiliation)", "Relevant affiliation link (if applicable)"))%>%
  autofit()
institutions <- compose(x = institutions, j = 2, value = as_paragraph( hyperlink_text(x = `Relevant affiliation link (if applicable)`, url = `Relevant affiliation link (if applicable)`)))
##################### list of data providers and institutions


##################### MPA DATA
shapefile_data_simple =  ms_simplify(readRDS(list.files(pattern = "shapefile_data_simple.RDS",recursive = TRUE)))
##################### MPA DATA


##################### MASTER SHEET
# File
master = read_feather(list.files(pattern = "master.feather",recursive=TRUE,full.names=TRUE))
##################### MASTER SHEET


##### species summary table
table1 = master%>%
  rename("IUCN range available?" = iucn) %>%
  rename("Species Distribution Model produced?" = sdm)%>%
  mutate(SPECIES_SCIENTIFIC = str_to_sentence(SPECIES_SCIENTIFIC))%>%
  rename("Common name" = Species_common)%>%
  rename("Scientific name" = SPECIES_SCIENTIFIC)%>%
  rename("IUCN Red List Status" = STATUS)%>%
  rename("Endemic status" = ENDEMIC.STATUS)%>%
  rename("Group" = group)
##### species summary table


##################### list of species per map as well as their most recent sighing
compiled_species_list = read_feather(list.files(pattern = "compiled_species_list.feather",recursive = TRUE, full.names = TRUE))
##################### list of species per map as well as their most recent sighing


##################### PREPARE MASTER TABLE FOR FIRST PAGE
overlap_shortened = read_feather(list.files(pattern = "overlap_shortened",recursive=TRUE,full.names=TRUE))
##################### PREPARE MASTER TABLE FOR FIRST PAGE


##################### Expert extents
load(list.files(pattern = "points.RData", recursive = TRUE, full.names = TRUE))
expert_extent = points
rm(points)
colnames(expert_extent)[1] = "SPECIES_SCIENTIFIC"
expert_extent$SPECIES_SCIENTIFIC = toupper(expert_extent$SPECIES_SCIENTIFIC)
expert_extent = st_as_sf(expert_extent)
expert_extent = left_join(overlap_shortened,expert_extent)
##################### Expert extents


##################### leaflet map
# zone type (test to speed up mapping)
notake = ms_simplify(readRDS(list.files(pattern ="notake.RDS",recursive = TRUE,full.names = TRUE)))
ccr = ms_simplify(readRDS(list.files(pattern ="ccr.RDS",recursive = TRUE,full.names = TRUE)))
c =  ms_simplify(readRDS(list.files(pattern ="c.RDS",recursive = TRUE,full.names = TRUE)))
cpl = ms_simplify(readRDS(list.files(pattern ="cpl.RDS",recursive = TRUE,full.names = TRUE)))
clp =  ms_simplify(readRDS(list.files(pattern ="clp.RDS",recursive = TRUE,full.names = TRUE)))

sa_map = leaflet() %>%
  addTiles()%>%
  addPolygons(data = eez_sa,weight = 1, color = "grey",fillColor = "white",fillOpacity = 0)%>%
  addPolylines(data = contours,weight = 1, color = "grey",label = ~DEPTH,popup = ~DEPTH,fillOpacity = 0)%>%
  addPolygons(data = notake,weight = 1,popup = ~CUR_NME,label = ~CUR_NME, color = "Blue",fillColor = "skyblue",fillOpacity = 0.6, group = "No-take zones")%>%
  addPolygons(data = ccr,weight = 1,label = ~CUR_NME,popup = ~CUR_NME, color = "green4",fillColor = "green",fillOpacity = 0.6,group = "Mixed-use zones")%>%
  addPolygons(data = c,weight = 1,label = ~CUR_NME,popup = ~CUR_NME, color = "Purple",fillColor = "purple",fillOpacity = 0.6,group = "Mixed-use zones")%>%
  addPolygons(data = cpl,weight = 1,label = ~CUR_NME,popup = ~CUR_NME, color = "Purple",fillColor = "hotpink",fillOpacity = 0.6,group = "Mixed-use zones")%>%
  addPolygons(data = clp,weight = 1,label = ~CUR_NME, popup = ~CUR_NME,color = "Purple",fillColor = "lightpink",fillOpacity = 0.6,group = "Mixed-use zones")%>%
  addLegend("bottomleft",title = "Zone Types", colors = c("skyblue","purple","hotpink","lightpink","green"), opacity = 1,
            labels = c("Wilderness, Sanctuary or Restricted (No-take zones)","Controlled (Mixed-use zones)","Controlled-Pelagic Linefish with List (Mixed-use zones)","Controlled Large Pelagic (Mixed-use zones)","Controlled Catch and Release (Mixed-use zone, only in iSimangaliso MPA - KZN)")) %>%
  addLayersControl(overlayGroups = c("No-take zones","Mixed-use zones"), options = layersControlOptions(collapsed = FALSE,autoZIndex = TRUE))
##################### leaflet map


#####################  DEFINING THE UI (USER INTERFACE)
# Define UI for application 
ui <- fluidPage(
  
  theme = shinytheme("united"),
  
  # Jumbotrons are pretty, they make nice headers
  tags$div(class = "jumbotron text-center", style = "margin-bottom:0px;margin-top:0px",
           tags$h2(class = 'jumbotron-heading', stye = 'margin-bottom:0px;margin-top:0px', 'Sharks, Rays and MPAs'),
           p('Interactive website to visualise spatial data on sharks, rays and MPAs in SA')
  ),
  
  tabsetPanel(
    
    ####### PURPOSE TAB
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
    ####### PURPOSE TAB
    
    ####### DATA TAB
    tabPanel("Data",
             fluidRow(
               column(width = 2),
               column(width = 8,
                      h1("Data sources", style = "font-size:30px;"),
                      HTML("A wide variety of data collection methods can be used to gather spatial information on sharks and rays.
                      <br>This site uses data from multiple sources including modelled ranges to accurate location data.
                           <br> See below for a brief description of all data sources provided."),
                      h2("International Union for the Conservation of Nature (IUCN) species ranges",style = "font-size:20px; text-decoration: underline;"),
                      HTML("
                      The IUCN provides a range map for several species which come in the form of polygons the encompass all known occurrences.
                      <br> This range is formally defined as an <b>Extent of Occurrence (EOO)</b>:<br><br> <i>The area contained within the shortest continuous imaginary boundary<br>which can be drawn to encompass all the known,
                      inferred or projected sites of present occurrence of a taxon,<br>excluding cases of vagrancy</i>.
                          <br><br> The range maps used here can all be downloaded from their website and their mapping standards are also available:<br>"),
                      a("IUCN mapping standards", href = "https://www.iucnredlist.org/resources/mappingstandards"),
                      HTML("<br>"),
                      a("IUCN Red List website", href = "https://www.iucnredlist.org"),
                      HTML("<br><br> The IUCN range maps are continually changing as new assessments are being added and changed. The following paper describes the assessment process and mapping approach"),
                      a("Dulvy, N. K., Pacoureau, N., Rigby, C. L., Pollom, R. A., Jabado, R. W., Ebert, D. A., Finucci, B., Pollock, C. M., Cheok, J., Derrick, D. H., Herman, K. B., Sherman, C. S., VanderWright, W. J., Lawson, J. M., Walls, R. H. L., Carlson, J. K., Charvet, P., Bineesh, K. K., Fernando, D., Ralph, G. M., Matsushiba, J. H., Hilton-Taylor, C., Fordham, S. V., & Simpfendorfer, C. A. (2021). Overfishing drives over one third of all sharks and rays toward a global extinction crisis. Current Biology, 31(21), P4773-4787.e4778. doi:https://doi.org/10.1016/j.cub.2021.08.062",href = "https://www.sciencedirect.com/science/article/pii/S0960982221011982"),
                      h2("Species Distribution Models (SDMs)",style = "font-size:20px;text-decoration: underline;"),
                      HTML("The following published paper describes the methods used to produce the species distribution models, these are all available upon request, please contact<b> nina-fb@outlook.com</b><br>"),
                      a("Faure-Beaulieu N.,(2023). A systematic conservation plan identifying critical areas for improved chondrichthyan protection in South Africa. Biological Conservation, 284, 110163. https://doi.org/10.1016/j.biocon.2023.110163",href ="https://doi.org/10.1016/j.biocon.2023.110163"),
                      h2("Species illustrations",style = "font-size:20px;text-decoration: underline;"),
                      HTML("All illustrations were kindly provided by Ann Hecht"),
                      h2("Life-history information",style = "font-size:20px;text-decoration: underline;"),
                      HTML("The following report has compiled all available life-history and distributional information on threatened and endemic species in South Africa.<br>
                           Information includes scientific and common names, Family, size range, distribution, habitat, depth range, major fisheries, IUCN status, CITES regulations, MLRA regulations, Compiler name, Reviewer name, Species Summary and Recommendations, Taxonomic and Identifcation Issues and more.<br>"),
                      a("Cliff, G. and Olbers, J.M. (Eds). 2022. Species profiles of South African sharks, rays and chimaeras. Volume 1: Threatened and Endemic Species. WILDTRUST Special Publication 2, Durban, South Africa. 556pp.",href ="https://sharksunderattackcampaign.co.za/wp-content/uploads/2023/03/2023_01_17_WILDOCEANS-endemic-and-threatened-sharks-species-reports.pdf"),
                      h2("Occurrence data",style = "font-size:20px;text-decoration: underline;"),
                      HTML("Location data has been provided by research institutions as well as individual researchers (listed at the bottom of this page). At a minimum for a species's location data to be included it must be accompanied by:"),
                      HTML(
                        "<ul>
                        <li> A date
                        <li> GPS coordinates
                        <li> Species scientific name
                        </ul>"
                      ),
                      HTML("Below is a table of all datasets, data-owners and institutions who have contributed data to the site.
                           <br> If you have a dataset you would like to contribute please contact <b>sharksraysmpas@gmail.com</b><br>"),
                      uiOutput("Providers"),
                      uiOutput("Institutions"),
                      h1("Data Privacy and Data sharing", style = "font-size:30px;"),
                      HTML("Sharing hard-earned datasets is a daunting prospect so it is important to know that this app does not allow for any dataset downloads." ,br(),
                        "This platform is designed for the <b> visualization </b> of datasets provided.
                        <br>Any queries regarding the use of these datasets can be directed to their respective owners listed above"),
                      h1("Data resolution",style = "font-size:30px;"),
                      HTML("This app currently displays data at a 10 x 10 km resolution. Please see the <b> species spatial information tab </b> to see how data is displayed.")
                      
               )
             )),
    ####### DATA TAB
    
    ####### MARINE PROTECTED AREAS TAB
    tabPanel("Marine Protected Areas",
             
             #######  FIRST ROW
             fluidRow(
               column(3,
                      h4(""),
                      HTML("<font color='red'><b>IMPORTANT:</b> The map takes 1 to 3 minutes to load</font><br><br>"),
                      HTML("<h4><b4>This is an interactive map of South Africa's<br>Marine Protected Area (MPA) network.<br><br>
                           Please refer to the definition boxes below for information on activities permitted in each zone.
                           <br><br><b>Click at the top right corner of the map</b> and individually display the two major zonation types (No-take zones and Mixed-use zones).
                           <br><br> <b>Scroll down to the bottom of this page</b> to select individual MPAs for a detailed view of their zonation</b></h4>"),
                      h4(tags$b("Source of spatial information:")),
                      a("South Africa Marine Protected Area Zonations (SAMPAZ_OR_2023_Q2), Department of Environmental Affairs",href ="https://egis.environment.gov.za/data_egis/data_download/current")
               ),
               column(9, align = "center", column(12, leafletOutput("mpas_sa",width = "80%", height = "50vh")
                                                 , actionButton("resetButton", "Reset Zoom")
               )
               
               )
             ),
             #######  FIRST ROW
             
             #######  SECOND ROW
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
             #######  SECOND ROW
             
             #######  THIRD ROW
             fluidRow(
               column(width = 3,
                      br(), br(), br(), br(),
                      wellPanel(
                        class = "custom-well",
                        div(style = "background-color: green; padding: 10px;",
                            HTML("<h4><b>Controlled Catch and Release</b></h4>"),
                            HTML("<p> Fishing is permitted. Gear type: linefishing using barbless hooks only. Target species: specified in MPA gazette. Important: All fish must be carefully handled and released alive and unharmed back into the water from which it was caught.</p>")
                        )))),
             #######  THIRD ROW
             
             #######  FOURTH ROW
             fluidRow(
               column(12,
                      HTML("Select individual MPAs for a detailed view of their zonation.<br>The selected MPA will be highlighted by a white border</h4><br><br>"),
                      ),
               column(4,  # list of MPA inputs
                      selectInput("filtermpa", 
                                  "Select an MPA:", 
                                  sort(unique(shapefile_data_simple$CUR_NME))))
             ),
             #######  FOURTH ROW

             #######  FIFTH ROW
             fluidRow( # row 2
               column(4, style='padding-left:10px; padding-right:10px; padding-top:10px; padding-bottom:10px',
                      #This is the mpa mpa
                      leafletOutput("individual_mpa_zones")),
               column(8, DT::dataTableOutput("tabletest"))
             )
             #######  FIFTH ROW
             
             ),
    ####### MARINE PROTECTED AREAS TAB
    
    
    ####### SHARKS AND RAYS IN SOUTH AFRICA - OVERVIEW TAB
    tabPanel("Sharks and Rays in South Africa - overview",
             
             #######  FIRST ROW
             # Create a new Row in the UI for selectInputs
             fluidRow(
               column(12,
                      h4(""),
                      HTML("<h4><b4> Below is a list of all 194 species found in South Africa.<br>The information used to create this table has come from different sources and is referenced at the bottom of the page.<br>For each species, information on its IUCN Red List status as well as endemic status is provided.</b></h4>")
               )),
             #######  FIRST ROW
             
             #######  SECOND ROW
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
             #######  SECOND ROW
             
             #######  THIRD ROW
             DT::dataTableOutput("table"),
             #######  THIRD ROW
             
    ),
    ####### SHARKS AND RAYS IN SOUTH AFRICA - OVERVIEW TAB
    
    
    ####### MPA SPECIES LIST TAB
    tabPanel("MPA species list", 
             
             #######  FIRST ROW
             fluidRow(
               column(12,
                      h4(""),
                      HTML("<h4>Use the dropdown below to get a list of species per Marine Protected Area.
                      <br><br>The list is built by using <b>three</b> different sources of information to assess the presence of a species within the MPA:<br>1. Species Distribution Models<br>2. IUCN Red List Ranges<br>3. True occurrence records from verified research sources
                      <br><br> Rows highlighted in green are species with confirmed presences in the MPA. The most recent sighting as well as data source is provided
                      <br><br><b>IMPORTANT:</b> This does not represent a complete and comprehensive list of species for an MPA, but is rather meant to show the most recent (or provided) sighting information in a any particular MPA</h4>")
                      
               )),
             #######  FIRST ROW
             
             #######  SECOND ROW
             fluidRow(
               column(3,# list of MPA inputs
                      selectizeInput("selectmpa2",
                                     "Select an MPA:",c(sort(unique(shapefile_data_simple$CUR_NME))),
                                     options = list(placeholder = 'Select an MPA', onInitialize = I('function() { this.setValue(""); }'))
                      ))),
             #######  SECOND ROW
             
             #######  THIRD ROW
             fluidRow( # row 2
               column(3, style='padding-left:10px; padding-right:10px; padding-top:10px; padding-bottom:10px',
                      #This is the mpa
                      leafletOutput("mpa_specieslist_tab")),
               column(9,DT::dataTableOutput("species_permpa")),
               style = "margin-bottom: 20px;" 
               
             ),
             #######  THIRD ROW
    ),
    ####### MPA SPECIES LIST TAB
    
    
    ####### SPECIES SPATIAL INFORMATION TAB
    tabPanel("Species spatial information", 
             
             #######  FIRST ROW
             fluidRow(
               column(12,
                      h4(""),
                      HTML("<h4><b4>This is an interactive map displaying spatial information for South Africa's shark and ray species.<br>
                      Select a species and a map will appear showing all available spatial information for this species, these layers can all be turned on or of and include:
                       <ul>
                          <li> <font color='#FFCC00'>The IUCN range</font>
                          <li> <font color='green'>Occurrence data at 10 x 10 km resolution</font>
                          <li> <font color='red'>Species Distribution Model (SDM)</font>
                       </ul>
                       <br>If occurrences are available, <b>click on any of the green cells</b> and the date as well as data type and owner will appear. The darker the green the more recent the sighting.
                       <br><br><b>IMPORTANT:</b><br> This does not represent the complete set of occurrence data for the selected species, but is rather meant to show the most recent (or provided) sighting information for a species occurrence in a any particular cell </b></h4>")
               )),
             #######  FIRST ROW
             
             #######  SECOND ROW
             fluidRow(column(4,selectizeInput("selectspecies", h4("Select a species"),choices = sort(str_to_sentence(unique(overlap_shortened$SPECIES_SCIENTIFIC))), multiple=FALSE, 
                                              options = list(
                                                placeholder = 'Please select a species',
                                                onInitialize = I('function() { this.setValue(""); }')
                                              ))
             )),
             #######  SECOND ROW
             
             #######  THIRD ROW
             fluidRow(align = "center",
                      
                      column(2, # Adjust the column width as needed
                             # Text
                             HTML("<h4><b>West/East extents<br>(from occurrences)</b></h4>"),
                             # Image
                             div(img(src = "boundary_cross.png"))),
                      column(10,leafletOutput("rasters_sa", width = "80%", height = "50vh"),
                             actionButton("resetButton2", "Reset Zoom")
                      )),
             #######  THIRD ROW
             
             #######  FOURTH ROW
             fluidRow(
               column(12,
                      h4(""),
                      HTML("<h4><br><br>The table below displays all Marine Protected Areas (MPAs) which overlap with any of the three forms of data for the selected species (Ocurrences, IUCN range, species distribution models (SDMs))</h4><br><br>")
               )),
             #######  FOURTH ROW
             
             #######  FIFTH ROW
             fluidRow(
              column(width = 1),
              column(DT::dataTableOutput("mpas_perspecies"), width = 10),
              column(width = 1)),
             #######  FIFTH ROW
             
             #######  SIXTH ROW
             # life history information and species images
             fluidRow(
               column(4,
                      selectInput("illustration",
                                  "Select a species:",
                                  choices = sort(str_to_sentence(unique(overlap_shortened$SPECIES_SCIENTIFIC)))
                      ))),
             #######  SIXTH ROW
             
             #######  SEVENTH ROW
             fluidRow(align = "left",
                      column(4,imageOutput("display_illustration")),
                      column(8,DT::dataTableOutput("species_LH"))
             ),
             #######  SEVENTH ROW
               ),
    ####### SPECIES SPATIAL INFORMATION TAB
    
    
    ####### QUERIES TAB
    
    #######  FIRST ROW
    tabPanel("Queries",
             fluidRow(
               column(width = 2),
               column(width = 8,
                      h1("Reporting Issues"),
                      p("The information presented on this site should be accurate. To report anything that seems inaccurate or wrong please email <b>sharksraysmpas@gmail.com</b>"),
                      h1("Suggesting changes or features"),
                      HTML("The code used to build this site is available on a github page.
                      This page also has an <b>issues tab</b> which allows users to log any suggestions such as additional features, ways of showing the data, graphs, tables etc...<br><br>
                        Please click on the following link, it will bring you to the issues page where you can click on <b>new issue</b> and write down your suggestion:"),
                      a("https://github.com/ninzyfb/sharks-and-rays-of-SA-SHINY-APP/issues",href ="https://github.com/ninzyfb/sharks-and-rays-of-SA-SHINY-APP/issues"),
                      
               )       
             )
             #######  FIRST ROW
             
    )
    ####### QUERIES TAB
    
  ),
  # Add inline CSS rule to align legend items to the left
  tags$style(HTML(".info { text-align: left }"))  
)
#####################  DEFINING THE UI (USER INTERFACE)


#####################  DEFINING THE SERVER LOGIC
# Define server logic
server <- function(input, output,session) {
  
  shinyalert("Hello! Please read:", "Please note that this app is in development and maps take about 1-3 minutes to appear", type = "info")
  
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
    sa_map
  })
  
  # Reset the map bounds when the button is clicked
  observeEvent(input$resetButton, {
    leafletProxy("mpas_sa") %>% fitBounds(initialBounds$lng1, initialBounds$lat1, initialBounds$lng2, initialBounds$lat2)
  })
  
  
  # FIRST TAB
  # this looks for the MPA outline
  output$individual_mpa_zones <- renderLeaflet({
    
    # extract single mpa
    single_mpa = shapefile_data_simple[which(shapefile_data_simple$CUR_NME == input$filtermpa),]
    # extract centroid for that mpa
    mpa_centroid = st_centroid(st_union(single_mpa))
    mpa_centroid = unlist(mpa_centroid)
    # get all other mpas to add thin opaque layer above them
    mpa_single = shapefile_data_simple[which(shapefile_data_simple$CUR_NME == input$filtermpa),]
    
    leaflet()%>%
      addTiles()%>%
      addPolygons(data = mpa_single,fillColor = "white",color = "white", opacity =1) %>%
      addPolygons(data = notake,weight = 1,label = ~CUR_ZON_NM, color = "Blue",fillColor = "skyblue",fillOpacity = 0.6 )%>%
      addPolygons(data = ccr,weight = 1,label = ~CUR_ZON_NM, color = "green4",fillColor = "green",fillOpacity = 0.6 )%>%
      addPolygons(data =c,weight = 1,label = ~CUR_ZON_NM,fillColor = "purple",fillOpacity = 0.6 )%>%
      addPolygons(data = cpl,weight = 1,label = ~CUR_ZON_NM, color = "Purple",fillColor = "hotpink",fillOpacity = 0.6 )%>%
      addPolygons(data = clp,weight = 1,label = ~CUR_ZON_NM, color = "Purple",fillColor = "lightpink",fillOpacity = 0.6 )%>%
      setView(mpa_centroid[1],mpa_centroid[2],8) %>%
      addPolylines(data = contours,weight = 1, color = "grey",label = ~DEPTH,popup = ~DEPTH,fillOpacity = 0)
      
    
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
    
    data = table1
    
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
  
  # Species spatial information tab
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
  
  # SPECIES LIST TAB
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
  
  # SPECIES LIST TAB
  # this looks for the MPA outline
  output$mpa_specieslist_tab <- renderLeaflet({
    
    # extract single mpa
    single_mpa = shapefile_data_simple[which(shapefile_data_simple$CUR_NME == input$selectmpa2),]
    # extract centroid for that mpa
    mpa_centroid = st_centroid(st_union(single_mpa))
    mpa_centroid = unlist(mpa_centroid)
    # get all other mpas to add thin opaque layer above them
    mpa_single = shapefile_data_simple[which(shapefile_data_simple$CUR_NME == input$selectmpa2),]
    
    leaflet()%>%
      addTiles()%>%
      addPolygons(data = mpa_single,fillColor = "white",color = "white", opacity =1) %>%
      addPolygons(data = notake,weight = 1,label = ~CUR_ZON_NM, color = "Blue",fillColor = "skyblue",fillOpacity = 0.6 )%>%
      addPolygons(data = ccr,weight = 1,label = ~CUR_ZON_NM, color = "green4",fillColor = "green",fillOpacity = 0.6 )%>%
      addPolygons(data =c,weight = 1,label = ~CUR_ZON_NM,fillColor = "purple",fillOpacity = 0.6 )%>%
      addPolygons(data = cpl,weight = 1,label = ~CUR_ZON_NM, color = "Purple",fillColor = "hotpink",fillOpacity = 0.6 )%>%
      addPolygons(data = clp,weight = 1,label = ~CUR_ZON_NM, color = "Purple",fillColor = "lightpink",fillOpacity = 0.6 )%>%
      setView(mpa_centroid[1],mpa_centroid[2],8) %>%
      addPolylines(data = contours,weight = 1, color = "grey",label = ~DEPTH,popup = ~DEPTH,fillOpacity = 0)
    
    
  })
  
  # SPECIES SPATIAL INFORMATION TAB
  # MPAS PER SPECIES
  output$mpas_perspecies <- DT::renderDataTable({
    
    temp = compiled_species_list  %>%
      filter(`Scientific name` == input$selectspecies)
    
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
      map = sa_map %>%
        # IUCN
        addPolygons(data = temp2, weight = 2,color = "Yellow", group = "IUCN range",opacity = 1) %>%
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

shinyApp(ui,server)

# code below is to deploy the app to shiny server when it was on shinyapps.io
#library(rsconnect)
#deployApp()
