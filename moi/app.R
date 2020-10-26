library(shiny)
library(tidyverse)
library(leaflet)
library(DT)
library(shinydashboard)
# library(CARTElette)
library(sf)
library(readxl)
library(vroom)
library(leaflet)
library(rgdal)
library(httr)
library(banR)
library(shinycssloaders)
library(magrittr)
library(dplyr)


# wd ----------------------------------------------------------------------

setwd("C:/Users/eric.wang/Documents/R projets/202010 - moicarte/moi/")

# load data ---------------------------------------------------------------

load(file = "data/moiliste.rdata")
load(file = "data/moilogementXY.rdata")
load(file = "data/moilogementXYn.rdata")
load(file = "data/intervention.rdata")

dep_sf <- sf::st_read(dsn = "geo/DEP_2019_CARTElette.shp", stringsAsFactors = F)
reg_sf <- sf::st_read(dsn = "geo/REG_2019_CARTElette.shp", stringsAsFactors = F)
epci_sf <- sf::st_read(dsn = "geo/EPCI_2019_CARTElette.shp", stringsAsFactors = F)
com_sf <- sf::st_read(dsn = "geo/communes-20190101.json", stringsAsFactors = F) 

com_map <- read_csv("geo//communes-01042019.csv", 
                    col_types = cols(reg = col_character()))

#to generate xy shp file
# xy <- moiliste[, c("longitude", "latitude")] 
# 
# moilisteSP <- SpatialPointsDataFrame(proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"), coords = xy, moiliste)

# general function --------------------------------------------------------

percent <- function(x,
                    digits = 1,
                    format = "f",
                    ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}


# label llsXYn FAUX ------------------------------------------------------------------

llsXYn <- llsXYn %>% mutate(group = cut(n, breaks = c(0, 25, 50, Inf), labels = c("green", "blue", "orange")))

getColor <- function(llsXYn) {
    sapply(llsXYn$group, function(group) {
        if(group == "green") {
            "green"
        } else if(group == "blue") {
            "blue"
        } else {
            "orange"
        } })
}

icons <- awesomeIcons(
    icon = 'ios-open',
    iconColor = 'black',
    library = 'ion',
    markerColor = getColor(llsXYn)
)

# sf new attribut ---------------------------------------------------------

# pour le nombre de lls par zone 
llsREG <- llsXY %>% group_by(REG, LIBREG) %>% summarise(lls = n()) %>% select(REG, lls)
llsDEP <- llsXY %>% group_by(DEP, LIBDEP) %>% summarise(lls = n()) %>% select(DEP, lls)
llsEPCI <- llsXY %>% group_by(EPCI, LIBEPCI) %>% summarise(lls = n()) %>% select(EPCI, lls)

# pour le nombre de lls par zone injectÃ© dans les rÃ©fÃ©rentiels geo 
reg_sf <- reg_sf %>% left_join(llsREG %>% mutate(REG = as.character(REG)), by = "REG")
dep_sf <- dep_sf %>% left_join(llsDEP %>% mutate(DEP = as.character(DEP)), by = "DEP")
epci_sf <- epci_sf %>% left_join(llsEPCI %>% mutate(EPCI = as.character(EPCI)), by = "EPCI")

# UI  ---------------------------------------------------------------------

ui <- fluidPage(
    dashboardPage(
        dashboardHeader(title = " "),
        dashboardSidebar(disable = T),
        dashboardBody(
            navbarPage(
                title = paste("MOI"),
                collapsible = T,
                fluid = T, 
                responsive = T,
                
# tab à lire --------------------------------------------------------------
                
                tabPanel("à lire",
                         icon = icon("angellist"),
                         
                         column(
                             width = 6,
                             wellPanel(
                                 titlePanel("Observatoire de la Maîtrise d'Ouvrage d'Insertion (MOI)"),
                                 tags$h2("Présentation"),
                                 tags$h4(
                                     "L'objectif de cet exercice est de rassembler l'ensemble des caractéristiques des organismes bailleurs nommés MOI (maîtrise d'ouvrage d'insertation) sur
                                                    un seul lieu numérique afin de centraliser l'information et de la rendre disponible aux services de l'état. Cet exercice concentre ses efforts sur plusieurs sources
                                                    afin de consolider l'ensemble des données : "
                                 ),
                                 tags$li("SIRENE (2020)"),
                                 tags$li("RPLS (2020)"),
                                 tags$li("GEOFLA (2019)"),
                                 tags$li("COG (2019)"),
                                 tags$h4("précaution d'interprétation : la géolocalisation n'est pas exhaustive, il ne représente pas l'ensemble du parc des organismes."),
                             ),
                             tags$code("document de travail"),
                             br(),
                             tags$code("tous droits réservés, production DGALN/DHUP/LO4")
                         )), 
                
                
                
                tabPanel("observatoire", icon = icon("map"),
                         column(width = 6, 
                                tabsetPanel(
                             
                             
# tab fonction 1-----------------------------------------------------------
                             
                             tabPanel(
                                 "parc",
                                 icon = icon("parking", lib = "font-awesome"), 
                                 # fonction 1
                                     selectInput(
                                     "select_id",
                                     label = h2("parc de l'organisme géolocalisés à partir du RPLS"),
                                     choices = llsXY %>% select(id) %>% unique()
                                 ),
                                 hr(),
                                 tableOutput("nLlsReactive")
                             ), 
                             

# tab fonction 2 --------------------------------------------------------------
                            
                            tabPanel(
                                "territoire",
                                icon = icon("map-marked", lib = "font-awesome"), 
                                selectInput(
                                    "select_geo",
                                    label = h2("liste des logements des organismes présents sur un territoire"),
                                    choices = list(
                                        "Région" = "reg",
                                        "Département" = "dep",
                                        "EPCI" = "epci"
                                        # "Métropole" = "metro", "DROM" = "drom",
                                        # "Commune" = "com"
                                    ),
                                    selected = "reg"
                                ),
                                withSpinner(uiOutput("ui_geo"), color = "#1484b1", type = "7"),
                                hr(),
                                tableOutput("rechercheOrganisme"),
                                textOutput("rechercheOrganisme1")
                            ), 
                             
                             

# tab fonction 3 --------------------------------------------------------------

                            tabPanel(
                                "intervention",
                                icon = icon("clone", lib = "font-awesome"), 
                                selectInput(
                                    "select_id_intervention",
                                    label = h2("territoire d'intervention d'un organisme"),
                                    choices = llsXY %>% select(id) %>% unique()
                                ),
                                br(),
                                tableOutput("tableauIntervention")
                            ), 
                             
# tab table 1 --------------------------------------------------------------
                             
                            tabPanel(
                                "Table MOI",
                                icon = icon("table", lib = "font-awesome"), 
                                tags$h2("Annuaire des MOI"),
                                dataTableOutput("tableauMOI")),
                            

# tab panel 2 -------------------------------------------------------------
                            
                            tabPanel(
                                "Table LLS",
                                icon = icon("table", lib = "font-awesome"), 
                                tags$h2("Annuaire des LLS"),
                                dataTableOutput("tableauLLS")
                                         )
                                      )
                         ),

# leaflet -----------------------------------------------------------------

                         column(width = 6, leafletOutput("mymap", height = 1000))     
                         
                ))
            )
        )
    )


# server ------------------------------------------------------------------

server <- function(input, output) {

    # reactive value available --------------------------------------------------
    
    Rmoiliste <- reactive({
        
        moiliste %>% 
            dplyr::filter(id == input$select_id)
        
    })
    
    RllsXYn <- reactive({
        
        llsXYn %>% 
            dplyr::filter(id == input$select_id)
        
    })
    
    RinterventionCom <- reactive({
        
        intervention %>% 
            filter(id == input$select_id_intervention) %>%
            pull(territoire_result) %>% 
            unlist()
        
    })
    
    Rintervention <- reactive({
        
        com_sf %>% 
            filter(insee %in% RinterventionCom())
        
    })
    
    
    
    # mymap = carte -----------------------------------------------------------
    
    output$mymap <- renderLeaflet({
        
        colorReg <- colorBin("YlOrRd", domain = reg_sf$lls)
        labelReg <- sprintf("<strong>REGION</strong> : %s</strong><br/>SOMME DES LLS</strong> : %g</sup>",
                            reg_sf$nom, reg_sf$lls) %>% 
            lapply(htmltools::HTML)
        
        labelDep <- sprintf("<strong>DEP</strong> : %s <br/><strong>SOMME DES LLS</strong> : %g</sup>",
                            dep_sf$nom, dep_sf$lls) %>% 
            lapply(htmltools::HTML)
        
        labelEpci <- sprintf("<strong>EPCI</strong> : %s <br/><strong>SOMME DES LLS</strong> : %g</sup>",
                             epci_sf$nom, epci_sf$lls) %>% 
            lapply(htmltools::HTML)
        
        labelmoiliste <- sprintf("<strong>ID</strong> : %s <br/><strong>ADRESSE</strong> : %s <br/> <strong>SOMME DES LLS</strong> : %s <br/>",
                                 Rmoiliste()$id, Rmoiliste()$adresse, Rmoiliste()$n) %>% 
            lapply(htmltools::HTML)
        
        labelllsXY <- sprintf("<strong>ADRESSE</strong> : %s <br/> <strong>NOMBRE DE LLS</strong> : %s <br/>",
                              RllsXYn()$adresse, RllsXYn()$n) %>% 
            lapply(htmltools::HTML)
        
        leaflet() %>%
            setView(zoom = 6,
                    lat = 46.6803337,
                    lng = 1.9408045) %>%
            addMapPane("siège social des MOI", zIndex = 410) %>%  # Level 1: bottom
            addMapPane("logements sociaux", zIndex = 420) %>% 
            addTiles() %>%
            addPolygons( # couche région
                data = reg_sf,
                group = "régions",
                fillColor = ~colorReg(lls),
                opacity = 0.51,
                color = "white",
                dashArray = "1",
                fillOpacity = 0.7,
                label = labelReg,
                highlight = highlightOptions(
                    weight = 1,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
            addPolygons(
                data = dep_sf, #couche département
                group = "départements",
                fillColor = ~colorReg(lls),
                opacity = 0.51,
                color = "white",
                dashArray = "1",
                fillOpacity = 0.7,
                label = labelDep,
                highlight = highlightOptions(
                    weight = 1,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
            addPolygons(
                data = epci_sf, #couche epci
                group = "epci",
                fillColor = ~colorReg(lls),
                opacity = 0.51,
                color = "white",
                dashArray = "1",
                fillOpacity = 0.7,
                label = labelEpci,
                highlight = highlightOptions(
                    weight = 1,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
            addPolygons(
                data = Rintervention(), #couche communes
                group = "communes",
                opacity = 1.0,
                color = "white",
                dashArray = "1",
                weight = 1, 
                smoothFactor = 0.5,
                fillColor = "white",
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                bringToFront = TRUE)
                ) %>% 
            addMarkers(
                data = Rmoiliste(), #couche siège social
                lat = ~ latitude,
                lng = ~ longitude,
                popup = labelmoiliste,
                group = "siège social des MOI"
            ) %>%
            addAwesomeMarkers(
                data = RllsXYn(), #couche lls
                lat = ~ latitude,
                lng = ~ longitude,
                popup = labelllsXY,
                group = "logements sociaux", 
                icon = icons
            ) %>%
            addLayersControl(
                overlayGroups = c("siège social des MOI", "logements sociaux", "régions", "départements", "epci", "communes"),
                options = layersControlOptions(collapsed = FALSE)
            ) %>% 
            hideGroup(c("epci", "départements", "régions")) 
        # %>% 
        #     groupOptions("communes", zoomLevels = 1:8) 
        
    })
    
    
    # tableau = liste des organismes ----------------------------------------------------------------
    
    output$tableauMOI <- renderDataTable({
        
        moiliste %>%
            rename(nom = denominationUniteLegale, lls = n) %>%
            select(nom, siren, adresse, lls) %>%
            arrange(desc(lls))
        
    }, filter =  'top',
    extensions = c('Buttons', 'ColReorder'),
    options = list(
        autoWidth = T,
        colReorder = T,
        dom = 'Bfrtip',
        pageLength = 10,
        buttons = c('copy', 'pdf', 'excel' , 'colvis')
    ))
    
    # tableau = liste des lls ----------------------------------------------------------------
    
    output$tableauLLS <- renderDataTable({
        
        llsXYn %>% 
            ungroup() %>% 
            select(id, n, adresse) %>% 
            group_by(id, adresse) %>% 
            summarise(lls = sum(n)) %>% 
            arrange(desc(lls))
        
    }, filter =  'top',
    extensions = c('Buttons', 'ColReorder'),
    options = list(
        autoWidth = T,
        colReorder = T,
        dom = 'Bfrtip',
        pageLength = 10,
        buttons = c('copy', 'pdf', 'excel' , 'colvis')
    ))
    

    # tableau = intervention ----------------------------------------------------

    output$tableauIntervention <- renderTable({
        
        if (RinterventionCom()!= 0){
        
        result <- com_map %>% 
            # filter(com %in% RinterventionCom())RinterventionCom
            filter(com %in% RinterventionCom()) %>% 
            left_join(as_tibble(reg_sf), by = c("reg" = "REG")) %>% 
            rename(region_nom = nom) %>% 
            left_join(as_tibble(dep_sf), by = c("dep" = "DEP")) %>% 
            rename(dep_nom = nom) %>% 
            select(com, libelle, dep_nom, region_nom)
        
        result_n <- result %>% 
            nrow()
        
        result_reg <- result %>% 
                select(region_nom) %>% 
            unique() %>% 
            pull() %>% 
            na.omit()
        
        result_dep <- result %>% 
            select(dep_nom) %>% 
            unique() %>% 
            pull() %>% 
            na.omit()
        
        result_liste_com <- com_map %>% 
            filter(com %in% RinterventionCom()) %>% 
            mutate(commune = paste(libelle, "-", com)) %>% 
            pull(commune)
        
        tibble("régions :" = str_c(result_reg, collapse = ", "),
               "départements : " = str_c(result_dep, collapse = ", "), 
               "nombre de communes :" = result_n,
               "liste des communes :" = str_c(result_liste_com, collapse = ", "))}
        
        else{print("absence de données disponibles pour le bailleur selectionné")}
        
        
    })
    
    # statistique reactive ----------------------------------------------------
    
    output$nLlsReactive <- renderTable({
        
        
        a <- RllsXYn() %>% nrow()
        
        a2 <- llsXYn %>% nrow()
        
        b <- RllsXYn()$n %>% 
            sum(na.rm = T)
        
        b2 <- llsXYn$n %>% 
            sum(na.rm = T)
        
        tibble("nombre des lls localisés" = paste(a,"/", percent(a/a2)),
               "somme des lls localisés" = paste(b,"/", percent(b/b2)))
        
    })
    
    
    
    # ui reactive select geo --------------------------------------------------
    
    output$ui_geo <- renderUI({
        if (is.null(input$select_geo)) {
            return()
        }
        
        switch(
            input$select_geo,
            "reg" =  selectInput(
                "select_mod",
                label = h4(" "),
                choices = sort(reg_sf$nom)
            ),
            "dep" =  selectInput(
                "select_mod",
                label = h4(" "),
                choices = sort(dep_sf$nom)
            )
            ,
            "epci" =  selectInput(
                "select_mod",
                label = h4(" "),
                choices = sort(epci_sf$nom)
            )
        )
        
    })
    
    id_select_mod <- reactive({
        switch(
            input$select_geo,
            "reg"  = return(reg_sf[reg_sf$nom %in% input$select_mod,]$REG),
            "dep"  = return(dep_sf[dep_sf$nom %in% input$select_mod,]$DEP),
            "epci" = return(epci_sf[epci_sf$nom %in% input$select_mod,]$EPCI),
            # "com"  = return(ref_com()[ref_com()$LIBCOM %in% input$select_mod,]$DEPCOM)
        )
        
    })
    
    
    # recherche organisme sur un territoire -----------------------------------
    
    output$rechercheOrganisme1 <- renderText({
        
        paste (id_select_mod(), id_select_mod() %>% class)
        
    })
    
    output$rechercheOrganisme <- renderTable({

        if (is.null(input$select_geo)) {
            return()
        }


        if(input$select_geo == "reg"){

            llsXY %>%
                ungroup() %>% 
                filter(REG == id_select_mod()) %>%
                group_by(id) %>%
                summarise('nombre de lls' = n(),
                          'loyer moyen' = mean(loymoy, na.omit = T),
                          'age moyen du parc' = mean(age, na.omit = T),
                          'duree de vac. moyen' = mean(duree_vacance, na.omit = T))

        }

        if(input$select_geo == "dep"){

            llsXY %>%
                filter(DEP == id_select_mod()) %>%
                group_by(id) %>%
                summarise('nombre de lls' = n(),
                          'loyer moyen' = mean(loymoy, na.omit = T),
                          'age moyen du parc' = mean(age, na.omit = T),
                          'duree de vac. moyen' = mean(duree_vacance, na.omit = T))

        }

        else{

            llsXY %>%
                filter(EPCI == id_select_mod()) %>%
                group_by(id) %>%
                summarise('nombre de lls' = n(),
                          'loyer moyen' = mean(loymoy, na.omit = T),
                          'age moyen du parc' = mean(age, na.omit = T),
                          'duree de vac. moyen' = mean(duree_vacance, na.omit = T))

        }


    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
