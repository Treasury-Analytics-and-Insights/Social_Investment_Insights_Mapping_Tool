Server_Def <- function(input, output, session){
  # Default selection for navigation page
  updateNavbarPage(session, "menu", selected = "Introduction")
  
  observeEvent(input$goButton, {
    updateNavbarPage(session, "menu", selected = "Data Visualisation")
  })
  
  output$DemoTable <- renderUI({
    if (is.null(DemoTableData())) return('')
    HTML(paste0('<table>',
                paste(sapply(names(DemoTableData()), head_html), collapse = ''),
                paste(apply(DemoTableData(), 1, row_html), collapse = ''),
                '</table>'))
  })
  
  Get_Selected_Area <- reactive({
    if (input$filterreg != '') return(input$filterreg)
    if (input$filterta != '') return(input$filterta)
    return(NULL)
  })
  
  output$Area <- renderText({ 
    if (is.null(Get_Selected_Area())) {
      return(NULL)
    }
    Age_Readable <- names(age.list)[sapply(age.list, FUN=function(X) Get_Selected_Age() %in% X)]
    paste0("Comparison of ", 
          Get_Selected_Area(),
          " to rest of NZ for '", Get_Selected_Risk(), "' risk group aged ",
          Age_Readable)
  })
  
  Get_Selected_Age <- reactive({
    if (input$mapagegrp != '') return(input$mapagegrp)
    # This shouldn't be possible in this version...
    return(NULL)
  })
  
  Get_Selected_Risk <- reactive({
    if (input$mapagegrp == '00-05' || input$mapagegrp == '06-14'){
      return(names(Risk0)[sapply(Risk0, FUN=function(X) input$maprisktype1 %in% X)])
    } else if (input$mapagegrp == '15-19'){
      return(names(Risk15)[sapply(Risk15, FUN=function(X) input$maprisktype2 %in% X)])
    } else if (input$mapagegrp == '20-24'){
      return(names(Risk20)[sapply(Risk20, FUN=function(X) input$maprisktype3 %in% X)])
    } else return(NULL)
  })
  
  output$DemoDesc <- renderText({ 
    if (is.null(Get_Selected_Age()) || is.null(Get_Selected_Area()) || is.null(Get_Selected_Risk())) {
      return(NULL)
    }
    Age_Readable <- names(age.list)[sapply(age.list, FUN=function(X) Get_Selected_Age() %in% X)]
    paste0("Gender and Multiple Response Ethnic profile for '", Get_Selected_Risk(), "' risk group aged ", 
          Age_Readable, " in ", Get_Selected_Area())
  })
  
  DemoTableData <- reactive({
    # Construct Male/Female counts
    if(input$dispgeo == 'region'){
      Area <- input$filterreg
      d <- copy(Region_Table)
    } else if (input$dispgeo == 'ta'){
      Area <- input$filterta
      d <- copy(TA_Table)
    } else return(NULL)
    if (input$mapagegrp == '00-05' || input$mapagegrp == '06-14'){
      var <- input$maprisktype1
    } else if (input$mapagegrp == '15-19'){
      var <- input$maprisktype2
    } else if (input$mapagegrp == '20-24'){
      var <- input$maprisktype3
    }
    if (Area == '') return(NULL)
    
    # Extract the risk category
    var <- strsplit(var,"_")[[1]][3]
    d %<>%  filter(agegrp==input$mapagegrp) %>%
      select(description, sex, ends_with(var)) %>%
      group_by(sex, description) %>%
      summarise_each(funs(sum))
    G_Area <- d %>% filter(description == Area) %>%
      select(-description) %>%
      group_by(sex) %>%
      summarise_each(funs(sum))
    
    Gender <- data.frame(c("Total", "Female", "Male"), G_Area[1:3,2])
    names(Gender) <- c("", "Number")
    Ethnicity <- data.frame(c("Maori", "NZ/European", "Asian", "Pacific", "Other"),
                            t(G_Area[1,3:7]))
    names(Ethnicity) <- c("", "Number")
    Final <- rbind(Gender,Ethnicity)
    Final$Percent <- sprintf("%1.1f%%", 100*c(Final$Number[1:3]/sum(Final$Number[2:3]), Final$Number[4:8]/sum(Final$Number[2:3])))
    Final$Number <- format(Final$Number,big.mark=",",scientific=FALSE)
    Final <- Final[-1,]
    return(Final)
  })
  
  output$NationalTable <- renderUI({
    if (input$mapagegrp == '00-05'){
      var <- input$maprisktype1
      Table <- Table_0005
      var <- strsplit(var,"_")[[1]][3]
      Table$At_Risk = Table[, strtoi(var) + 1]
      Table$P1 <- sprintf("%1.1f%%",100*Table$At_Risk/Table$At_Risk[1])
      Table$Not_At_Risk = Table$All - Table$At_Risk
      # Fix Avg Cost.
      Table$Not_At_Risk[7] <- (Table$All[7]*Table$All[1] - Table$At_Risk[7]*Table$At_Risk[1])/Table$Not_At_Risk[1]
      Table %<>% select(X,At_Risk, P1, Not_At_Risk)
      Table$P2 <- sprintf("%1.1f%%",100*Table$Not_At_Risk/Table$Not_At_Risk[1])
      Table$P1[1] = ''
      Table$P2[1] = ''
      Table$P1[7] = ''
      Table$P2[7] = ''
      Table$At_Risk <- format(round(Table$At_Risk) , big.mark=",", scientific=FALSE)
      Table$Not_At_Risk <- format(round(Table$Not_At_Risk) , big.mark=",", scientific=FALSE)
      Table$Not_At_Risk[7] <- paste0('$', Table$Not_At_Risk[7])
      Table$At_Risk[7] <- paste0('$', Table$At_Risk[7])
      names(Table) <- c("Outcome", "At Risk", "", "Not At Risk", "")
    } else if(input$mapagegrp == '06-14'){
      var <- input$maprisktype1
      Table <- Table_0614
      var <- strsplit(var,"_")[[1]][3]
      Table$At_Risk = Table[, strtoi(var) + 1]
      Table$P1 <- sprintf("%1.2f%%",100*Table$At_Risk/Table$At_Risk[1])
      Table$Not_At_Risk = Table$All - Table$At_Risk
      # Fix Avg Cost.
      Table$Not_At_Risk[7] <- (Table$All[7]*Table$All[1] - Table$At_Risk[7]*Table$At_Risk[1])/Table$Not_At_Risk[1]
      Table %<>% select(X,At_Risk, P1, Not_At_Risk)
      Table$P2 <- sprintf("%1.1f%%",100*Table$Not_At_Risk/Table$Not_At_Risk[1])
      Table$P1[1] = ''
      Table$P2[1] = ''
      Table$P1[7] = ''
      Table$P2[7] = ''
      Table$At_Risk <- format(round(Table$At_Risk) , big.mark=",", scientific=FALSE)
      Table$Not_At_Risk <- format(round(Table$Not_At_Risk) , big.mark=",", scientific=FALSE)
      Table$Not_At_Risk[7] <- paste0('$', Table$Not_At_Risk[7])
      Table$At_Risk[7] <- paste0('$', Table$At_Risk[7])
      names(Table) <- c("Outcome", "At Risk", "", "Not At Risk", "")
    } else if (input$mapagegrp == '15-19'){
      var <- input$maprisktype2
      Table <- Table_1519
      var <- strsplit(var,"_")[[1]][3]
      Table$At_Risk = Table[, strtoi(var) + 1]
      Table$P1 <- sprintf("%1.2f%%",100*Table$At_Risk/Table$At_Risk[1])
      Table$Not_At_Risk = Table$All - Table$At_Risk
      # Fix Avg Cost.
      Table$Not_At_Risk[5] <- (Table$All[5]*Table$All[1] - Table$At_Risk[5]*Table$At_Risk[1])/Table$Not_At_Risk[1]
      Table %<>% select(X,At_Risk, P1, Not_At_Risk)
      Table$P2 <- sprintf("%1.1f%%",100*Table$Not_At_Risk/Table$Not_At_Risk[1])
      Table$P1[1] = ''
      Table$P2[1] = ''
      Table$P1[5] = ''
      Table$P2[5] = ''
      Table$At_Risk <- format(round(Table$At_Risk) , big.mark=",", scientific=FALSE)
      Table$Not_At_Risk <- format(round(Table$Not_At_Risk) , big.mark=",", scientific=FALSE)
      Table$Not_At_Risk[5] <- paste0('$', Table$Not_At_Risk[5])
      Table$At_Risk[5] <- paste0('$', Table$At_Risk[5])
      names(Table) <- c("Outcome", "At Risk", "", "Not At Risk", "")
    } else if (input$mapagegrp == '20-24'){
      var <- input$maprisktype3
      Table <- Table_2024
      var <- strsplit(var,"_")[[1]][3]
      Table$At_Risk = Table[, strtoi(var) + 1]
      Table$P1 <- sprintf("%1.1f%%",100*Table$At_Risk/Table$At_Risk[1])
      Table$Not_At_Risk = Table$All - Table$At_Risk
      # Fix Avg Cost.
      Table$Not_At_Risk[5] <- (Table$All[5]*Table$All[1] - Table$At_Risk[5]*Table$At_Risk[1])/Table$Not_At_Risk[1]
      Table %<>% select(X,At_Risk, P1, Not_At_Risk)
      Table$P2 <- sprintf("%1.1f%%",100*Table$Not_At_Risk/Table$Not_At_Risk[1])
      Table$P1[1] = ''
      Table$P2[1] = ''
      Table$P1[5] = ''
      Table$P2[5] = ''
      Table$At_Risk <- format(round(Table$At_Risk) , big.mark=",", scientific=FALSE)
      Table$Not_At_Risk <- format(round(Table$Not_At_Risk) , big.mark=",", scientific=FALSE)
      Table$Not_At_Risk[5] <- paste0('$', Table$Not_At_Risk[5])
      Table$At_Risk[5] <- paste0('$', Table$At_Risk[5])
      names(Table) <- c("Outcome (age 25 to 34)", "At Risk", "", "Not At Risk", "")
    } else return(NULL)
    HTML(paste0('<table>',
                paste(sapply(names(Table), head_html), collapse = ''),
                paste(apply(Table, 1, row_html), collapse = ''),
                '</table>'))
  })
  
  output$National <- renderText({
    Age_Readable <- names(age.list)[sapply(age.list, FUN=function(X) Get_Selected_Age() %in% X)]
    return(paste0("Projected outcomes for '", Get_Selected_Risk(), 
                 "' risk group aged ", Age_Readable, ' (Total NZ)'))
  })
  
  output$ComparisonTable <- renderUI({
    if (is.null(ComparisonTableData())) return('')
    HTML(paste0('<table>',
                paste(sapply(names(ComparisonTableData()), head_html), collapse = ''),
                paste(apply(ComparisonTableData(), 1, row_html), collapse = ''),
                '</table>'))
  })
  
  ComparisonTableData <- reactive({
    # Import the relevant geographic linked data.
    if(input$dispgeo == 'region'){
      Area <- input$filterreg
      d <- copy(Region_Table)
    } else if (input$dispgeo == 'ta'){
      Area <- input$filterta
      d <- copy(TA_Table)
    } else return(NULL)
    if (input$mapagegrp == '00-05' || input$mapagegrp == '06-14'){
      var <- input$maprisktype1
    } else if (input$mapagegrp == '15-19'){
      var <- input$maprisktype2
    } else if (input$mapagegrp == '20-24'){
      var <- input$maprisktype3
    }
    if (Area == '') return(NULL)
    
    # Find the information we are interested in.
    d %<>%  filter(agegrp == input$mapagegrp, sex == "All") %>%
      select(description, all, ends_with(var)) %>%
      mutate(description = ifelse(description == Area, Area, "Rest of NZ")) %>%
      group_by(description) %>%
      summarise_each(funs(sum))
    
    d$description <- gsub("Local Board Area", "", d$description)
    d$description <- gsub("District", "", d$description)
    d$description <- gsub("City", "", d$description)
    d$description <- gsub("Region", "", d$description)
    
    d$Percent <- sprintf('%1.1f%%', 100*d[[var]]/d$all)
    names(d) <- c("", "Total", "At Risk", "Percent")
    d[["Total"]] <-  format(d[["Total"]],big.mark=",",scientific=FALSE)
    d[["At Risk"]] <-  format(d[["At Risk"]],big.mark=",",scientific=FALSE)
    return(d)
  })
  
  DownloadData <- reactive({
    if (input$mapgeonz=="regc" || input$mapgeoreg=="regc"){
      map_data <- copy(Region_Table)
    } else if (input$mapgeonz=="ta" || input$mapgeoreg=="ta" || input$mapgeota=="ta"){
      map_data <- copy(TA_Table)
    } else if (input$mapgeoreg=="au" || input$mapgeota=="au"){
      map_data <- copy(Area_Unit_Table)
    } else return(NULL)
    
    map_data %<>% filter(sex == input$mapsex, agegrp==input$mapagegrp) %>%
      select(-sex, -agegrp)
    
    if (input$filterreg != ''){
      map_data %<>% filter(reg == input$filterreg)
    } else if (input$filterta != ''){
      map_data %<>% filter(tla == input$filterta)
    }
    
    if (input$mapagegrp == '00-05' || input$mapagegrp == '06-14'){
      RiskList <- Risk0
      map_data[[Get_Selected_Risk()]] <- map_data[[input$maprisktype1]]
    } else if (input$mapagegrp == '15-19'){
      RiskList <- Risk15
      map_data[[Get_Selected_Risk()]] <- map_data[[input$maprisktype2]]
    } else if (input$mapagegrp == '20-24'){
      RiskList <- Risk20
      map_data[[Get_Selected_Risk()]] <- map_data[[input$maprisktype3]]
    } else return(NULL)
    
    if (input$filterreg != ''){
      map_data %<>%
        mutate(reg = input$filterreg) %>%
        select(reg, description, all, ends_with(Get_Selected_Risk())) %>% 
        arrange(description)
      names(map_data) <- c('Region', 'Area', 'Total Population', Get_Selected_Risk())
    } else if (input$filterta != ''){
      map_data %<>%
        mutate(tla = input$filterta) %>%
        select(tla, description, all, ends_with(Get_Selected_Risk())) %>% 
        arrange(description)
      names(map_data) <- c('Territorial Authority', 'Area', 'Total Population', Get_Selected_Risk())
    } else {
      map_data %<>% 
        select(description, all, ends_with(Get_Selected_Risk())) %>% 
        arrange(description)
      names(map_data) <- c('Area', 'Total Population', Get_Selected_Risk())
    }
    return(map_data)
  })
  
  output$downloadDataA <- downloadHandler(
    # Add in better name generation...
    filename = function() { 
      Age_Readable <- names(age.list)[sapply(age.list, FUN=function(X) Get_Selected_Age() %in% X)]
      paste0('Output', Get_Selected_Risk(), Age_Readable,  Get_Selected_Area(), input$mapsex, '.csv') },
    content = function(file) { write.csv( DownloadData(), file) }
  )
  
  observeEvent(input$dispgeo, {
    if (input$dispgeo == 'nz'){
      updateSelectInput(session, "mapgeonz",selected="regc")
      updateSelectInput(session, "mapgeoreg",selected="")
      updateSelectInput(session, "mapgeota",selected="")
      updateSelectizeInput(session, "filterreg",selected="")
      updateSelectizeInput(session, "filterta",selected="")
    } else if(input$dispgeo == 'region'){
      updateSelectInput(session, "mapgeoreg",selected="regc")
      updateSelectInput(session, "mapgeonz",selected="")
      updateSelectInput(session, "mapgeota",selected="")
      updateSelectizeInput(session, "filterreg",selected="Auckland Region")
      updateSelectizeInput(session, "filterta",selected="")
    } else if(input$dispgeo == 'ta'){
      updateSelectInput(session, "mapgeota",selected="ta")
      updateSelectInput(session, "mapgeonz",selected="")
      updateSelectInput(session, "mapgeoreg",selected="")
      updateSelectizeInput(session, "filterreg",selected="")
      updateSelectizeInput(session, "filterta",selected="Far North District")
    }
  })
  
  # Register event for updating the region selected.
  observeEvent(input$filterreg, {
    if (input$filterreg != '') {
      dgeom <- copy(Region_Shape_16)
      if (input$filterreg %in% dgeom@data$description) dgeom <- dgeom[dgeom$description == input$filterreg,]
      view_box <- bbox(dgeom)
      leafletProxy("map") %>% fitBounds(view_box[1], view_box[4], view_box[3], view_box[2])
    }
  })
  
  # Register event for updating the territorial authority selected.
  observeEvent(input$filterta, {
    if (input$filterta != '') {
      dgeom <- copy(TA_Shape_16)
      if (input$filterta %in% dgeom@data$description) dgeom <- dgeom[dgeom$description == input$filterta,]
      view_box <- bbox(dgeom)
      leafletProxy("map") %>% fitBounds(view_box[1], view_box[4], view_box[3], view_box[2])
    }
  })
  
  
  
  output$map <- renderLeaflet({
    #  Display a progress bar.
    progress <- shiny::Progress$new()
    on.exit({progress$close()})
    progress$set(message = "Updating Plot - Be Patient", #detail = 'This may take a while...', 
                 value = 0)
    
    # First work out the level of display geography
    if (input$dispgeo == 'nz'){
      # If NZ then use mapgeonz variable
      if (input$mapgeonz=="regc") geom <- Region_Shape_16
      else if (input$mapgeonz=="ta") geom <- TA_Shape_16
      else return(NULL)
    } else if(input$dispgeo == 'region'){
      # If region then use mapgeoreg variable
      if (input$mapgeoreg=="regc") geom <- Region_Shape_16
      else if (input$mapgeoreg=="ta") geom <- TA_Shape_16
      else if (input$mapgeoreg=="au") geom <- Area_Unit_Shape_16
      else return(NULL)
    } else if(input$dispgeo == 'ta'){
      # If ta then use mapgeota variable
      if (input$mapgeota=="ta") geom <- TA_Shape_16
      else if (input$mapgeota=="au") geom <- Area_Unit_Shape_16
      else return(NULL)
    }
    progress$set(value = 0.2)
    
    if (input$dispgeo == 'nz'){
      view_box <- bbox(geom)
      map <- leaflet() %>% clearShapes() %>%
        mapOptions(zoomToLimits = 'never') %>%
        fitBounds(view_box[1], view_box[4], view_box[3], view_box[2])
    } else {
      map <- leafletProxy("map") %>% clearShapes() %>% clearMarkerClusters() %>%
        mapOptions(zoomToLimits = 'never')
    }
    
    # Import the map data
    Extra <- ''
    if (input$mapgeonz=="regc" || input$mapgeoreg=="regc"){
      map_data <- copy(Region_Table) %>%
        filter(sex == input$mapsex, agegrp==input$mapagegrp) %>%
        select(description, starts_with("all")) %>%
        group_by(description) %>%
        summarise_each(funs(sum))
    } else if (input$mapgeonz=="ta" || input$mapgeoreg=="ta" || input$mapgeota=="ta"){
      if(input$dispgeo == 'region'){
        Extra <- Get_Selected_Area()
        map_data <- copy(TA_Table) %>% 
          filter(sex == input$mapsex, agegrp == input$mapagegrp, reg == input$filterreg) %>%
          select(description, starts_with("all")) %>%
          group_by(description) %>%
          summarise_each(funs(sum))
      } else{
        map_data <- copy(TA_Table) %>%
          filter(sex == input$mapsex, agegrp==input$mapagegrp) %>%
          select(description, starts_with("all")) %>%
          group_by(description) %>%
          summarise_each(funs(sum))
      }
    } else if (input$mapgeoreg=="au" || input$mapgeota=="au"){
      Extra <- Get_Selected_Area()
      if(input$dispgeo == 'region'){
        map_data <- copy(Area_Unit_Table) %>% 
          filter(sex == input$mapsex, agegrp == input$mapagegrp, reg == input$filterreg) %>%
          select(description, starts_with("all")) %>%
          group_by(description) %>%
          summarise_each(funs(sum))
      } else if(input$dispgeo == 'ta'){
        map_data <- copy(Area_Unit_Table) %>% 
          filter(sex == input$mapsex, agegrp == input$mapagegrp, tla == input$filterta) %>%
          select(description, starts_with("all")) %>%
          group_by(description) %>%
          summarise_each(funs(sum))
      }
    } else return(NULL)
    
    progress$set(value = 0.4)
    
    # Some further data manipulation
    if (input$mapagegrp == '00-05' || input$mapagegrp == '06-14'){
      risk <- input$maprisktype1
      RiskList <- Risk0
      map_data$at_risk <- map_data[[input$maprisktype1]]
    } else if (input$mapagegrp == '15-19'){
      risk <- input$maprisktype2
      RiskList <- Risk15
      map_data$at_risk <- map_data[[input$maprisktype2]]
    } else if (input$mapagegrp == '20-24'){
      risk <- input$maprisktype3
      RiskList <- Risk20
      map_data$at_risk <- map_data[[input$maprisktype3]]
    } else return(NULL)
    map_data$plot_var <- map_data$at_risk/map_data$all
    map_data$plot_var_num <- map_data$all
    progress$set(value = 0.6)
    
    # Import the shape data
    if (input$mapgeonz=="regc" || input$mapgeoreg=="regc") map_layer <- Region_Shape_16
    else if (input$mapgeonz=="ta" || input$mapgeoreg=="ta" || input$mapgeota=="ta") map_layer <- TA_Shape_16
    else if (input$mapgeonz=="au" || input$mapgeoreg=="au" || input$mapgeota=="au") map_layer <- Area_Unit_Shape_16
    else return(NULL)
    
    if (is.null(map_layer) || is.null(map_data) || nrow(map_data) == 0) return(NULL)
    
    map_layer <- sp::merge(map_layer, map_data, by = c("description"),
                           all.x = T)
    map_layer <- map_layer[!is.na(map_layer$all),]
    map_layer@data$plot_var[is.na(map_layer@data$plot_var)] <- 0 
    
    # Create the popup information
    popup <- ~paste0(description, "<br>", Extra,
                     "<dl><dt># at risk </dt><dd>",
                     at_risk,
                     "</dd><dt>Population </dt><dd>",
                     all, 
                     "</dd><dt>Percentage at risk </dt><dd>", 
                     sprintf("%1.1f%%", 100*plot_var),"</dd></dl>")
    
    # Update the color palette for this variable
    if (min(map_layer@data$plot_var) == max(map_layer@data$plot_var)){
      pal <- colorNumeric(MaroonPalette, domain = c(0,map_layer@data$plot_var,1), na.color="black")
      Legend_Palette <- colorBin(MaroonPalette, c(0,map_layer@data$plot_var,1))
    } else {
      pal <- colorNumeric(MaroonPalette, domain = c(map_layer@data$plot_var), na.color="black")
      Legend_Palette <- colorBin(MaroonPalette, map_layer@data$plot_var)
    }
    
    if (input$mapgeoreg=="au"){
      High_layer <- Region_Shape_16
      High_layer <- High_layer[High_layer$description != input$filterreg,]
    } else if(input$mapgeoreg=="ta"){
      High_layer <- Region_Shape_16
      High_layer <- High_layer[High_layer$description != input$filterreg,]
    } else if(input$mapgeota=="au"){
      High_layer <- TA_Shape_16
      High_layer <- High_layer[High_layer$description != input$filterta,]
    }
    # Create custom legend...
    bins <- sapply(attr(Legend_Palette,"colorArgs")$bins, 
                   function(x) paste0(sprintf('%1.1f',100*x),"%"))
    cols <- Legend_Palette(attr(Legend_Palette,"colorArgs")$bins)
    
    # Finally add the filled polygons and the legend to the map
    if (input$mapgeoreg=="au" || input$mapgeota=="au" || 
        input$mapgeoreg=="ta"){
      map %<>% addPolygons(data=High_layer,
                           opacity=1, weight=1, color="black", fillColor='#FFFF00')
    }
    
    map %<>% # addTiles() %>% # Tiles needed for markerCluster
      addPolygons(data=map_layer,
                  opacity=1, weight=0.75, color="black",
                  fillOpacity=0.8, fillColor=~pal(plot_var),popup=popup
      ) %>%
      addLegend(position = "bottomleft",# pal = pal, values=map_layer@data$plot_var
                colors = rev(cols),
                labels = rev(bins), opacity = 0.8, 
                title = "At risk"#names(RiskList)[sapply(RiskList, FUN=function(X) risk %in% X)]
      )
    
    progress$set(value = 0.8)

    map
  })
  
  
}