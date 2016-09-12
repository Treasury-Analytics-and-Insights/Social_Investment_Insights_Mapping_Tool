UI_Def <- shinyUI(
  # CB - Note to self, cannot have multiple inputs with the same ID, even if they are in ConditionalPanels
  # CB - Use of the absolute panel allows overlay of controls on map.
  # CB - web content must be in /www directory. DO NOT RENAME.  http://shiny.rstudio.com/articles/css.html
  # CB - CSS for Shiny https://github.com/rstudio/shiny/blob/515a67a/inst/www/shared/shiny.css#L94-L114
  # CB - Zoom controls for leaflet map are too far forward, need to adjust leaflet css available from
  #  http://cdn.leafletjs.com/leaflet/v0.7.7/leaflet.css, .leaflet-top, .leaflet-bottom param z-index.
  # Check the css file in the \www directory for progress bar positioning
  # May need some manual tweaking to position progress bar...
  
  # fluidPage - Elements to include within the page.
  list(
  fluidPage(
    tags$head(includeCSS("www/skeleton.css")),
    div(class="x", style="margin: 0 auto; padding: 0; max-width: 1200px;",
        navbarPage("Social Investment Insights", collapsible=TRUE, inverse=F, fluid=TRUE, position = "fixed-top", id="menu",
                   
                   tabPanel("Introduction", 
                            tags$style(type="text/css", "body {padding-top: 70px;}"),
                            div(actionButton("goButton", "Go to Data Visualisation!"), align='center'),
                            div(includeHTML("about.html"), style="width: 100%; height: 100%; overflow: auto; padding-right: 10px;"))
                   ,
                   tabPanel("Data Visualisation", tags$style(type="text/css", "body {padding-top: 70px;}"),
                            mainPanel(class="controls",
                                      top="10px", right="10px", bottom="auto", left="auto", width=200,
                                      draggable=FALSE, fixed = FALSE,
                                      fluidRow(column(6,
                                                      helpText("View Type"),
                                                      selectInput("dispgeo", NULL, 
                                                                  choices=c("Display Geography" = "", "New Zealand" = "nz", "Region" = "region", "Territorial Authority" = "ta"),
                                                                  selected = "nz"),
                                                      conditionalPanel("input.dispgeo=='nz'",
                                                                       helpText("Geographical Unit"),
                                                                       selectInput("mapgeonz", NULL,
                                                                                   choices=c("choose geography"="",  "Region"="regc", "Territorial Authority"="ta"),
                                                                                   selected = "regc")
                                                      ),
                                                      conditionalPanel("input.dispgeo=='region'",
                                                                       helpText("Choose Region"),
                                                                       selectizeInput("filterreg", NULL,
                                                                                      choices=c(region.list))
                                                      ),
                                                      conditionalPanel("input.dispgeo=='region'",
                                                                       helpText("Geographical Unit"),
                                                                       selectInput("mapgeoreg", NULL,
                                                                                   choices=c("choose geography"="",  "Region"="regc", "Territorial Authority"="ta", "Area Unit"="au"))
                                                      ),
                                                      conditionalPanel("input.dispgeo=='ta'",
                                                                       helpText("Choose T.A."),
                                                                       selectizeInput("filterta", NULL,
                                                                                      choices=c(ta.list))
                                                      ),
                                                      conditionalPanel("input.dispgeo=='ta'",
                                                                       helpText("Geographical Unit"),
                                                                       selectInput("mapgeota", NULL,
                                                                                   choices=c("choose geography"="",  "Territorial Authority"="ta", "Area Unit"="au"))
                                                      )),
                                               column(6,
                                                      helpText("Gender"),
                                                      selectInput("mapsex", NULL,
                                                                  choices=c("choose sex"="", sex.list),
                                                                  selected = "All"),
                                                      helpText("Age Group"),
                                                      selectInput("mapagegrp", NULL,
                                                                  choices=c("choose age group"="", age.list),
                                                                  selected = "00-05"),
                                                      helpText("Risk Measure"),
                                                      conditionalPanel("input.mapagegrp=='00-05'||input.mapagegrp=='06-14'",
                                                                       selectInput("maprisktype1", NULL,
                                                                                   choices=c("choose risk type"="",  Risk0),
                                                                                   selected = "all_risk_2")
                                                      ),
                                                      conditionalPanel("input.mapagegrp=='15-19'",
                                                                       selectInput("maprisktype2", NULL,
                                                                                   choices=c("choose risk type"="",  Risk15),
                                                                                   selected = "all_risk_1")
                                                      ),
                                                      conditionalPanel("input.mapagegrp=='20-24'",
                                                                       selectInput("maprisktype3", NULL,
                                                                                   choices=c("choose risk type"="",  Risk20),
                                                                                   selected = "all_risk_1")
                                                      )))
                            ),
                            div(class = 'shiny-split-layout',
                            #splitLayout(
                                         div(style="border: 1px solid black; padding-left:0px;",
                                             leafletOutput("map", height = 800)
                            ),
                            # Now for the information panel...
                            div(
                              # Title
                              div(
                                h4("Summary Information"),
                                # Summary Table
                                helpText(textOutput("DemoDesc")),
                                htmlOutput("DemoTable")
                              ), 
                              conditionalPanel("input.dispgeo!='nz'",div(
                                # Comparison Table
                                helpText(textOutput("Area")),
                                htmlOutput("ComparisonTable")
                              )), 
                              div(
                                # Comparison Table
                                helpText(textOutput("National")),
                                tableOutput("NationalTable")
                              ),
                              div(
                                # Export links
                                helpText("Export Data to csv"),
                                downloadButton('downloadDataA', 'Map Data')
                              )          
                            )))
                   # Placeholder for next tab
        )),
    div(includeHTML("www/footer.html"), style="margin: 0 auto; padding-top: 20px; max-width: 1200px;"),
    style="margin: 0; padding: 0;"), includeScript("google-analytics.js"))
)