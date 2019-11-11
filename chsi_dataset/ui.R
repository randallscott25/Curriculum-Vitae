#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
varlist <- setdiff(names(ale.1), "geometry")

runApp(list(
    ui = fluidPage(
        titlePanel("Average Life Expectancies"),
        sidebarLayout(
            sidebarPanel(
                selectInput("var", label = "Variable", choices = varlist, selected = "pop_est_dens")
            ),
            mainPanel(
                leafletOutput("map")
            )
        )
    ),
    server = function(input, output) {
        output$map = renderLeaflet({
            if (packageVersion("tmap") >= 2.0) {
                tm <- tm_basemap(leaflet::providers$Stamen.TerrainBackground) +
                    tm_shape(ale.Map)  +
                    tm_polygons(input$var) +
                    tm_tiles(leaflet::providers$Stamen.TonerLabels, group = "Labels")
                
            } else {
                tm <- tm_shape(ale.Map) +
                    tm_polygons(input$var) +
                    tm_view(basemaps = "Stamen.TerrainBackground")
            }
            
            tmap_leaflet(tm)
        })
    }
))
