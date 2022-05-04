library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(rsconnect)
library(leaflet)
library(plotly)
library(tmap)
library(tidyverse)
library(DT)
library(ggplot2)
library(gganimate)
library(gridExtra)
# options("rgdal_show_exportToProj4_warnings"="none") 
# library(rgdal)

# Import data
load("WQS_deficit_APP.Rdata")
# D <- read.csv("WQS_deficit_APP_data.csv", header = TRUE)
# d <- read.csv("WQS_deficit_segment_APP_data.csv", header = TRUE)
# mk <- read.csv("WQS_deficit_trend_APP_data.csv", header = TRUE)

D[, -1] <- round(D[, -1], 2)
d$Attainment <- round(d$Attainment, 2)
d <- d %>% rename(attainment_deficit = Attainment)
d$DU <- as.character(d$DU)
d$DU <- factor(d$DU, levels = c("MSN", "OW", "DW", "DC", "CHLspr", "CHLsum"))

# UI
ui <- fluidPage(
    
    # Add a theme
    theme = shinytheme("flatly"),
    
    # Add a title panel
    titlePanel("Chesapeake Bay Water Quality Standards Attainment Deficit"),
    
    
    # Add a disclaimer
    print("This R Shiny APP is designed for visualizing the water quality standards attainment deficit for the tidal segments of Chesapeake Bay."),
    br(),
    print("Notes:"),
    br(),
    print("1. Attainment deficit: 0% = full attainment; -100% = full non-attainment."),
    br(),
    print("2. Designated use: OW = open water dissolved oxygen (DO); DW = deep water DO; DC = deep channel DO; MSN = migratory spawning and nursery DO; CHLspr = chlorophyll-a (spring); CHLsum = chlorophyll-a (summer)."),
    br(),
    print("3. Assessment period: Year represents the first year of a 3-year assessment period, e.g., year 1985 represents the 1985-1987 period; year 2018 represents the 2018-2020 period."),
    br(),
    print("4. Official Website: https://www.chesapeakeprogress.com/clean-water/water-quality."),
    br(),
    print("5. References: https://doi.org/10.1016/j.scitotenv.2018.05.025; https://doi.org/10.3389/fmars.2018.00422."),
    br(),
    print("6. For questions or feedback, please contact Qian Zhang (qzhang@chesapeakebay.net)."),
    hr(),
    
    navbarPage(
        "",
        
        # Section 1
        tabPanel(
            "System",
            mainPanel(
                tabsetPanel(
                    tabPanel("Tidal System", plotlyOutput("myfigure1", "800px", "1500px")),
                    tabPanel("Data Table", dataTableOutput("mytable1")),
                )
            )
        ),
        
        # Section 2
        tabPanel(
            "Segment Table",
            sidebarLayout(
                sidebarPanel(
                    
                    # Add a download button
                    downloadButton(outputId = "download_data", label = "Download Data"),
                    helpText("Note: Click this button to download all data. If user inputs are provided below, only the selected data will be downloaded."),
                    br(),
                    selectInput(
                        inputId = "Segment",
                        label = "Specify the segments to show data",
                        choices = c("All", as.character(unique(d$Segment))),
                        multiple = TRUE,
                        selected = c("All"),
                        width = NULL
                    ),
                    helpText("Note: Multiple or all segments can be selected."),
                    br(),
                    checkboxGroupInput(
                        inputId = "DU",
                        label = "Specify the designated use (DU) to show data",
                        choices = c("All DUs", "MSN", "OW", "DW", "DC", "CHLspr", "CHLsum"),
                        selected = c("All DUs")
                    ),
                    helpText("Note: Multiple or all DUs can be selected."),
                    br(),
                    sliderInput(
                        inputId = "Year",
                        label = "Specify the years to show data",
                        min = 1985,
                        max = 2018,
                        value = c(1985, 2018),
                        width = NULL
                    ),
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel("Data Table", dataTableOutput("mytable2"))
                    )
                )
            )
        ),
        
        # Section 3
        tabPanel(
            "Segment Figure",
            sidebarLayout(
                sidebarPanel(
                    
                    selectInput(
                        inputId = "Segment_for_figure",
                        label = "Specify the segment to show time series",
                        choices = c(as.character(unique(d$Segment))),
                        multiple = FALSE,
                        selected = c("CB4MH"),
                        width = NULL
                    ),
                    br(),
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel("Data Figure", plotlyOutput("myfigure2", "800px", "900px")),
                    )
                )
            )
        ),
        
        # Section 4
        tabPanel(
            "Segment Map",
            sidebarLayout(
                sidebarPanel(
                    sliderInput(
                        inputId = "Year1",
                        label = "Specify the year to show maps",
                        min = 1985,
                        max = 2018,
                        value = 2018,
                        width = NULL
                    ),
                    helpText("Note: Default = 2018, i.e., the 2018-2020 assessment period."),
                    helpText("Note: It takes ~15 seconds to generate the map.")
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel("MSN", tmapOutput("mymap1", "800px", "800px")),
                        tabPanel("OW", tmapOutput("mymap2", "800px", "800px")),
                        tabPanel("DW", tmapOutput("mymap3", "800px", "800px")),
                        tabPanel("DC", tmapOutput("mymap4", "800px", "800px")),
                        tabPanel("CHLspr", tmapOutput("mymap5", "800px", "800px")),
                        tabPanel("CHLsum", tmapOutput("mymap6", "800px", "800px"))
                    )
                )
            )
        ),
        
        # Section 5
        tabPanel(
            "Segment Comparison",
            sidebarLayout(
                sidebarPanel(
                    sliderInput(
                        inputId = "Y1",
                        label = "Specify the 1st year for comparison",
                        min = 1985,
                        max = 2018,
                        value = 2017,
                        width = NULL
                    ),
                    helpText("Note: Default = 2017, i.e., the 2017-2019 assessment period."),
                    sliderInput(
                        inputId = "Y2",
                        label = "Specify the 2nd year for comparison",
                        min = 1985,
                        max = 2018,
                        value = 2018,
                        width = NULL
                    ),
                    helpText("Note: Default = 2018, i.e., the 2018-2020 assessment period."),
                    helpText("Note: It takes ~30 seconds to generate the map.")
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel("Table", dataTableOutput("mytable3")),
                        tabPanel("MSN", fluidRow(
                            column(6, tmapOutput("mymap1A", width = "100%", height = 800)),
                            column(6, tmapOutput("mymap1B", width = "100%", height = 800))
                        )),
                        tabPanel("OW", fluidRow(
                            column(6, tmapOutput("mymap2A", width = "100%", height = 800)),
                            column(6, tmapOutput("mymap2B", width = "100%", height = 800))
                        )),
                        tabPanel("DW", fluidRow(
                            column(6, tmapOutput("mymap3A", width = "100%", height = 800)),
                            column(6, tmapOutput("mymap3B", width = "100%", height = 800))
                        )),
                        tabPanel("DC", fluidRow(
                            column(6, tmapOutput("mymap4A", width = "100%", height = 800)),
                            column(6, tmapOutput("mymap4B", width = "100%", height = 800))
                        )),
                        tabPanel("CHLspr", fluidRow(
                            column(6, tmapOutput("mymap5A", width = "100%", height = 800)),
                            column(6, tmapOutput("mymap5B", width = "100%", height = 800))
                        )),
                        tabPanel("CHLsum", fluidRow(
                            column(6, tmapOutput("mymap6A", width = "100%", height = 800)),
                            column(6, tmapOutput("mymap6B", width = "100%", height = 800))
                        ))
                    )
                )
            )
        ),
        
        # Section 6
        tabPanel(
            "Segment Animation",
            mainPanel(
                tabsetPanel(
                    tabPanel("MSN", imageOutput("mygif1", "800px", "800px")),
                    tabPanel("OW", imageOutput("mygif2", "800px", "800px")),
                    tabPanel("DW", imageOutput("mygif3", "800px", "800px")),
                    tabPanel("DC", imageOutput("mygif4", "800px", "800px")),
                    tabPanel("CHLspr", imageOutput("mygif5", "800px", "800px")),
                    tabPanel("CHLsum", imageOutput("mygif6", "800px", "800px"))
                )
            )
        ),
        
        # Section 7
        tabPanel(
            "Trend Map",
            sidebarLayout(
                sidebarPanel(
                    radioButtons(
                        inputId = "Trend_DU",
                        label = "Specify the designated use (DU) for 1985-2020 trends",
                        choices = c("MSN", "OW", "DW", "DC", "CHLspr", "CHLsum"),
                        selected = "MSN"
                    ),
                    br(),
                    radioButtons(
                        inputId = "p.value",
                        label = "Specify the significance level for 1985-2020 trends",
                        choices = c(
                            "All Trends" = "1.0",
                            "p-value < 0.1" = "0.1",
                            "p-value < 0.05" = "0.05"
                        ),
                        selected = "1.0"
                    ),
                    helpText("Note: Default = 1.0, i.e., showing all computed long-term trends."),
                    helpText("Note: It takes ~15 seconds to generate the map.")
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel("Trend Map", tmapOutput("mymap0", "800px", "800px"))
                    )
                )
            )
        )
        
    )
)

server <- function(input, output, session) {
    # Making a reactive object called points where we filter the data
    # filtered_data <- reactive({
    #  data <- subset(d, Segment %in% input$Segment)
    #  data
    # })

    # Table output 1
    output$mytable1 <- renderDataTable(
        {
            D
        },
        options = list(iDisplayLength = 50)
    )
    
    # Table output 2
    output$mytable2 <- renderDataTable(
        {
            # Complete the data frame (mainly for CHLspr and CHLsum)
            d <- d %>% 
                complete(Segment, Year, DU)
            
            if (mean(input$Segment == "All") == 1) {
                data1 <- d
            }
            
            if (mean(input$Segment == "All") < 1) {
                selection <- as.vector(input$Segment[input$Segment != "All"])
                data1 <- d %>%
                    filter(Segment %in% selection)
            }
            
            data2 <- data1 %>%
                spread(key = "DU", value = "attainment_deficit") %>%
                filter(Year >= input$Year[1]) %>%
                filter(Year <= input$Year[2]) %>%
                select(Segment, Class, System, Year, MSN, OW, DW, DC, CHLspr, CHLsum)
            
            if (mean(input$DU == "All DUs") == 1) {
                data3 <- data2
            }
            
            if (mean(input$DU == "All DUs") < 1) {
                selection <- as.vector(input$DU[input$DU != "All DUs"])
                data3 <- data2[, c("Segment", "Class", "System", "Year", selection)]
            }

            # Only show rows with at least one non-NA value beyond Segment, Class, System, and Year
            data3 <- data3[rowSums(!is.na(data3)) > 4, ]
            data3
        },
        options = list(aLengthMenu = c(10, 25, 50, 100), iDisplayLength = 50)
    )
    
    # Table output 3
    output$mytable3 <- renderDataTable(
        {
            data1 <- d %>%
                spread(key = "DU", value = "attainment_deficit") %>%
                filter(Year == input$Y1) %>%
                select(-Year)
            
            data2 <- d %>%
                spread(key = "DU", value = "attainment_deficit") %>%
                filter(Year == input$Y2) %>%
                select(-Year)
            
            data <- data1
            data[, 4:9] <- data2[, 4:9] - data1[, 4:9]
            data
        },
        options = list(aLengthMenu = c(10, 25, 50, 100), iDisplayLength = 50)
    )
    
    # Create a download handler
    output$download_data <- downloadHandler(
        filename = "CB_WQS_attainment_deficit_data.csv",
        content = function(file) {
            # Complete the data frame (mainly for CHLspr and CHLsum)
            d <- d %>% 
                complete(Segment, Year, DU)
            
            if (mean(input$Segment == "All") == 1) {
                data1 <- d
            }
            
            if (mean(input$Segment == "All") < 1) {
                selection <- as.vector(input$Segment[input$Segment != "All"])
                data1 <- d %>%
                    filter(Segment %in% selection)
            }
            
            data2 <- data1 %>%
                spread(key = "DU", value = "attainment_deficit") %>%
                filter(Year >= input$Year[1]) %>%
                filter(Year <= input$Year[2]) %>%
                select(Segment, Class, System, Year, MSN, OW, DW, DC, CHLspr, CHLsum)
            
            if (mean(input$DU == "All DUs") == 1) {
                data3 <- data2
            }
            
            if (mean(input$DU == "All DUs") < 1) {
                selection <- as.vector(input$DU[input$DU != "All DUs"])
                data3 <- data2[, c("Segment", "Class", "System", "Year", selection)]
            }
            
            # Only show rows with at least one non-NA value beyond Segment, Class, System, and Year
            data3 <- data3[rowSums(!is.na(data3)) > 4, ]
            data3
            
            # Write the filtered data into a CSV file
            write.csv(data3, file, row.names = FALSE)
        }
    )
    
    # Figure Output 1
    output$myfigure1 <- renderPlotly({
        ggplotly({
            D1 <- D %>%
                gather(key = "System", value = "attainment_deficit", -Year) %>%
                mutate(System = factor(System, levels = names(D)[-1]))
            
            ggplot(D1, aes(x = Year, y = attainment_deficit)) +
                geom_point(pch = 16, col = "deepskyblue", cex = 2) +
                geom_line(lwd = 1) +
                theme_bw(base_size = 15) +
                ylim(-40, 0) +
                geom_smooth(method = "loess") +
                labs(
                    x = "Assessment period", y = "Attainment deficit, percent",
                    # title = "Attainment deficit by desginated use or tidal system",
                    subtitle = ""
                ) +
                facet_wrap(~System, ncol = 3)
        })
    })
    
    # Figure Output 2
    output$myfigure2 <- renderPlotly({
        data1 <- d %>% 
            mutate(DU == factor(DU, levels = c("MSN", "OW", "DW", "DC", "CHLspr", "CHLsum"))) %>% 
            filter(Segment %in% input$Segment_for_figure) %>%
            complete(Year, DU)
            # filter(!is.na(attainment_deficit))
            # mutate(status = ifelse(attainment_deficit == 0, 16, 1))

        ggplotly({
            ggplot(data1, aes(x = Year, y = attainment_deficit)) +
                geom_point(col = "deepskyblue", cex = 2, pch = 1) +
                geom_line(lwd = 1) +
                theme_bw(base_size = 15) +
                ylim(-100, 0) +
                geom_smooth(method = "loess") +
                labs(
                    x = "Assessment period", y = "Attainment deficit, percent",
                    title = input$Segment_for_figure,
                    subtitle = ""
                ) +
                facet_wrap(~DU, ncol = 2)
        })
    })
    
    # Map 0
    output$mymap0 <- renderTmap({
        d1 <- mk %>%
            filter(DU == input$Trend_DU) %>%
            filter(!is.na(Slope)) %>%
            mutate(Slope = as.numeric(Slope))
        
        if (input$p.value == "1.0") {
            d2 <- d1
        }
        if (input$p.value == "0.1") {
            d2 <- d1 %>% filter(p.value < 0.1)
        }
        if (input$p.value == "0.05") {
            d2 <- d1 %>% filter(p.value < 0.05)
        }
        
        cb1 <- CB_92_segments
        cb1@data <- left_join(cb1@data, d2, by = c("CBSEG_92" = "Segment"))
        DU_trend <- cb1
        
        tm_shape(DU_trend) +
            tm_fill("Slope",
                    title = paste(input$Trend_DU, " 1985-2020 Trend", sep = ""), id = "CBSEG_92",
                    showNA = TRUE, colorNA = "grey", midpoint = 0,
                    alpha = 1
            ) +
            tm_layout(scale = 0.9) +
            tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
            tm_shape(CB_92_segments) +
            tm_borders(col = "black")
    })
    
    # Map 1
    output$mymap1 <- renderTmap({
        d1 <- d %>%
            filter(Year == input$Year1) %>%
            filter(DU == "MSN")
        cb1 <- CB_92_segments
        cb1@data <- left_join(cb1@data, d1, by = c("CBSEG_92" = "Segment"))
        DU_attainment <- cb1
        
        tm_shape(DU_attainment) +
            tm_fill("attainment_deficit",
                    title = paste("MSN ", input$Year1, "-", input$Year1 + 2, sep = ""),
                    id = "CBSEG_92", showNA = TRUE, colorNA = "grey",
                    alpha = 0.8, palette = rev(c(
                        "darkcyan", "cadetblue1", "cornsilk", "gold",
                        "darkorange2", "darkorange4"
                    )),
                    breaks = rev(c(0, -0.1, -5, -10, -25, -50, -100))
            ) +
            tm_layout(scale = 0.9) +
            tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
            tm_shape(CB_92_segments) +
            tm_borders(col = "black")
    })
    
    # Map 2
    output$mymap2 <- renderTmap({
        d1 <- d %>%
            filter(Year == input$Year1) %>%
            filter(DU == "OW")
        cb1 <- CB_92_segments
        cb1@data <- left_join(cb1@data, d1, by = c("CBSEG_92" = "Segment"))
        DU_attainment <- cb1
        
        tm_shape(DU_attainment) +
            tm_fill("attainment_deficit",
                    title = paste("OW ", input$Year1, "-", input$Year1 + 2, sep = ""),
                    id = "CBSEG_92", showNA = TRUE, colorNA = "grey",
                    alpha = 0.8, palette = rev(c(
                        "darkcyan", "cadetblue1", "cornsilk", "gold",
                        "darkorange2", "darkorange4"
                    )),
                    breaks = rev(c(0, -0.1, -5, -10, -25, -50, -100))
            ) +
            tm_layout(scale = 0.9) +
            tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
            tm_shape(CB_92_segments) +
            tm_borders(col = "black")
    })
    
    # Map 3
    output$mymap3 <- renderTmap({
        d1 <- d %>%
            filter(Year == input$Year1) %>%
            filter(DU == "DW")
        cb1 <- CB_92_segments
        cb1@data <- left_join(cb1@data, d1, by = c("CBSEG_92" = "Segment"))
        DU_attainment <- cb1
        
        tm_shape(DU_attainment) +
            tm_fill("attainment_deficit",
                    title = paste("DW ", input$Year1, "-", input$Year1 + 2, sep = ""),
                    id = "CBSEG_92", showNA = TRUE, colorNA = "grey",
                    alpha = 0.8, palette = rev(c(
                        "darkcyan", "cadetblue1", "cornsilk", "gold",
                        "darkorange2", "darkorange4"
                    )),
                    breaks = rev(c(0, -0.1, -5, -10, -25, -50, -100))
            ) +
            tm_layout(scale = 0.9) +
            tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
            tm_shape(CB_92_segments) +
            tm_borders(col = "black")
    })
    
    # Map 4
    output$mymap4 <- renderTmap({
        d1 <- d %>%
            filter(Year == input$Year1) %>%
            filter(DU == "DC")
        cb1 <- CB_92_segments
        cb1@data <- left_join(cb1@data, d1, by = c("CBSEG_92" = "Segment"))
        DU_attainment <- cb1
        
        tm_shape(DU_attainment) +
            tm_fill("attainment_deficit",
                    title = paste("DC ", input$Year1, "-", input$Year1 + 2, sep = ""),
                    id = "CBSEG_92", showNA = TRUE, colorNA = "grey",
                    alpha = 0.8, palette = rev(c(
                        "darkcyan", "cadetblue1", "cornsilk", "gold",
                        "darkorange2", "darkorange4"
                    )),
                    breaks = rev(c(0, -0.1, -5, -10, -25, -50, -100))
            ) +
            tm_layout(scale = 0.9) +
            tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
            tm_shape(CB_92_segments) +
            tm_borders(col = "black")
    })
    
    # Map 5
    output$mymap5 <- renderTmap({
        d1 <- d %>%
            filter(Year == input$Year1) %>%
            filter(DU == "CHLspr")
        cb1 <- CB_92_segments
        cb1@data <- left_join(cb1@data, d1, by = c("CBSEG_92" = "Segment"))
        DU_attainment <- cb1
        
        tm_shape(DU_attainment) +
            tm_fill("attainment_deficit",
                    title = paste("CHLspr ", input$Year1, "-", input$Year1 + 2, sep = ""),
                    id = "CBSEG_92", showNA = TRUE, colorNA = "grey",
                    alpha = 0.8, palette = rev(c(
                        "darkcyan", "cadetblue1", "cornsilk", "gold",
                        "darkorange2", "darkorange4"
                    )),
                    breaks = rev(c(0, -0.1, -5, -10, -25, -50, -100))
            ) +
            tm_layout(scale = 0.9) +
            tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
            tm_shape(CB_92_segments) +
            tm_borders(col = "black")
    })
    
    # Map 6
    output$mymap6 <- renderTmap({
        d1 <- d %>%
            filter(Year == input$Year1) %>%
            filter(DU == "CHLsum")
        cb1 <- CB_92_segments
        cb1@data <- left_join(cb1@data, d1, by = c("CBSEG_92" = "Segment"))
        DU_attainment <- cb1
        
        tm_shape(DU_attainment) +
            tm_fill("attainment_deficit",
                    title = paste("CHLsum ", input$Year1, "-", input$Year1 + 2, sep = ""),
                    id = "CBSEG_92", showNA = TRUE, colorNA = "grey",
                    alpha = 0.8, palette = rev(c(
                        "darkcyan", "cadetblue1", "cornsilk", "gold",
                        "darkorange2", "darkorange4"
                    )),
                    breaks = rev(c(0, -0.1, -5, -10, -25, -50, -100))
            ) +
            tm_layout(scale = 0.9) +
            tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
            tm_shape(CB_92_segments) +
            tm_borders(col = "black")
    })
    
    # Map Comparison 1
    output$mymap1A <- renderTmap({
        d1 <- d %>%
            filter(Year == input$Y1) %>%
            filter(DU == "MSN")
        cb1 <- CB_92_segments
        cb1@data <- left_join(cb1@data, d1, by = c("CBSEG_92" = "Segment"))
        DU_attainment <- cb1
        
        tm_shape(DU_attainment) +
            tm_fill("attainment_deficit",
                    title = paste("MSN ", input$Y1, "-", input$Y1 + 2, sep = ""),
                    id = "CBSEG_92", showNA = TRUE, colorNA = "grey",
                    alpha = 0.8, palette = rev(c(
                        "darkcyan", "cadetblue1", "cornsilk", "gold",
                        "darkorange2", "darkorange4"
                    )),
                    breaks = rev(c(0, -0.1, -5, -10, -25, -50, -100))
            ) +
            tm_layout(scale = 0.9) +
            tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
            tm_shape(CB_92_segments) +
            tm_borders(col = "black")
    })
    
    output$mymap1B <- renderTmap({
        d1 <- d %>%
            filter(Year == input$Y2) %>%
            filter(DU == "MSN")
        cb1 <- CB_92_segments
        cb1@data <- left_join(cb1@data, d1, by = c("CBSEG_92" = "Segment"))
        DU_attainment <- cb1
        
        tm_shape(DU_attainment) +
            tm_fill("attainment_deficit",
                    title = paste("MSN ", input$Y2, "-", input$Y2 + 2, sep = ""),
                    id = "CBSEG_92", showNA = TRUE, colorNA = "grey",
                    alpha = 0.8, palette = rev(c(
                        "darkcyan", "cadetblue1", "cornsilk", "gold",
                        "darkorange2", "darkorange4"
                    )),
                    breaks = rev(c(0, -0.1, -5, -10, -25, -50, -100))
            ) +
            tm_layout(scale = 0.9) +
            tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
            tm_shape(CB_92_segments) +
            tm_borders(col = "black")
    })
    
    # Map Comparison 2
    output$mymap2A <- renderTmap({
        d1 <- d %>%
            filter(Year == input$Y1) %>%
            filter(DU == "OW")
        cb1 <- CB_92_segments
        cb1@data <- left_join(cb1@data, d1, by = c("CBSEG_92" = "Segment"))
        DU_attainment <- cb1
        
        tm_shape(DU_attainment) +
            tm_fill("attainment_deficit",
                    title = paste("OW ", input$Y1, "-", input$Y1 + 2, sep = ""),
                    id = "CBSEG_92", showNA = TRUE, colorNA = "grey",
                    alpha = 0.8, palette = rev(c(
                        "darkcyan", "cadetblue1", "cornsilk", "gold",
                        "darkorange2", "darkorange4"
                    )),
                    breaks = rev(c(0, -0.1, -5, -10, -25, -50, -100))
            ) +
            tm_layout(scale = 0.9) +
            tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
            tm_shape(CB_92_segments) +
            tm_borders(col = "black")
    })
    
    output$mymap2B <- renderTmap({
        d1 <- d %>%
            filter(Year == input$Y2) %>%
            filter(DU == "OW")
        cb1 <- CB_92_segments
        cb1@data <- left_join(cb1@data, d1, by = c("CBSEG_92" = "Segment"))
        DU_attainment <- cb1
        
        tm_shape(DU_attainment) +
            tm_fill("attainment_deficit",
                    title = paste("OW ", input$Y2, "-", input$Y2 + 2, sep = ""),
                    id = "CBSEG_92", showNA = TRUE, colorNA = "grey",
                    alpha = 0.8, palette = rev(c(
                        "darkcyan", "cadetblue1", "cornsilk", "gold",
                        "darkorange2", "darkorange4"
                    )),
                    breaks = rev(c(0, -0.1, -5, -10, -25, -50, -100))
            ) +
            tm_layout(scale = 0.9) +
            tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
            tm_shape(CB_92_segments) +
            tm_borders(col = "black")
    })
    
    # Map Comparison 3
    output$mymap3A <- renderTmap({
        d1 <- d %>%
            filter(Year == input$Y1) %>%
            filter(DU == "DW")
        cb1 <- CB_92_segments
        cb1@data <- left_join(cb1@data, d1, by = c("CBSEG_92" = "Segment"))
        DU_attainment <- cb1
        
        tm_shape(DU_attainment) +
            tm_fill("attainment_deficit",
                    title = paste("DW ", input$Y1, "-", input$Y1 + 2, sep = ""),
                    id = "CBSEG_92", showNA = TRUE, colorNA = "grey",
                    alpha = 0.8, palette = rev(c(
                        "darkcyan", "cadetblue1", "cornsilk", "gold",
                        "darkorange2", "darkorange4"
                    )),
                    breaks = rev(c(0, -0.1, -5, -10, -25, -50, -100))
            ) +
            tm_layout(scale = 0.9) +
            tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
            tm_shape(CB_92_segments) +
            tm_borders(col = "black")
    })
    
    output$mymap3B <- renderTmap({
        d1 <- d %>%
            filter(Year == input$Y2) %>%
            filter(DU == "DW")
        cb1 <- CB_92_segments
        cb1@data <- left_join(cb1@data, d1, by = c("CBSEG_92" = "Segment"))
        DU_attainment <- cb1
        
        tm_shape(DU_attainment) +
            tm_fill("attainment_deficit",
                    title = paste("DW ", input$Y2, "-", input$Y2 + 2, sep = ""),
                    id = "CBSEG_92", showNA = TRUE, colorNA = "grey",
                    alpha = 0.8, palette = rev(c(
                        "darkcyan", "cadetblue1", "cornsilk", "gold",
                        "darkorange2", "darkorange4"
                    )),
                    breaks = rev(c(0, -0.1, -5, -10, -25, -50, -100))
            ) +
            tm_layout(scale = 0.9) +
            tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
            tm_shape(CB_92_segments) +
            tm_borders(col = "black")
    })
    
    # Map Comparison 4
    output$mymap4A <- renderTmap({
        d1 <- d %>%
            filter(Year == input$Y1) %>%
            filter(DU == "DC")
        cb1 <- CB_92_segments
        cb1@data <- left_join(cb1@data, d1, by = c("CBSEG_92" = "Segment"))
        DU_attainment <- cb1
        
        tm_shape(DU_attainment) +
            tm_fill("attainment_deficit",
                    title = paste("DC ", input$Y1, "-", input$Y1 + 2, sep = ""),
                    id = "CBSEG_92", showNA = TRUE, colorNA = "grey",
                    alpha = 0.8, palette = rev(c(
                        "darkcyan", "cadetblue1", "cornsilk", "gold",
                        "darkorange2", "darkorange4"
                    )),
                    breaks = rev(c(0, -0.1, -5, -10, -25, -50, -100))
            ) +
            tm_layout(scale = 0.9) +
            tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
            tm_shape(CB_92_segments) +
            tm_borders(col = "black")
    })
    
    output$mymap4B <- renderTmap({
        d1 <- d %>%
            filter(Year == input$Y2) %>%
            filter(DU == "DC")
        cb1 <- CB_92_segments
        cb1@data <- left_join(cb1@data, d1, by = c("CBSEG_92" = "Segment"))
        DU_attainment <- cb1
        
        tm_shape(DU_attainment) +
            tm_fill("attainment_deficit",
                    title = paste("DC ", input$Y2, "-", input$Y2 + 2, sep = ""),
                    id = "CBSEG_92", showNA = TRUE, colorNA = "grey",
                    alpha = 0.8, palette = rev(c(
                        "darkcyan", "cadetblue1", "cornsilk", "gold",
                        "darkorange2", "darkorange4"
                    )),
                    breaks = rev(c(0, -0.1, -5, -10, -25, -50, -100))
            ) +
            tm_layout(scale = 0.9) +
            tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
            tm_shape(CB_92_segments) +
            tm_borders(col = "black")
    })
    
    # Map Comparison 5
    output$mymap5A <- renderTmap({
        d1 <- d %>%
            filter(Year == input$Y1) %>%
            filter(DU == "CHLspr")
        cb1 <- CB_92_segments
        cb1@data <- left_join(cb1@data, d1, by = c("CBSEG_92" = "Segment"))
        DU_attainment <- cb1
        
        tm_shape(DU_attainment) +
            tm_fill("attainment_deficit",
                    title = paste("CHLspr ", input$Y1, "-", input$Y1 + 2, sep = ""),
                    id = "CBSEG_92", showNA = TRUE, colorNA = "grey",
                    alpha = 0.8, palette = rev(c(
                        "darkcyan", "cadetblue1", "cornsilk", "gold",
                        "darkorange2", "darkorange4"
                    )),
                    breaks = rev(c(0, -0.1, -5, -10, -25, -50, -100))
            ) +
            tm_layout(scale = 0.9) +
            tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
            tm_shape(CB_92_segments) +
            tm_borders(col = "black")
    })
    
    output$mymap5B <- renderTmap({
        d1 <- d %>%
            filter(Year == input$Y2) %>%
            filter(DU == "CHLspr")
        cb1 <- CB_92_segments
        cb1@data <- left_join(cb1@data, d1, by = c("CBSEG_92" = "Segment"))
        DU_attainment <- cb1
        
        tm_shape(DU_attainment) +
            tm_fill("attainment_deficit",
                    title = paste("CHLspr ", input$Y2, "-", input$Y2 + 2, sep = ""),
                    id = "CBSEG_92", showNA = TRUE, colorNA = "grey",
                    alpha = 0.8, palette = rev(c(
                        "darkcyan", "cadetblue1", "cornsilk", "gold",
                        "darkorange2", "darkorange4"
                    )),
                    breaks = rev(c(0, -0.1, -5, -10, -25, -50, -100))
            ) +
            tm_layout(scale = 0.9) +
            tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
            tm_shape(CB_92_segments) +
            tm_borders(col = "black")
    })
    
    # Map Comparison 6
    output$mymap6A <- renderTmap({
        d1 <- d %>%
            filter(Year == input$Y1) %>%
            filter(DU == "CHLsum")
        cb1 <- CB_92_segments
        cb1@data <- left_join(cb1@data, d1, by = c("CBSEG_92" = "Segment"))
        DU_attainment <- cb1
        
        tm_shape(DU_attainment) +
            tm_fill("attainment_deficit",
                    title = paste("CHLsum ", input$Y1, "-", input$Y1 + 2, sep = ""),
                    id = "CBSEG_92", showNA = TRUE, colorNA = "grey",
                    alpha = 0.8, palette = rev(c(
                        "darkcyan", "cadetblue1", "cornsilk", "gold",
                        "darkorange2", "darkorange4"
                    )),
                    breaks = rev(c(0, -0.1, -5, -10, -25, -50, -100))
            ) +
            tm_layout(scale = 0.9) +
            tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
            tm_shape(CB_92_segments) +
            tm_borders(col = "black")
    })
    
    output$mymap6B <- renderTmap({
        d1 <- d %>%
            filter(Year == input$Y2) %>%
            filter(DU == "CHLsum")
        cb1 <- CB_92_segments
        cb1@data <- left_join(cb1@data, d1, by = c("CBSEG_92" = "Segment"))
        DU_attainment <- cb1
        
        tm_shape(DU_attainment) +
            tm_fill("attainment_deficit",
                    title = paste("CHLsum ", input$Y2, "-", input$Y2 + 2, sep = ""),
                    id = "CBSEG_92", showNA = TRUE, colorNA = "grey",
                    alpha = 0.8, palette = rev(c(
                        "darkcyan", "cadetblue1", "cornsilk", "gold",
                        "darkorange2", "darkorange4"
                    )),
                    breaks = rev(c(0, -0.1, -5, -10, -25, -50, -100))
            ) +
            tm_layout(scale = 0.9) +
            tmap_options(check.and.fix = TRUE, show.warnings = FALSE) +
            tm_shape(CB_92_segments) +
            tm_borders(col = "black")
    })
    
    # GIF 1
    output$mygif1 <- renderImage(
        {
            list(
                src = "gif/MSN_Attainment_Deficit_Animation.gif",
                contentType = "image/gif",
                width = 600, height = 800
            )
        },
        deleteFile = FALSE
    )
    
    # GIF 2
    output$mygif2 <- renderImage(
        {
            list(
                src = "gif/OW_Attainment_Deficit_Animation.gif",
                contentType = "image/gif",
                width = 600, height = 800
            )
        },
        deleteFile = FALSE
    )
    
    # GIF 3
    output$mygif3 <- renderImage(
        {
            list(
                src = "gif/DW_Attainment_Deficit_Animation.gif",
                contentType = "image/gif",
                width = 600, height = 800
            )
        },
        deleteFile = FALSE
    )
    
    # GIF 4
    output$mygif4 <- renderImage(
        {
            list(
                src = "gif/DC_Attainment_Deficit_Animation.gif",
                contentType = "image/gif",
                width = 600, height = 800
            )
        },
        deleteFile = FALSE
    )
    
    # GIF 5
    output$mygif5 <- renderImage(
        {
            list(
                src = "gif/CHLspr_Attainment_Deficit_Animation.gif",
                contentType = "image/gif",
                width = 600, height = 800
            )
        },
        deleteFile = FALSE
    )
    
    # GIF 6
    output$mygif6 <- renderImage(
        {
            list(
                src = "gif/CHLsum_Attainment_Deficit_Animation.gif",
                contentType = "image/gif",
                width = 600, height = 800
            )
        },
        deleteFile = FALSE
    )
}

# Run the application
shinyApp(ui = ui, server = server)

# End
