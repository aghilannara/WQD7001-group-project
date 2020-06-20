#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(dplyr)
library(sunburstR)
library(d3r)
library(tidyr)
library(stringr)
library(treemap)
library(DT)
library(d3treeR)
library(ggplot2)
library(plotly)

# Define UI for application that draws a histogram
ui <- dashboardPage(

    # Application title
    dashboardHeader(title="awan-group"),

    # Sidebar with a slider input for number of bins 
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Documentation", tabName = "doc", icon=icon("info-circle")),
            menuItem("Group Members", tabName = "member", icon=icon("users"))
            )
        ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard",
                fluidRow(box(titlePanel(h1("Economic Impact Assessment - Input-Output Model",align="center")),
                             width = 12,
                             status = "primary",
                             background = "light-blue"),
                ),
                tags$head(
                    tags$style(HTML(".main-sidebar { font-size: 16px; }")) #change the font size to 20
                ),
                fluidRow(
                box(title = "1. Select Sector",
                    solidHeader = T,
                    status = "primary",
                    width = 4,
                    collapsible = T,
                    htmlOutput("sector_selector"),
                    htmlOutput("mult_sector_selector"),
                    htmlOutput("workers_affected"),
                    htmlOutput("period_of_impairment"),
                    actionButton("add","Add Info"),
                    actionButton("remove","Remove")),
                box(title = "2. Chosen Sectors",
                    solidHeader = T,
                    status = "info",
                    width = 8, 
                    collapsible = T,
                    DTOutput("initial_df"),
                    actionButton("calc","Calculate Impact"))
               ),
            fluidRow(
                valueBoxOutput("cnt_sector",width=3),
                valueBoxOutput("cnt_mult_sector",width=3),
                valueBoxOutput("cnt_workers",width=3),
                valueBoxOutput("cnt_avg_impairment",width=3),
                
            ),
            fluidRow(
                box(titlePanel(title="Aggregated Economic Impact"),
                    width = 12,
                    status = "info")
            ),
            fluidRow(
                valueBoxOutput("initial_impairment",width=4),
                valueBoxOutput("total_changes_output",width=4),
                valueBoxOutput("total_changes_valueAdded",width=4)
            ),
            fluidRow(
                box(title = "Sunburst",
                    status="info",
                    width = 6,
                    collapsible = T,
                    sunburstOutput("sb"),
                    ),
                box(title = "TreeMap",
                    status="info",
                    width = 6,
                    collapsible = T,
                    d3tree3Output("tm")
                    )
               ),
            fluidRow(
                box(titlePanel(title="Breakdown of Changes in Output"),
                    width = 12,
                    status = "info")
            ),
            fluidRow(
                valueBoxOutput("sum_output_direct",width=4),
                valueBoxOutput("sum_output_indirect",width=4),
                valueBoxOutput("sum_output_induced",width=4)
            ),
            fluidRow(
                valueBoxOutput("sum_initial", width = 6),
                valueBoxOutput("sum_output_total", width = 6)
                ),
            fluidRow(
                box(title="Sector Categories Impact Breakdown",
                    solidHeader = T,
                    div(plotlyOutput("bc_output_sector"))),
                box(title="Multiplier Categories Impact Breakdown",
                    solidHeader = T,
                    div(plotlyOutput("bc_output_mult")))
            ),
            fluidRow(
                box(titlePanel(title="Breakdown of Changes in Value-Added"),
                    width = 12,
                    status = "info")
            ),
            fluidRow(
                valueBoxOutput("sum_va_direct",width=4),
                valueBoxOutput("sum_va_indirect",width=4),
                valueBoxOutput("sum_va_induced",width=4)
            ),
            fluidRow(
                valueBoxOutput("sum_va_initial", width = 6),
                valueBoxOutput("sum_va_total", width = 6)
                ),
            fluidRow(
                box(title="Sector Categories Impact Breakdown",
                    solidHeader = T,
                    div(plotlyOutput("bc_va_sector"))),
                box(title="Multiplier Categories Impact Breakdown",
                    solidHeader = T,
                    div(plotlyOutput("bc_va_mult")))
            ),
            
            ),
            tabItem(tabName = "doc",
                    fluidRow(
                        box(headerPanel(h1("Documentation",align="center")),
                            width = 12,
                            status = "info",
                            background = "aqua")),
                    fluidRow(box(includeMarkdown("document.Rmd"),
                                 width = 12,
                                 status= "info"))
            ),
            tabItem(tabName = "member",
                    fluidRow(
                        box(headerPanel(h1("Group Member (Group I)",align="center")),
                            width = 12,
                            status = "info",
                            background = "aqua")),
                    fluidRow(box(titlePanel("Aghilan Narayanan (17218898)"),
                                 width = 6,
                                 status= "info"),
                             box(titlePanel("Nurarlisa Sulong (17220304)"),
                                 width = 6,
                                 status= "info")),
                    fluidRow(box(titlePanel("Najlaa Ramli (17219402)"),
                                 width = 6,
                                 status= "info"),
                             box(titlePanel("Nurul Aswani (17218967)"),
                                 width = 6,
                                 status= "info")),
                    
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Taken and modified from working.R
    # --------------------------------------------------------------------------------------
    gdp_df <- read.csv("gdp-per-worker.csv", header=T)
    employment_df <- read.csv("employment.csv", header=T)
    output_mult_df <- read.csv("output-multiplier.csv", header=T)
    value_added_mult_df <- read.csv("value-added-multiplier.csv", header=T)
    
    names(gdp_df) <- c("sectors", "units", "y2015", "y2016", "y2017", "y2018", "y2019")
    names(employment_df) <- c("sector", "units", "y2015","y2016","y2017","y2018", "y2019")
    
    # Removing whitespace from the string
    gdp_df <- gdp_df %>%
        mutate(sectors = str_trim(sectors, side = "both"))
    employment_df <- employment_df %>%
        mutate(sector = str_trim(sector, side = "both"))
    output_mult_df <- output_mult_df %>%
        mutate(Sector = str_trim(Sector, side = "both"))
    value_added_mult_df <- value_added_mult_df %>%
        mutate(Sector = str_trim(Sector, side = "both"))
    
    sector_categories <- sort(unique(gdp_df$sectors))
    multiplier_categories <- sort(unique(output_mult_df$Sector))
    
    
    output$sector_selector <- renderUI({
        selectizeInput("sector", "General Sector", choices=sector_categories)
    })
    output$mult_sector_selector <- renderUI({
        selectizeInput("mult_sector", "Multiplier Sector", choices=multiplier_categories)
    })
    
    output$workers_affected <- renderUI({
        numericInput("workers_affected","Estimated number of workers affected", value=0, min=0, max=1000000, step=1000)
    })
    output$period_of_impairment <- renderUI({
        numericInput("period_impairment","Period of productivity impairment (in days)", value=0, min=0, max=365, step=1)
    })
    
    initial_df <- reactiveValues()
    initial_df$df <- data.frame("sector"=character(), "mult_sector"=character(), "workers_affected"=numeric(), "periods_of_impairment"=numeric(), stringsAsFactors = F)
    observeEvent(
        input$remove,
        {
            isolate(initial_df$df <- initial_df$df[-nrow(initial_df$df),])
        })
    observeEvent(
        input$add,
        {
            isolate(initial_df$df[nrow(initial_df$df) +1, ]<- c(input$sector,
                                                                input$mult_sector,
                                                                input$workers_affected,
                                                                input$period_impairment))
            
            
        }
        
        
        
    )
    output$initial_df <- renderDataTable(initial_df$df, options = list(pageLength = 5))
    
    output$cnt_sector <- renderValueBox({
        valueBox(lengths(unique(initial_df$df['sector'])),"Sectors",color="blue")
    })
    output$cnt_mult_sector <- renderValueBox({
        valueBox(lengths(unique(initial_df$df['mult_sector'])),"Multiplier Sectors",color="blue")
    })
    output$cnt_workers <- renderValueBox({
        valueBox(format(sum(as.numeric(initial_df$df[["workers_affected"]])),big.mark = ",",scientific = F),"Estimated No. Affected Workers", color="blue")
    })
    output$cnt_avg_impairment <- renderValueBox({
        rounded = round(mean(as.numeric(initial_df$df[["periods_of_impairment"]])),1)
        valueBox(paste0(rounded," days"), "Average Period of Impairment", color="blue")
    })
    
    observeEvent(input$calc,{
        df <- initial_df$df
        
        # Revised initial dataframe
        
        df <- df %>% left_join(select(gdp_df,"y2019","sectors"), by=c("sector"="sectors")) %>%
            rename(gdp_per_worker = y2019)
        df <- df %>% left_join(select(employment_df,"y2019","sector"), by=c("sector"="sector")) %>%
            rename(employment = y2019)
        df <- df %>% left_join(select(output_mult_df,"Direct.impact","Sector"),by=c("mult_sector"="Sector")) %>%
            rename(mult_direct = Direct.impact)
        df <- df %>% left_join(select(output_mult_df,"Indirect.impact","Sector"),by=c("mult_sector"="Sector")) %>%
            rename(mult_indirect = Indirect.impact)
        df <- df %>% left_join(select(output_mult_df,"Induced.impact","Sector"),by=c("mult_sector"="Sector")) %>%
            rename(mult_induced = Induced.impact)
        df <- df %>% left_join(select(value_added_mult_df,"Direct.impact","Sector"),by=c("mult_sector"="Sector")) %>%
            rename(va_mult_direct = Direct.impact)
        df <- df %>% left_join(select(value_added_mult_df,"Indirect.impact","Sector"),by=c("mult_sector"="Sector")) %>%
            rename(va_mult_indirect = Indirect.impact)
        df <- df %>% left_join(select(value_added_mult_df,"Induced.impact","Sector"),by=c("mult_sector"="Sector")) %>%
            rename(va_mult_induced = Induced.impact)
        
        # Calculate the outputs and value added
        
        df <- df %>%
            mutate(initial_calc = as.numeric(gdp_per_worker) * as.numeric(workers_affected) * (as.numeric(periods_of_impairment) / 365))
        
        # Build whole dataframe
        
        df <- df %>%
            mutate(direct_impact = mult_direct * initial_calc) %>%
            mutate(indirect_impact = mult_indirect * initial_calc) %>%
            mutate(induced_impact = mult_induced * initial_calc) %>%
            mutate(va_direct_impact = va_mult_direct * initial_calc) %>%
            mutate(va_indirect_impact = va_mult_indirect * initial_calc) %>%
            mutate(va_induced_impact = va_mult_induced * initial_calc)
        
        # Tidying up and add a column to identify "output" changes vs "value_added" changes
        output_df <- df %>% 
            select(sector, mult_sector, direct_impact, indirect_impact, induced_impact) %>%
            gather(calculated,value, 3:5 ) %>%
            mutate(typeOfChanges="output")
        
        output_df <- output_df[, c(1,2,5,3,4)]
        output_df <- output_df[order(output_df$sector),]  
        
        va_df <- df %>% 
            select(sector, mult_sector, va_direct_impact, va_indirect_impact, va_induced_impact) %>%
            gather(calculated,value, 3:5 ) %>%
            mutate(typeOfChanges="value_added")
        
        va_df <- va_df[, c(1,2,5,3,4)]
        va_df <- va_df[order(va_df$sector),]  
        
        
        # Building dataframe to be output in sunburst chart
        # Need to have 2 columns only, "Agri - Paddy - .... - .... " , value
        
        combined_df <- rbind(output_df, va_df)
        combined_df_cls <- combined_df %>%
            mutate_all(funs(str_replace(.,"-",".")))
        df_united <- unite(combined_df_cls, "V1", c(sector, mult_sector, typeOfChanges, calculated),remove=FALSE, sep=" - ")
        sb_output <- df_united %>%
            select(V1, value)
        
        # breakdown by category and subcategory with direct, indirect, induced using sunburst
        sb <- sunburst(sb_output) 
        
        # viz using treemap
        tm <- treemap(combined_df,
                      index=c("sector","mult_sector","typeOfChanges","calculated"),
                      vSize="value",
                      vColor="value")
        
        # viz using bar chart
        
        # Select important columns
        calc_df <- df %>%
            select("sector","mult_sector","workers_affected","periods_of_impairment","employment","initial_calc","direct_impact",
                   "indirect_impact","induced_impact","va_direct_impact","va_indirect_impact","va_induced_impact")
        
        
        # Changes in Output tab
        sum_initial_impairment <- sum(calc_df$initial_calc)
        sum_output_direct <- sum(calc_df$direct_impact)
        sum_output_indirect <- sum(calc_df$indirect_impact)
        sum_output_induced <- sum(calc_df$induced_impact)
        total_changes_in_output <- sum_output_direct + sum_output_indirect + sum_output_induced
        
        
        # Changes in Value Added tab
        sum_va_direct <- sum(calc_df$va_direct_impact)
        sum_va_indirect <- sum(calc_df$va_indirect_impact)
        sum_va_induced <- sum(calc_df$va_induced_impact)
        total_changes_in_valueAdd <- sum_va_direct + sum_va_indirect + sum_va_induced
        
        output$sb <- renderSunburst({
            sunburst(sb_output, legend = FALSE)
        })
        
        output$tm <- renderD3tree3(d3tree3(tm,rootname = "General"))
        
        output$sum_initial <- renderValueBox({
            rounded = format(round((as.numeric(sum_initial_impairment)),2),big.mark = ",", nsmall = 2)
            valueBox(paste0("RM (",rounded, ")"), "Initial Changes", color="maroon")
        })
        output$sum_output_direct <- renderValueBox({
            rounded = format(round((as.numeric(sum_output_direct)),2),big.mark = ",", nsmall = 2)
            valueBox(paste0("RM (",rounded, ")"), "Direct Impact to Output", color="blue")
        })
        output$sum_output_indirect <- renderValueBox({
            rounded = format(round((as.numeric(sum_output_indirect)),2),big.mark = ",", nsmall = 2)
            valueBox(paste0("RM (",rounded, ")"), "Indirect Impact to Output", color="blue")
        })
        output$sum_output_induced <- renderValueBox({
            rounded = format(round((as.numeric(sum_output_induced)),2),big.mark = ",", nsmall = 2)
            valueBox(paste0("RM (",rounded, ")"), "Induced Impact to Output", color="blue")
        })
        output$sum_output_total <- renderValueBox({
            rounded = format(round((as.numeric(total_changes_in_output)),2),big.mark = ",", nsmall = 2)
            valueBox(paste0("RM (",rounded, ")"), "Total Impact to Output (Direct + Indirect + Induced)", color="navy")
        })
        
        output_df_summary <- output_df %>%
            group_by(sector, calculated) %>%
            summarise(value=sum(value))
        
        output$bc_output_sector <- renderPlotly({
            ggplot(data=output_df_summary,aes(x=sector,y=value,fill=calculated)) + 
                geom_bar(stat="identity", position=position_dodge()) +
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank())
        })
        output$bc_output_mult <- renderPlotly({
            ggplot(data=output_df,aes(x=mult_sector,y=value,fill=calculated)) + 
                geom_bar(stat="identity", position=position_dodge()) +
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank())
        })
        #print(output_df)
        
        output$sum_va_initial <- renderValueBox({
            rounded = format(round((as.numeric(sum_initial_impairment)),2),big.mark = ",", nsmall = 2)
            valueBox(paste0("RM (",rounded, ")"), "Initial Changes", color="maroon")
        })
        output$sum_va_direct <- renderValueBox({
            rounded = format(round((as.numeric(sum_va_direct)),2),big.mark = ",", nsmall = 2)
            valueBox(paste0("RM (",rounded, ")"), "Direct Value-Added Impact", color="blue")
        })
        output$sum_va_indirect <- renderValueBox({
            rounded = format(round((as.numeric(sum_va_indirect)),2),big.mark = ",", nsmall = 2)
            valueBox(paste0("RM (",rounded, ")"), "Indirect Value-Added Impact", color="blue")
        })
        output$sum_va_induced <- renderValueBox({
            rounded = format(round((as.numeric(sum_va_induced)),2),big.mark = ",", nsmall = 2)
            valueBox(paste0("RM (",rounded, ")"), "Induced Value-Added Impact", color="blue")
        })
        output$sum_va_total <- renderValueBox({
            rounded = format(round((as.numeric(total_changes_in_valueAdd)),2),big.mark = ",", nsmall = 2)
            valueBox(paste0("RM (",rounded, ")"), "Total Value-Added Impact (Direct + Indirect + Induced)", color="navy")
        })
        
        va_df_summary <- va_df %>%
            group_by(sector, calculated) %>%
            summarise(value=sum(value))
        
        output$bc_va_sector <- renderPlotly({
            ggplot(data=va_df_summary,aes(x=sector,y=value,fill=calculated)) + 
                geom_bar(stat="identity", position=position_dodge()) +
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank())
        })
        output$bc_va_mult <- renderPlotly({
            ggplot(data=va_df,aes(x=mult_sector,y=value,fill=calculated)) + 
                geom_bar(stat="identity", position=position_dodge()) +
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank())
        })
        
        output$initial_impairment <- renderValueBox({
            rounded = format(round((as.numeric(sum_initial_impairment)),2),big.mark = ",", nsmall = 2)
            valueBox(paste0("RM (",rounded, ")"), "Initial Changes (Worker Productivity Impairment)", color="maroon")
        })
        output$total_changes_output <- renderValueBox({
            rounded = format(round((as.numeric(total_changes_in_output)),2),big.mark = ",", nsmall = 2)
            valueBox(paste0("RM (",rounded, ")"), "Changes in Output (Direct + Indirect + Induced)", color="navy")
        })
        output$total_changes_valueAdded <- renderValueBox({
            rounded = format(round((as.numeric(total_changes_in_valueAdd)),2),big.mark = ",", nsmall = 2)
            valueBox(paste0("RM (",rounded, ")"), "Changes in Value-Added (Direct + Indirect + Induced)", color="navy")
        })
    })
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
