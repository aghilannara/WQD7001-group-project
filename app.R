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

# Define UI for application that draws a histogram
ui <- dashboardPage(

    # Application title
    dashboardHeader(title="awan-group"),

    # Sidebar with a slider input for number of bins 
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Help", tabName = "help", icon=icon("info-circle"))
            )
        ),
    dashboardBody(
        tags$head(
            tags$style(HTML(".main-sidebar { font-size: 16px; }")) #change the font size to 20
        ),
        tabItems(
            tabItem(tabName = "dashboard",
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
               )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Taken from working.R
    # --------------------------------------------------------------------------------------
    gdp_df <- read.csv("gdp-per-worker.csv", header=T)
    employment_df <- read.csv("employment.csv", header=T)
    output_mult_df <- read.csv("output-multiplier.csv", header=T)
    value_added_mult_df <- read.csv("value-added-multiplier.csv", header=T)
    
    names(gdp_df) <- c("sectors", "units", "y2015", "y2016", "y2017", "y2018", "y2019")
    names(employment_df) <- c("sector", "units", "y2015","y2016","y2017","y2018", "y2019")
    
    sector_categories <- levels(gdp_df$sectors)
    multiplier_categories <- levels(output_mult_df$Sector)
    
    
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
        valueBox(sum(as.numeric(initial_df$df[["workers_affected"]])),"Estimated No. Affected Workers", color="blue")
    })
    output$cnt_avg_impairment <- renderValueBox({
        rounded = round(mean(as.numeric(initial_df$df[["periods_of_impairment"]])),1)
        valueBox(paste0(rounded," days"), "Average Period of Impairment", color="blue")
    })
    
    observeEvent(input$calc,{
        df <- initial_df$df
        #print(df)
        # Get initial dataframe
        
        df <- df %>% 
            mutate(gdp_per_worker = pull(subset(gdp_df, sectors %in% df[["sector"]], select = c('y2019')))) %>%
            mutate(employment = pull(subset(employment_df, sector %in% df[["sector"]], select = c("y2019")))*1000) %>%
            mutate(mult_direct = pull(subset(output_mult_df, Sector %in% df[["mult_sector"]], select= c("Direct.impact")))) %>%
            mutate(mult_indirect = pull(subset(output_mult_df, Sector %in% df[["mult_sector"]], select= c("Indirect.impact")))) %>%
            mutate(mult_induced = pull(subset(output_mult_df, Sector %in% df[["mult_sector"]], select= c("Induced.impact")))) %>%
            mutate(va_mult_direct = pull(subset(value_added_mult_df, Sector %in% df[["mult_sector"]], select= c("Direct.impact")))) %>%
            mutate(va_mult_indirect = pull(subset(value_added_mult_df, Sector %in% df[["mult_sector"]], select= c("Indirect.impact")))) %>%
            mutate(va_mult_induced = pull(subset(value_added_mult_df, Sector %in% df[["mult_sector"]], select= c("Induced.impact"))))
        
        
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
        
        output$tm <- renderD3tree3(d3tree3(tm))
    })
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
