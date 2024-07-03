library(shiny)
library(bs4Dash)
library(reactable)
library(openalexR)
library(shinyalert)
library(reactablefmtr)
library(shinycssloaders)

options(spinner.color.background = "#FFFFFF",
        spinner.color = "#37a1e7",
        spinner.size = 2)


ui <- dashboardPage(scrollToTop = T,
                    title = "OpenAlex4NodeXL",dark = NULL,help = NULL,
  dashboardHeader(h2(strong("OpenAlex For NodeXL"),style = "margin-left:380px;color:#1b7ccc;")),
  dashboardSidebar(id = "side",
                   actionButton("info",strong("Info"),style = "margin-left:75px;color:black;"),hr(),
                   h6(strong("Select Search Window"),style = "margin-left:35px;color:black;"),
                   dateInput("start",h6(strong("From"),style = "margin-left:80px;color:black;")),
                   dateInput("end",h6(strong("To"),style = "margin-left:90px;color:black;")),
                   textInput("text",h6(strong("Keyword Search"),style = "margin-left:40px;color:black;"),placeholder = "Enter keyword(s) here"),
                   actionButton("query",strong("Search"),icon = icon("search"),style = "margin-left:60px;color:black;"),hr(),
                   tags$a(href = "https://github.com/Ifeanyi55/OpenAlex4NodeXL", target = "_blank", h4(strong("GitHub"),icon("github", lib = "font-awesome"),style = "text-align:left;")),
                   minified = F),
  dashboardBody(
    includeCSS("styles.css"),
    includeScript("code.js"),
    fluidRow(
      br(),style = "text-align:center;"),h4(strong("Author To Publication Network Data")),
    withSpinner(reactableOutput("table",width = "100%",height = 400),type = 1),br(),
    downloadButton("download",strong("Download CSV"),icon = icon("download"),style = "margin-left:1px;")
    )
)


server <- function(input,output,session){
  
# read OpenAlex4NodeXL script
source("OpenAlex4NodeXL.R")
  
start_date <- reactive({input$start})

end_date <- reactive({input$end})

keywords <- reactive({input$text})

# make OpenAlex4NodeXL() a reactive function
OpenAlex4NodeXL_reactive <- reactive({
  OpenAlex4NodeXL(
    keywords = c(unlist(strsplit(keywords(),split = ","))),
    pub_start_date =  start_date(),
    pub_end_date =  end_date())
})

run_OpenAlex4NodeXL <- eventReactive(input$query,{
  OpenAlex4NodeXL_reactive()
})

output$table <- renderReactable({
  tryCatch(
    {
      reactable(run_OpenAlex4NodeXL(),
                theme = reactableTheme(
                  borderColor = "#37a1e7",
                  borderWidth = 3
                ),
                compact = T,
                filterable = T,
                bordered = T)
    },
    error = function(error){
      safeError(error = "Please make sure to enter dates in chronological order")
    }
  )
})

# activate download handler
output$download <- downloadHandler(
  filename = function(){
    paste("Author2Pub",".csv",sep = "")
  },
  content = function(file){
    write.csv(run_OpenAlex4NodeXL(),file,row.names = F)
  }
)
  
observeEvent(input$info,{
  shinyalert(title = "Info",
             text = "Please note that the bigger the search window, the more data is collected. The more data is collected, the longer the runtime and the longer it takes to commence file download.",
             confirmButtonCol = "#37a1e7",
             closeOnEsc = T,
             closeOnClickOutside = T,
             showConfirmButton = T,
             timer = 10000,
             animation = "slide-from-top"
             )
})
}

# Run the application 
shinyApp(ui = ui, server = server)
