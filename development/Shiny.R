#devtools::install_github("rstudio/SimulateDataPowerCaculation")
library(shiny)
library(shinysky)
library(dplyr)
library(readr)
library(readxl)
library(MASS)
require(GenOrd)
library(ggplot2)
library(stringr)
library(DT)
library(tools)
library(ggpubr)
library(rhandsontable)
library(doParallel)
library(SimulateDataPowerCaculation)
library(shinyIncubator)
library(shinyjs)

AKA<-DF



# UI
ui <- fluidPage(

  titlePanel(
    "Clinical Score data simulation plus Power calculation and Power plot",
    windowTitle = "Simulaiton and Power"),

  sidebarLayout(

    # Input(s)
    sidebarPanel(

      numericInput("seed",
                   "First, Set the seeds inorder to have reproductive data:",17),
      hr(),br(),
      numericInput("start",
                   "Then, Set the min value for each clinical score",0),
      br(),
      numericInput("end",
                   "Then, Set the max value for each clinical score",8),
      br(),
      numericInput("number.of.scores",
                   "Then, Set the number of clinical scores ",4),
      br(),
      numericInput("x",
                   "Then, Set the number of patients ",100),

      hr(),br(),

      HTML("Please, enter the desired the correlation
           matrix in the tab in order to proceed"),
      br(),


      actionButton("test","Submit all the parameters"),

      hr(),

      hr(),
      # Show data table
      conditionalPanel( condition = "input.t==true",
                        checkboxInput(inputId = "show_data",
                                      label = "Show Simulated data",
                                      value = FALSE),

      br(),

      HTML("Select filetype, then hit 'Download data'."),
      # Select filetype
      radioButtons(inputId = "filetype",
                   label = "Select filetype:",
                   choices = c("csv", "tsv"),
                   selected = "csv"),
      downloadButton("download_data", "Download data"))
      ,
      # Built with Shiny by RStudio
      br(),
      h5("Built with",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
         "by",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
         "."),width=4

      ),

    # Output(s)
    mainPanel(
      tabsetPanel(id = "tabspanel", type = "tabs",

                 tabPanel(title = "Correlation Matrix",
                          br(),
                          conditionalPanel( condition = "input.test>=1",
                                            wellPanel(hotable("matrixTable"),
                                    hr(),hotable("resultTable")),
                          hr(),
                          actionButton("t","Finish uploading Correlation Matrix")
                 ))
                   ,
                  tabPanel(title = "Simulated data",
                           br(),
                           DT::dataTableOutput(outputId = "Demotable")),
                 tabPanel(title = "Power Plot",
                          br(),
                          sidebarLayout(
                            sidebarPanel(width = 4,
                                         wellPanel(
                              numericInput("from",
                                           "Set the effect size from:",0.1),
                              numericInput("to",
                                           "Set the effect size to:",0.3),
                              numericInput("by",
                                           "By:",0.01),
                              numericInput("sd",
                                           "Standard Deviation:",5),
                              numericInput("iterations",
                                           "Number of iterations",1000)
                            ),
                            hr(),
                            actionButton("setting","Finish"))
                            ,

                            mainPanel(


                              conditionalPanel( condition = "input.setting>=1",
                                                plotOutput("powerPlot", width = "100%", height = "400px", click = NULL,
                                                           dblclick = NULL, hover = NULL, hoverDelay = NULL,
                                                           hoverDelayType = NULL, brush = NULL, clickId = NULL, hoverId = NULL,
                                                           inline = FALSE),
                                                tableOutput("powerTable")
                              )
                            )
                          )



                          )



  )
)))

# Server
server <- function(input, output, session) {

  # set.seed(input$seed)

  session$onSessionEnded(function() {
    stopApp()
  })

   mm<-reactive({
    m<-matrix(rep(0.1,input$number.of.scores**2),input$number.of.scores,input$number.of.scores)
     diag(m)<-as.integer(1)
     data.matrix(m)
      })

   core<-reactive({
     detectCores()
   })

  plotData_5<-reactive({

    require(input$from,input$to,input$by,input$sd,input$iterations)

    withProgress(message = 'Calculating power for plot', value = 0, {

      plotData(input$from, input$to, input$by, result(), input$sd, input$iterations, core(),input$seed)
})
  })

   observeEvent(input$setting, {
     if (input$setting) {

       withProgress(message = 'Making plot and the table', value = 0, {
   output$powerPlot <- renderPlot({
     ggplot(plotData_5()) +
       geom_line(aes(x=beta, y=pcaPower,color='PCA'))+
       geom_line(aes(x=beta, y=spcaPower,color='SPCA'))+
       labs(y="Power",x="mu")
                                   })

   output$powerTable<-DT::renderDataTable({
     DT::datatable(data = plotData_5(),
                   options = list(pageLength = 10),
                   rownames = TRUE)

   })
       })
                     }
   })

   A<-reactive({mm()})# init - input matrix A
   output$matrixTable <- renderHotable({data.frame(A())}, readOnly = FALSE)

   R<- reactive({mm()})# init - result matrix R
   output$resultTable <- renderHotable({data.frame(R())}, readOnly = TRUE)

   observe({  # process matrix
     df <- hot.to.df(input$matrixTable)
     if(!is.null(df)) {    # ensure data frame from table exists
       B = data.matrix(df) # ensure its numeric
       R = B             # some matrix operation
       output$resultTable <- renderHotable({data.frame(R)})
     }
   })
   df<- reactive({

     matrix(as.double(data.matrix(hot.to.df(input$resultTable))),
            input$number.of.scores,input$number.of.scores)

   })
   output$table.output <- renderTable({
     data.matrix(input$mx)
})

  result<-reactive({
      req(input$resultTable,input$start,input$end,
          input$number.of.scores,input$x)

    data.frame(Generate.data(input$start,input$end,
                        input$number.of.scores,df(),input$x))
  })



  output$Demotable <- DT::renderDataTable({
    DT::datatable(data = result(),
                  options = list(pageLength = 10),
                  rownames = TRUE)






  })



  # Display data table tab only if show_data is checked
  observeEvent(input$show_data, {
    if(input$show_data){
      showTab(inputId = "tabspanel", target = "Simulated data", select = TRUE)
    } else {
      hideTab(inputId = "tabspanel", target = "Simulated data")
    }
  })
   observeEvent(input$t, {
     if(input$t>0){
       showTab(inputId = "tabspanel", target = "Power Plot", select = TRUE)
     } else {
       hideTab(inputId = "tabspanel", target = "Power Plot")
     }
   })

  # Download file
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("Simulated Clinical Score data.", input$filetype)
    },
    content = function(file) {
      if(input$filetype == "csv"){
        write_csv(result(), file)
      }
      if(input$filetype == "tsv"){
        write_tsv(result(), file)
      }
    }
  )


}




# Create a Shiny app object
shinyApp(ui = ui, server = server)
