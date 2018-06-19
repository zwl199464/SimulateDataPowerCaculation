shiny::runApp(list(
  ui=pageWithSidebar(
    headerPanel('Simple matrixInput')
    ,
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
      ,
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   'Comma'),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   'Double Quote')
    )
    ,
    mainPanel(

      tableOutput(outputId = 'table.output')
    ))
  ,
  server=function(input, output){
    output$table.output <- renderTable({

      inFile <- input$file1

      if (is.null(inFile))
        return(NULL)

      tbl <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
      return(tbl)
    })
  }
))
