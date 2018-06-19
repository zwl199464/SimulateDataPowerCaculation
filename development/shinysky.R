devtools::install_github("AnalytixWare/ShinySky")
library(shinysky)
shinyApp(
  ui     = shinyUI (wellPanel(hotable("matrixTable"),hotable("resultTable"))),

  server = shinyServer (function(input, output) {
    A = matrix(c(1:6), nrow=2) # init - input matrix A
    output$matrixTable <- renderHotable({data.frame(A)}, readOnly = FALSE)

    R = matrix(rep(0,6), nrow=2) # init - result matrix R
    output$resultTable <- renderHotable({data.frame(R)}, readOnly = TRUE)

    observe({  # process matrix
      df <- hot.to.df(input$matrixTable)
      if(!is.null(df)) {    # ensure data frame from table exists
        B = data.matrix(df) # ensure its numeric
        R = B^2             # some matrix operation
        output$resultTable <- renderHotable({data.frame(R)})
      }
    }) # end of observe
  }) # end of server
)
