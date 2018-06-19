

#' A shiny app to edit correlation matrix
#'
#' @param DF the original dataset that need to put in in order to invoke the app
#' @param outdir the file directory
#' @param outfilename the output file name
#' @return a dataframe that is the eidited correlation matrix as dataframe format.
#' @import shiny
#' @import rhandsontable
#' @export
#'
#' @examples
#' correlation.matrix(DF)
#'
correlation.matrix <- function(DF, outdir=getwd(), outfilename="table"){

  doshiny <- function() {

  ui <- shinyUI(fluidPage(

    titlePanel("Edit and save the correlation matrix"),
    sidebarLayout(
      sidebarPanel(
        helpText("Shiny app based on an example given in the rhandsontable package.",
                 "Right-click on the table to delete/insert rows.",
                 "Double-click on a cell to edit"),

        wellPanel(
          h3("Table options"),
          radioButtons("useType", "Use Data Types", c("TRUE", "FALSE"))
        ),
        br(),

        wellPanel(
          h3("Save the correlation data then click the close window button(Mandatory),
             Please only enter valid correlation matrix in order to proceed"),
          actionButton("save", "Save Correlation Matrix")
        ),
        wellPanel(
        tags$button(
          id = 'close',
          type = "button",
          class = "btn action-button",
          onclick = "setTimeout(function(){window.close();},500);",  # close browser
          "Close window"
        ))

      ),

      mainPanel(

        rHandsontableOutput("hot")

      )
    )
  ))

  server <- shinyServer(function(input, output, session) {


    values <- reactiveValues()

    ## Handsontable
    observe({
      if (!is.null(input$hot)) {
        DF = hot_to_r(input$hot)
      } else {
        if (is.null(values[["DF"]]))
          DF <- DF
        else
          DF <- values[["DF"]]
      }
      values[["DF"]] <- DF
    })

    output$hot <- renderRHandsontable({
      DF <- values[["DF"]]
      if (!is.null(DF))
        rhandsontable(DF, useTypes = as.logical(input$useType)
                      ,version =3, stretchH = "all")
    })

    ## Save
    observeEvent(input$save, {
      finalDF <- isolate(values[["DF"]])
      saveRDS(finalDF, file=file.path(outdir, sprintf("%s.rds", outfilename)))
    })
    observe({
      if (input$close > 0) stopApp()                             # stop shiny
    })

  })

  ## run app
  runApp(list(ui=ui, server=server))
  }
  doshiny()
  # finalDF<-doshiny()
  # return(finalDF)
  print("Finished.")
  finalDF<-readRDS(file=file.path(outdir, sprintf("%s.rds", outfilename)))
  return(finalDF)
}


