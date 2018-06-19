rm(list=ls())

library(shiny)

doshiny <- function() {
  app=shinyApp(
    ui = fluidPage(
      textInput("textfield", "Insert some text", value = "SomeText")
    ),
    server = function(input, output, session) {
      session$onSessionEnded(function() {
        stopApp()
      })
    }
  )
  runApp(app)
}

openshiny <- function() {
  doshiny()
  print("Finished.")
}

openshiny()
