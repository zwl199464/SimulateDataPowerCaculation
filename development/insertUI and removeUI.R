observeEvent(input$insertBtn, {
  insertUI(
    selector = '#placeholder',
    ui = radioButtons('rd6', 'Choose one', c('a', 'b', 'c')
    )
})

  observeEvent(input$removeBtn, {
    removeUI(
      selector = '#rd5'
    )
  })
