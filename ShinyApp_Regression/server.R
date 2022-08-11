
server <- function(input, output) {
  
  output$summary <- renderPrint({
    fit <- lm(marketing[,input$outcome] ~ marketing[,input$indepvar])
    names(fit$coefficients) <- c("Intercept", input$var2)
    summary(fit)
  })
  
  output$tbl = DT::renderDataTable({
    DT::datatable(marketing, options = list(lengthChange = FALSE))
  })
  
  
  output$scatterplot <- renderPlot({
    plot(marketing[,input$indepvar], marketing[,input$outcome], main="Advertising Medias Scatterplot",
         xlab=input$indepvar, ylab=input$outcome, pch=19)
    abline(lm(marketing[,input$outcome] ~ marketing[,input$indepvar]), col="red")
    lines(lowess(marketing[,input$indepvar],marketing[,input$outcome]), col="blue")
  }, height=400)
  
  
  output$distribution1 <- renderPlot({
    hist(marketing[,input$outcome], main="Dependent Variable", xlab=input$outcome,
         col = 3)
  }, height=300, width=300)
  
  output$distribution2 <- renderPlot({
    hist(marketing[,input$indepvar], main="Independent Variable", xlab=input$indepvar,
         col = 5)
  }, height=300, width=300)
}
