library("shiny")
library("dentalAffinities")
library("MASS")

function(input, output) {
  # get data
  dataInput <- reactive({
    if (input$button) {
      load("df_sample2.rda")
      return(df)
    }
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    df <- loadData(inFile)
    attr(df, which = "name") = inFile[[1]]
    df
  })

  # get dist
  getDist <- reactive({
    res <- NULL

    df <- dataInput()

    selected <- strsplit(input$method_sel, split="_")[[1]]
    if (selected[1] == "MMD") {
      theta <- dentalAffinities::theta_Anscombe
      if (selected[2] == "FRE")
        theta <- dentalAffinities::theta_Freeman

      thetadiff <- dentalAffinities::thetadiff_uncorrected
      if (selected[3] == "FRE")
        thetadiff <- dentalAffinities::thetadiff_Freeman
      if (selected[3] == "GRE")
        thetadiff <- dentalAffinities::thetadiff_Grewal

      tmp <- dentalAffinities::get_Mn_Mp(df)
      res <- dentalAffinities::calculateMMD(data.frame(tmp$Mn), as.data.frame(tmp$Mp), thetadiff, theta)
    }

    res
  })

  # table
  output$ggMDS <- renderPlot({
    di <- dataInput()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    mat <- getDist()
    dentalAffinities::getMDS(mat$MMDMatrix)
  })

  # table
  output$ggClust <- renderPlot({
    di <- dataInput()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    mat <- getDist()
    dentalAffinities::getClust(mat$MMDMatrix)
  })

  output$textSummary <- renderPrint({
    di <- dataInput()
    if (is.null(di)) {
      "Upload data"
    } else {
      mat <- getDist()$MMDMatrix
      print(round(mat, 2))
    }
  })

}
