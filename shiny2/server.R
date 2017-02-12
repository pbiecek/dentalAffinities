library("shiny")
library("dentalAffinities")
library("MASS")

function(input, output) {
  # get data
  dataInput <- reactive({
    if (input$button) {
      load("df_sample2.rda")
    } else {
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      df <- loadData(inFile)
      attr(df, which = "name") = inFile[[1]]
    }
    # sex handling
    if (input$sex_handling == "MALE") {
      df <- df[which(df[,3] == "M"),]
    }
    if (input$sex_handling == "FEMALE") {
      df <- df[which(df[,3] == "F"),]
    }
    # side handling
    if (input$init_trait == "RIGHT") {
      df <- df[,c(1:3,seq(4, ncol(df), 2))]
    }
    if (input$init_trait == "LEFT") {
      df <- df[,c(1:3,seq(5, ncol(df), 2))]
    }
    if (input$init_trait == "MIN") {
      for (i in seq(5, ncol(df), 2)) {
        df[,2 + (i-1)/2] <- pmin(df[,i], df[,i-1], na.rm = TRUE)
      }
      df <- df[,1:(2 + (i-1)/2)]
    }
    if (input$init_trait == "MAX") {
      for (i in seq(5, ncol(df), 2)) {
        df[,2 + (i-1)/2] <- pmax(df[,i], df[,i-1], na.rm = TRUE)
      }
      df <- df[,1:(2 + (i-1)/2)]
    }
    if (input$init_trait == "AVG") {
      for (i in seq(5, ncol(df), 2)) {
        df[,2 + (i-1)/2] <- rowMeans(df[,i+c(-1,0)], na.rm = TRUE)
      }
      df <- df[,1:(2 + (i-1)/2)]
    }

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

  output$distSummary <- renderPrint({
    di <- dataInput()
    if (is.null(di)) {
      "Upload data"
    } else {
      mat <- getDist()$MMDMatrix
      print(round(mat, 2))
    }
  })
  output$sdSummary <- renderPrint({
    di <- dataInput()
    if (is.null(di)) {
      "Upload data"
    } else {
      mat <- getDist()$SDMatrix
      print(round(mat, 2))
    }
  })
  output$signifSummary <- renderPrint({
    di <- dataInput()
    if (is.null(di)) {
      "Upload data"
    } else {
      mat <- getDist()$SigMatrix
      print(round(mat, 5))
    }
  })

}
