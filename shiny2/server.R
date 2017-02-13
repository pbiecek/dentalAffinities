library("shiny")
library("dentalAffinities")
library("MASS")
library("openxlsx")
library("ggbiplot")

function(input, output) {
  # get data
  dataInput <- reactive({
    inFile <- input$file1
    if (input$button & is.null(inFile)) {
      load("df_sample2.rda")
    } else {
      if (is.null(inFile))
        return(NULL)
      df <- loadData(inFile$datapath)
      attr(df, which = "name") = inFile[[1]]
    }
    # binarisation
    if (input$binarisation == "USER") {
      THRESHOLD = df[1,]
      df <- df[-1,]
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
    # binarisation
    if (input$binarisation == "USER") {
      for (i in 4:ncol(df)) {
        df[,i] <- (df[,i] >= THRESHOLD[1,i]) + 0
      }
    }
    if (input$binarisation == "BALANCED") {
      # remove without site
      df <- df[!is.na(df[,2]),]
      for (i in 4:ncol(df)) {
        df[,i] <- (df[,i] >= median(df[,i], na.rm=TRUE)) + 0
      }
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
  output$ggCzekanowski <- renderPlot({
    di <- dataInput()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    mat <- getDist()
    dentalAffinities::getCzekanowski(mat$MMDMatrix)
  })
  output$ggPCA <- renderPlot({
    di <- dataInput()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    dentalAffinities::getPCA(di)
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

  output$downloadFigures <- downloadHandler(
    filename = function() {
      paste('dentalAffinities_', Sys.Date(), '.pdf', sep='')
    },
    content = function(con) {
      di <- dataInput()
      if (!is.null(di)) {
        pdf(file = con, width = 10, height = 10)
        mat <- getDist()

        print(dentalAffinities::getClust(mat$MMDMatrix))
        print(dentalAffinities::getMDS(mat$MMDMatrix))
        print(dentalAffinities::getCzekanowski(mat$MMDMatrix))
        print(dentalAffinities::getPCA(di))

        dev.off()
      }
    }
  )

  output$downloadData <- downloadHandler(
    filename = function() {
      paste('dentalAffinities_', Sys.Date(), '.xlsx', sep='')
    },
    content = function(con) {
      di <- dataInput()
      if (is.null(di)) {
        mat <- data.frame("Upload the data first")
      } else {
        mat <- getDist()
        write.xlsx(mat, file = con, row.names = TRUE)
      }
    }
  )

}
