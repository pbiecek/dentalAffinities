library("shiny")
library("openxlsx")
library("rbokeh")
library("htmlwidgets")
library("ggplot2")
library("ggrepel")
library("grid")
library("gridExtra")
library("ggthemes")
library("dplyr")
library("tidyr")
library("MASS")
library("cluster")
library("factoextra")

# df <- read.xlsx("../docs/example.xlsx", 1)
# for (i in 3:ncol(df)) 
#   df[,i] <- as.numeric(gsub(pattern = "[^0-9]",replacement = "", as.character(df[,i])))
# df <- df[, apply(df, 2, function(x) length(unique(na.omit(x)))) > 1]
# save(df, file="df_sample.rda")

function(input, output) {
  # get data
  dataInput <- reactive({
    if (input$button) {
      load("df_sample.rda")
      return(df)
    }
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    df <- read.xlsx("../docs/example.xlsx", 1)
    for (i in 3:ncol(df)) 
      df[,i] <- as.numeric(gsub(pattern = "[^0-9]",replacement = "", as.character(df[,i])))
    df <- df[, apply(df, 2, function(x) length(unique(na.omit(x)))) > 1]
    attr(df, which = "name") = inFile[[1]]
    df
  })

  # get dist
  getDist <- reactive({
    df <- dataInput()
    for (i in 3:ncol(df))
      df[,i] <- (df[,i] > mean(df[,i], na.rm = TRUE)) + 0
    dfg <- na.omit(gather(df, variables, values, c(-1, -2)))
    colnames(dfg) <- c("site", "id", "variable", "value")
    dfg %>% 
      group_by(site, variable) %>%
      summarise(m = sum(value),
              n = n(),
              theta1 = asin(1 - (2*m + 3/4)/(n + 3/4)),
              theta2 = asin(1 - (2*m)/(n+1))/2 + asin(1 - (2*m+2)/(n+1))/2) -> tmp
    if (input$transforamtion == "ATR") {
      tmp$theta <- tmp$theta1
    } else {
      tmp$theta <- tmp$theta2
    }
    tmp2 <- tmp[,c("site", "variable", "theta")] 
    tmp2s <- spread(tmp2, variable, theta)
    tmp3 <- tmp[,c("site", "variable", "n")] 
    tmp3s <- spread(tmp3, variable, n)
    tmp2s[is.na(tmp2s)] <- 0
    tmp3s[is.na(tmp3s)] <- 1
    mat <- as.matrix(dist(tmp2s[,-1]))^2
    for (i in 1:ncol(mat))
      for (j in 1:nrow(mat)) {
#        mat[i,j] <- (sum((tmp2s[i,-1] - tmp2s[j,-1])^2) - 
#                       sum((1/(tmp3s[i,-1]+0.5) + 1/(tmp3s[j,-1]+0.5))))/(ncol(tmp2s) - 1)
# bez korekty    
        mat[i,j] <- sum((tmp2s[i,-1] - tmp2s[j,-1])^2)/(ncol(tmp2)-1)
      }
    colnames(mat) <- tmp2s$site
    rownames(mat) <- tmp2s$site
    mat
  })
  
  # table
  output$ggMDS <- renderPlot({
    di <- dataInput()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    mat <- getDist()
    
    np <- isoMDS(as.dist(mat), k = 2)$points
    df <- data.frame(site = rownames(np), x = np[,1], y = np[,2])
    ggplot(df, aes(x,y)) + 
      geom_point(size=2) + 
      geom_text_repel(aes(label=site)) +
      xlab("") + ylab("") + theme_classic() +
      ggtitle("Multidimensional scaling")
  })

  # table
  output$ggClust <- renderPlot({
    di <- dataInput()
    if (is.null(di)) {
      return(grid::grid.text('Please, first upload a file with data'))
    }
    mat <- getDist()
    
    grupy <- agnes(as.dist(mat), method = "ward")
    fviz_dend(grupy, rect = TRUE, main = "Ward method")
  })
  
  output$textSummary <- renderPrint({
    di <- dataInput()
    if (is.null(di)) {
      "Upload data"
    } else {
      mat <- getDist()
      print(round(mat, 2))
    }
  })

}
