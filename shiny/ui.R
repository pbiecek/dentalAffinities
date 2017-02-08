library("shiny")
library("rbokeh")
library("htmlwidgets")

# ciekawy artykul
# http://www.pacea.u-bordeaux1.fr/IMG/pdf/AnthropMMD_v101_en.pdf
# https://www.academia.edu/14001615/A_critical_review_of_the_Mean_Measure_of_Divergence_and_Mahalanobis_Distances_using_artificial_data_and_new_approaches_to_the_estimation_of_biodistances_employing_nonmetric_traits

fluidPage(
  fluidRow(class = "myRow1",
    column(width = 4, h2("Dental Affinities"), p("Link to the article, contact email")),
    column(width = 4,
           fileInput('file1', 'Or upload your data in XLSX file',
                     accept=c('.csv', 'application/xlsx',
                              'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                              '.xlsx')),
           br(),
           checkboxInput("button",label = "Example data")
        ),
    column(width = 4,
           p("Options:"),
           selectInput("measure", label = "Select distance measure", 
                       choices = c("Mean measure of divergence" = "MMD"
                                   #"Tetrachoric Mahalanobis D2 distance" = "TMD"
                                   ), 
                       selected = "MMD"),           
#           selectInput("reduce", label = "How to reduce values", 
#                       choices = c("Equal sizes" = "EQU", 
#                                   "Max difference" = "MDI"), 
#                       selected = "EQU"),
          selectInput("transforamtion", label = "How to transform values", 
                      choices = c("Anscombe transformation" = "ATR", 
                                  "Freeman-Tukey transformation" = "FTT"), 
                      selected = "ATR"))
    ),
  fluidRow(
    column(width = 4, plotOutput('ggMDS')),
    column(width = 4, plotOutput('ggClust')),
    column(width = 4,  verbatimTextOutput('textSummary'))
  ),
  tags$head(tags$style("
      .myRow1{background-color: #dddddd;}
      .myRow3{height:3px; background-color: #dddddd;}
     "
  ))
)
