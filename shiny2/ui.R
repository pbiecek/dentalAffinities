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
    column(width = 2,
           p("Options:"),
           selectInput("init_trait", label = "Initial trait selection",
                       choices = c("All traits" = "ALL",
                                   "Only right side" = "RIGHT",
                                   "Only left side" = "LEFT",
                                   "Maximum score" = "MAX",
                                   "Minimum score" = "MIN",
                                   "Average score" = "AVE"
                       ),
                       selected = "ALL"),
           selectInput("sex_handling", label = "Sex handling",
                       choices = c("All individuals" = "ALL",
                                   "Males only" = "MALE",
                                   "Females only" = "FEMALE",
                                   "Sample-wise selection" = "SAMPLE",
                                   "Predefined selection" = "PRE"
                       ),
                       selected = "ALL"),
           selectInput("post_trait", label = "Post-hoc trait selection",
                       choices = c("All traits" = "ALL",
                                   "Traits that differentiate" = "DIFF",
                                   "Traits with high inter-sample variance" = "VAR"
                       ),
                       selected = "ALL")),
    column(width = 2,
           p("Options:"),
           selectInput("method_sel", label = "Method selection",
                       choices = c("MMD - Anscombe" = "MMD_ANS_0",
                                   "MMD - Anscombe (Freeman & Tukey correction)" = "MMD_ANS_FRE",
                                   "MMD - Anscombe (Grewal correction)" = "MMD_ANS_GRE",
                                   "MMD - Freeman & Tukey" = "MMD_FRE_0",
                                   "MMD - Freeman & Tukey (Freeman & Tukey correction)" = "MMD_FRE_FRE",
                                   "MMD - Freeman & Tukey (Grewal correction)" = "MMD_FRE_GRE",
                                   "Mahalanobis - tetrachoric correlation (TMD)" = "MAH_TMD",
                                   "Mahalanobis - ordinal (OMD)" = "MAH_OMD",
                                   "Mahalanobis - corrected ordinal (COMD)" = "MAH_COMD",
                                   "Mahalanobis - Pearson correlation coefficients (RMD)" = "MAH_RMD",
                                   "PCA - polychoric correlation " = "MAH_PCA"
                       ),
                       selected = "MMD_ANS"),
           selectInput("binarisation", label = "Binarization",
                       choices = c("User defined" = "USER",
                                   "Balanced" = "BALANCED",
                                   "Highest inter sample" = "HIGH"
                       ),
                       selected = "BALANCED"))
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
