library("shiny")

# ciekawy artykul
# http://www.pacea.u-bordeaux1.fr/IMG/pdf/AnthropMMD_v101_en.pdf
# https://www.academia.edu/14001615/A_critical_review_of_the_Mean_Measure_of_Divergence_and_Mahalanobis_Distances_using_artificial_data_and_new_approaches_to_the_estimation_of_biodistances_employing_nonmetric_traits

fluidPage(
  fluidRow(class = "myRow1",
    column(width = 3, h2("Dental Affinities"),
           p("Link to the article, contact email"),
           checkboxInput("button",label = "Example data", value = TRUE)),
    column(width = 3,
           selectInput("method_sel", label = "Method selection",
                       choices = c("MMD - Anscombe" = "MMD_ANS_0",
                                   "MMD - Freeman & Tukey" = "MMD_FRE_0",
                                   "MMD - Anscombe (Freeman & Tukey correction)" = "MMD_ANS_FRE",
                                   "MMD - Anscombe (Grewal correction)" = "MMD_ANS_GRE",
                                   "MMD - Freeman & Tukey (Freeman & Tukey correction)" = "MMD_FRE_FRE",
                                   "MMD - Freeman & Tukey (Grewal correction)" = "MMD_FRE_GRE",
                                   "[not ready] Mahalanobis - tetrachoric correlation (TMD)" = "MAH_TMD",
                                   "[not ready] Mahalanobis - ordinal (OMD)" = "MAH_OMD",
                                   "[not ready] Mahalanobis - corrected ordinal (COMD)" = "MAH_COMD",
                                   "[not ready] Mahalanobis - Pearson correlation coefficients (RMD)" = "MAH_RMD",
                                   "[not ready] PCA - polychoric correlation " = "MAH_PCA"
                       ),
                       selected = "MMD_ANS"),
           fileInput('file1', 'Upload your data as an XLSX file',
                      accept=c('.csv', 'application/xlsx',
                               'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                               '.xlsx'))
        ),
    column(width = 3,
           selectInput("sex_handling", label = "Sex handling",
                       choices = c("All individuals" = "ALL",
                                   "Males only" = "MALE",
                                   "Females only" = "FEMALE",
                                   "[not ready] Sample-wise selection" = "SAMPLE",
                                   "[not ready] Predefined selection" = "PRE"
                       ), selected = "ALL"),
           selectInput("binarisation", label = "Binarization",
                       choices = c("User defined" = "USER",
                                   "Balanced" = "BALANCED",
                                   "[not ready] Highest inter sample" = "HIGH"
                       ),
                       selected = "BALANCED")),
    column(width = 3,
           selectInput("init_trait", label = "Initial trait selection",
                       choices = c("All traits" = "ALL",
                                   "Only right side" = "RIGHT",
                                   "Only left side" = "LEFT",
                                   "Maximum score" = "MAX",
                                   "Minimum score" = "MIN",
                                   "Average score" = "AVG"
                       ),
                       selected = "ALL"),
           selectInput("post_trait", label = "Post-hoc trait selection",
                       choices = c("All traits" = "ALL",
                                   "[not ready] Traits that differentiate" = "DIFF",
                                   "[not ready] Traits with high inter-sample variance" = "VAR"
                       ),
                       selected = "ALL"))
    ),
  fluidRow(
    column(width = 4, plotOutput('ggMDS', width = 400, height = 400)),
    column(width = 4, plotOutput('ggClust', width = 400, height = 400)),
    column(width = 4,
           downloadLink('downloadData', '* Download matrixes as XLSX file *'),
           p("Distance matrix"),
           verbatimTextOutput('distSummary'),
           p("SD matrix"),
           verbatimTextOutput('sdSummary'),
           p("Significance matrix"),
           verbatimTextOutput('signifSummary'))
  ),
  fluidRow(
    column(width = 4, plotOutput('ggCzekanowski', width = 400, height = 400))
  ),
  tags$head(tags$style("
      .myRow1{background-color: #dddddd;}
      .myRow3{height:3px; background-color: #dddddd;}
     "
  ))
)
