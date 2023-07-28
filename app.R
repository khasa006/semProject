############## Add Source Files #########

source("resources.R")

###### Libraries ######

library(conflicted)
library(shinydashboard)
library(shiny)

conflicts_prefer(shinydashboard::box)

######################### app  ######################

ui <- dashboardPage(
  dashboardHeader(
    title = "The Impact of Heavy Metals Exposure on Adverse Pregnancy Outcomes",
    titleWidth = 700,
    tags$li(class = "dropdown",
            tags$a(href = "https://github.com/khasa006/semProject", icon("github"), "Source Code", target = "_blank")
    )
  ), # dashboardHeader

  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("About", tabName = "about", selected = TRUE),
      menuItem("Methods", tabName = "methods"),
      menuItem("Results",
               tabName = "results",
               menuSubItem("Descriptive", tabName = "descriptive"),
               menuSubItem("Pre", tabName = "pre"),
               menuSubItem("Trim1", tabName = "trim1"),
               menuSubItem("Trim2", tabName = "trim2"),
               menuSubItem("Summary of Models", tabName = "summary"),
               menuSubItem("Test Model", tabName = "test"),
               menuSubItem("SEM Model", tabName = "sem")
      ), # menuItem
      menuItem("Discussion", tabName = "discussion"),
      menuItem("Conclusion", tabName = "conclusion")
    ) # sidebarMenu
  ), # dashboardSidebar

  dashboardBody(
    tabItems(
      # First tab Item
      tabItem(
        tabName = "about",
        tabBox(
          id = "t1", width = 12,
          tabPanel("Abstract", fluidPage(includeMarkdown("abstract.Rmd"))),
          tabPanel("Introduction", fluidPage(includeMarkdown("introduction.Rmd")))
        ) # tabBox
      ), # tabItem1

      # 2nd tab item
      tabItem(
        tabName = "methods",
        tabBox(
          id = "t2", width = 12,
          tabPanel("Lasso Regression", fluidPage(includeMarkdown("lasso.Rmd"))),
          tabPanel("Structural Equation Modeling", fluidPage(includeMarkdown("sem.Rmd"))),
          tabPanel("Data Analysis", fluidPage(includeMarkdown("analysis.Rmd")))
        ) # tabBox
      ), # tabItem2

      # 3rd tab Item
      tabItem(
        tabName = "results",
        tabBox(
          id = "t3", width = 12,
          tabPanel(
            "Descriptive",
            fluidRow(
              box(
                width = 12,
                includeMarkdown("descriptive.Rmd")
              )
            )
          ), # tabpanel
          tabPanel(
            "Pre",
            fluidRow(
              box(
                width = 12,
                includeMarkdown("pre.Rmd")
              )
            ),
            fluidRow(
              box(
                width = 12, status = "primary",
                title = "Important Predictors ",
                plotOutput("vip_lasso_pre")
              )
            ),
            fluidRow(
              box(
                width = 4, status = "primary",
                title = "Prediction Performance",
                tableOutput("lasso_matrices_pre")
              ),
              box(
                width = 4, status = "primary",
                title = "Confusion Matrix",
                plotOutput("lasso_conf_mat_pre")
              )
            ), # fluidrow
            fluidRow(
              box(
                width = 4, status = "primary",
                title = "ROC plot",
                plotOutput("roc_lasso_plot_pre")
              ),
              box(
                width = 4, status = "primary",
                title = "Prediction Plot",
                plotOutput("Pred_plot_lasso_pre")
              )
            ) # fluidrow
          ), # tabpanel
          tabPanel(
            "Trim1",
            fluidRow(
              box(
                width = 12,
                includeMarkdown("trim1.Rmd")
              )
            ),
            fluidRow(
              box(
                width = 12, status = "primary",
                title = "Important Predictors ",
                plotOutput("vip_lasso_trim1")
              )
            ),
            fluidRow(
              box(
                width = 4, status = "primary",
                title = "Prediction Performance",
                tableOutput("lasso_matrices_trim1")
              ),
              box(
                width = 4, status = "primary",
                title = "Confusion Matrix",
                plotOutput("lasso_conf_mat_trim1")
              )
            ), # fluidrow
            fluidRow(
              box(
                width = 4, status = "primary",
                title = "ROC plot",
                plotOutput("roc_lasso_plot_trim1")
              ),
              box(
                width = 4, status = "primary",
                title = "Prediction Plot",
                plotOutput("Pred_plot_lasso_trim1")
              )
            ) # fluidrow
          ),
          tabPanel(
            "Trim2",
            fluidRow(
              box(
                width = 12,
                includeMarkdown("trim2.Rmd")
              )
            ),
            fluidRow(
              box(
                width = 12, status = "primary",
                title = "Important Predictors ",
                plotOutput("vip_lasso_trim2")
              )
            ),
            fluidRow(
              box(
                width = 4, status = "primary",
                title = "Prediction Performance",
                tableOutput("lasso_matrices_trim2")
              ),
              box(
                width = 4, status = "primary",
                title = "Confusion Matrix",
                plotOutput("lasso_conf_mat_trim2")
              )
            ), # fluidrow
            fluidRow(
              box(
                width = 4, status = "primary",
                title = "ROC plot",
                plotOutput("roc_lasso_plot_trim2")
              ),
              box(
                width = 4, status = "primary",
                title = "Prediction Plot",
                plotOutput("Pred_plot_lasso_trim2")
              )
            ) # fluidrow
          ),
          tabPanel(
            "summary",
            fluidRow(
              box(
                width = 12,
                includeMarkdown("summary.Rmd")
              )
            ),
            fluidRow(
              box(
                width = 12, status = "primary",
                title = "Model Comparison",
                tableOutput("com_table")
              ),
            )
          ),
          tabPanel(
            "Test",
            fluidRow(
              box(
                width = 12,
                includeMarkdown("test.Rmd")
              )
            ),
            fluidRow(
              box(
                width = 12, status = "primary",
                title = "Important Predictors ",
                plotOutput("test_vip")
              )
            ),
            fluidRow(
              box(
                width = 4, status = "primary",
                title = "Prediction Performance",
                tableOutput("test_roc")
              )
            ),
            fluidRow(
              box(
                width = 4, status = "primary",
                title = "ROC plot",
                plotOutput("test_roc_plot")
              ),
              box(
                width = 4, status = "primary",
                title = "Prediction Plot",
                plotOutput("test_plot")
              )
            ) # fluidrow
          )
        ), # tabBox
        tabPanel(
          "SEM",
          fluidRow(
            box(
              width = 12,
              includeMarkdown("sem.Rmd")
            )
          ),
          fluidRow(
            box(
              width = 12, status = "primary",
              title = "CFA Summary ",
              plotOutput("cfaFit_summary")
            )
          ),
          fluidRow(
            box(
              width = 4, status = "primary",
              title = "SEM Summary",
              tableOutput("semFit_summary_rds")
            )
          ),
          fluidRow(
            box(
              width = 4, status = "primary",
              title = "SEM Plot",
              plotOutput("test_plot")
            )
          ) # fluidrow
        )
      ), # tabItem3

      # 4th tab item
      tabItem(
        tabName = "discussion",
        tabBox(
          id = "t1", width = 12,
          tabPanel("Discussion", fluidPage(includeMarkdown("discussion.Rmd")))
        ) # tabBox
      ), # tabItem4

      # 5th tab Item
      tabItem(
        tabName = "conclusion",
        tabBox(
          id = "t1", width = 12,
          tabPanel("Conclusion", fluidPage(includeMarkdown("conclusion.Rmd")))
        ) # tabBox
      ) # tabItem5
    ) # tabItems
  ) # dashboardBody
) # dashboardPage

server <- function(input, output, session) {
  ## Lasso output for pre

  output$vip_lasso_pre <- renderPlot(
    vip_lasso_pre_rds
  )

  output$lasso_matrices_pre <- renderTable(
    lasso_matrices_pre_rds %>%
      select(-".config") %>%
      rename(
        "Metric" = ".metric",
        "Estimator" = ".estimator",
        "Estimate" = ".estimate"
      )
  )

  output$lasso_conf_mat_pre <- renderPlot(
    lasso_conf_mat_pre_rds
  )

  output$roc_lasso_plot_pre <- renderPlot(
    roc_lasso_plot_pre_rds
  )

  output$Pred_plot_lasso_pre <- renderPlot(
    Pred_plot_lasso_pre_rds
  )

  ## Lasso output for trim1

  output$vip_lasso_trim1 <- renderPlot(
    vip_lasso_trim1_rds
  )

  output$lasso_matrices_trim1 <- renderTable(
    lasso_matrices_trim1_rds %>%
      select(-".config") %>%
      rename(
        "Metric" = ".metric",
        "Estimator" = ".estimator",
        "Estimate" = ".estimate"
      )
  )

  output$lasso_conf_mat_trim1 <- renderPlot(
    lasso_conf_mat_trim1_rds
  )

  output$roc_lasso_plot_trim1 <- renderPlot(
    roc_lasso_plot_trim1_rds
  )

  output$Pred_plot_lasso_trim1 <- renderPlot(
    Pred_plot_lasso_trim1_rds
  )

  ## Lasso output for trim2

  output$vip_lasso_trim2 <- renderPlot(
    vip_lasso_trim2_rds
  )

  output$lasso_matrices_trim2 <- renderTable(
    lasso_matrices_trim2_rds %>%
      select(-".config") %>%
      rename(
        "Metric" = ".metric",
        "Estimator" = ".estimator",
        "Estimate" = ".estimate"
      )
  )

  output$lasso_conf_mat_trim2 <- renderPlot(
    lasso_conf_mat_trim2_rds
  )

  output$roc_lasso_plot_trim2 <- renderPlot(
    roc_lasso_plot_trim2_rds
  )

  output$Pred_plot_lasso_trim2 <- renderPlot(
    Pred_plot_lasso_trim2_rds
  )

  # comparison table

  output$compare_table <- renderTable(
    compare_table
  )

  # test outputs

  output$test_roc <- renderTable(
    test_roc_rds %>%
      rename(
        "Metric" = ".metric",
        "Estimator" = ".estimator",
        "Estimate" = ".estimate"
      )
  )

  output$test_roc_plot <- renderPlot(
    test_roc_plot_rds
  )

  output$test_vip <- renderPlot(
    test_vip_rds
  )

  output$test_plot <- renderPlot(
    test_plot_rds
  )

  # SEM output

  output$cfaFit_summary <- renderTable(
    cfaFit_summary_rds
  )

  output$semFit_summary <- renderTable(
    semFit_summary_rds
  )

  output$semPlot <- renderTable(
    semPlot_rds
  )
}

shinyApp(ui, server)
