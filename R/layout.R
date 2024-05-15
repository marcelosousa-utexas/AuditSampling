initGUI <- function() {

  ui <- fluidPage(
    useShinyjs(), # Use shinyjs
    titlePanel("Audit Sampling"),
    tabsetPanel(id = "tabs",
                tabPanel("Input Data",
                         sidebarLayout(
                           sidebarPanel(
                             fileInput("file1", "Choose Excel File",
                                       accept = c(".xlsx")
                             ),
                             uiOutput("column_selector"),
                             actionButton("next_button", "Next")
                           ),
                           tabPanel(
                             tableOutput("contents")
                           )
                         )
                ),
                tabPanel("Sampling",
                         sidebarLayout(
                           sidebarPanel(
                             #numericInput("precision", "Desired precision", value = 1925441129.31),
                             numericInput("precision", "Desired precision", value = 928003.97),
                             numericInput("n_min", "Global minimum n", value = 30),
                             numericInput("ni_min", "Minimum n per stratum", value = 5),
                             selectInput("estimation_method", "Estimation method", choices = c("mean", "difference")),
                             selectInput("allocation_method", "Allocation method", choices = c("Neyman", "proportional")),
                             sliderInput("confidence", "Conficende Level", min = 0.50, max = 0.99, value = 0.95, step = 0.01), # Step size changed to 0.01
                             #sliderInput("L", "Sequence Intervals", min = 3, max = 20, value = 3, step = 1),  New scrollbar for sequence
                             sliderInput("L", "Sequence Intervals", min = 3, max = 20, value = c(5, 7)),
                             actionButton("submit", "Submit")
                           ),

                           tabPanel(
                             div(id = "feedback"), # This will display the feedback message
                             tableOutput("dataTable"), # This will display the table
                             downloadButton("downloadDesign", "Download Sampling Design"), # This will create the download button
                             downloadButton("downloadSample", "Download Sample Units"), # This will create the download button
                             actionButton("next_to_evaluation", "Generate Sample")
                           )
                         )
                ),
                tabPanel("Evaluation",
                         mainPanel(
                           tableOutput("dataTableEvaluate"), # This will display the table
                           actionButton("new_sample", "New Sample")
                         )
                )
    )
  )

  server <- function(input, output, session) {
    # Hide Tab2 initially
    hideTab(inputId = "tabs", target = "Sampling")
    hideTab(inputId = "tabs", target = "Evaluation")
    result_react <- reactiveVal(NULL)
    samplingDesign_react <- reactiveVal(NULL)
    sampleUnits_react <- reactiveVal(NULL)
    evaluation_react <- reactiveVal(NULL)
    formData <- reactiveValues(data = list())
    data_react <- reactiveVal()

    observeEvent(input$file1, {
      df <- read_excel(input$file1$datapath)
      data_react(df)
      output$column_selector <- renderUI({
        selectInput("column_selector", "Select a column", choices = colnames(data_react()))
      })

      updateSelectInput(session, "column_selector", "Select a column", choices = colnames(df))

    })

    selected_column <- reactiveVal(NULL)

    # Update selected column when user makes a selection
    observeEvent(input$column_selector, {
      selected_column(input$column_selector)
    })

    selected_data <- reactive({
      req(input$column)
      data.frame(Selected_Column = data_react()[, input$column])
    })

    output$contents <- renderTable({
      req(selected_data())
      selected_data()
    })

    observeEvent(input$next_button, {
      hideTab(inputId = "tabs", target = "Input Data")
      hideTab(inputId = "tabs", target = "Evaluation")
      showTab(inputId = "tabs", target = "Sampling")

    })

    observeEvent(input$next_to_evaluation, {

      output$dataTableEvaluate <- renderTable({ data.frame() })

      hideTab(inputId = "tabs", target = "Input Data")
      hideTab(inputId = "tabs", target = "Sampling")
      showTab(inputId = "tabs", target = "Evaluation")
      #print(result_react())
      evaluation_react(evaluate_sample(samplingDesign_react(), sampleUnits_react()))

      output$dataTableEvaluate <- renderTable({
        #shinyjs::html("feedback2", "")
        as.data.frame(evaluation_react())
      })

    })


    observeEvent(input$new_sample, {

      output$dataTableEvaluate <- renderTable({ data.frame() })
      #hideTab(inputId = "tabs", target = "Input Data")
      #hideTab(inputId = "tabs", target = "Sampling")
      #showTab(inputId = "tabs", target = "Evaluation")
      #print(result_react())

      unitsToExamine <- unitsToSample(data_react(), selected_column(), result_react())
      sampleUnits_react(unitsToExamine)
      #print(sampleUnits_react())

      evaluation_react(evaluate_sample(samplingDesign_react(), sampleUnits_react()))

      output$dataTableEvaluate <- renderTable({
        #shinyjs::html("feedback2", "")
        as.data.frame(evaluation_react())
      })

    })


    observeEvent(input$submit, {

      my_data <- data_react() # Corrected
      data_column_name <- selected_column() # Corrected

      # Hide the download button at the start
      shinyjs::hide("downloadDesign")
      shinyjs::hide("downloadSample")
      shinyjs::hide("next_to_evaluation")

      # Clear the previous table immediately
      output$dataTable <- renderTable({ data.frame() })

      # Use shinyjs to show the message immediately
      shinyjs::html("feedback", "Calculating... Please wait.")


      # Simulate a delay to mimic a calculation process
      Sys.sleep(2) # This adds a 2-second pause

      #observeEvent(input$submit, {
      formData$data <- list(
        precision = input$precision,
        n_min = input$n_min,
        ni_min = input$ni_min,
        estimation_method = input$estimation_method,
        allocation_method = input$allocation_method,
        confidence = input$confidence,
        L = seq(input$L[1], input$L[2])
      )

      result_react(execute(
        my_data = my_data, # Corrected
        data_column_name = data_column_name, # Corrected
        #user_cutoff =  formData$data$precision/2,
        user_cutoff =  35000,
        estimation_method = formData$data$estimation_method,
        allocation_method = formData$data$allocation_method,
        L = formData$data$L,
        confidence = formData$data$confidence,
        desired_precision = formData$data$precision,
        n_min = formData$data$n_min,
        ni_min = formData$data$ni_min,
        break_n = 4))

      #result_react(result)

      #print(result_react()$sample_planning)

      strata <- result_react()$sample_planning %>%
        select(-pi, -Sum_Squares)

      output$dataTable <- renderTable({
        shinyjs::html("feedback", "")
        as.data.frame(strata)
      })

      samplingDesign_react(strata)
      print(samplingDesign_react())

      unitsToExamine <- unitsToSample(data_react(), selected_column(), result_react())
      sampleUnits_react(unitsToExamine)
      print(sampleUnits_react())



      # Define the download handler
      output$downloadDesign <- downloadHandler(
        filename = function() {
          paste("samplingDesign-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(samplingDesign_react(), file, row.names = FALSE)
        }
      )

      # Define the download handler
      output$downloadSample <- downloadHandler(
        filename = function() {
          paste("sampleUnits-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          # Call your custom function
          # Write the data to a file
          write.csv(sampleUnits_react(), file, row.names = FALSE)
        }
      )

      # Show the download button when the dataframe is ready
      shinyjs::show("downloadDesign")
      shinyjs::show("downloadSample")
      shinyjs::show("next_to_evaluation")

    })

  }

  shinyApp(ui = ui, server = server)

}

initGUI()
