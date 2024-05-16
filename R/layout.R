read_file <- function (filepath) {

  # List of common Excel file extensions
  excel_extensions <- c("xls", "xlsx", "xlsm", "xlsb")

  # Example file extension
  file_extension <- tools::file_ext(filepath)
  #print(file_extension)

  # Compare if the file extension is in the list of Excel extensions
  if (file_extension %in% excel_extensions) {
    df <- read_excel(filepath)
  } else if (file_extension %in% c("csv")) {
    df <- read.csv(filepath)
  } else {
    # Code to execute if the file extension is neither Excel nor CSV
    stop("Extension not supported.")
  }

}

show_warning_yes_no <- function (achieved_precision, desired_precision) {

  text = paste("Achieved Precision from the sample was ", achieved_precision ,", which is greater than the planned precision of ", desired_precision,". Do you want to increase your sample size to match the specifications?",  sep = "")

  showModal(modalDialog(
    title = "Important message",
    div(id = "textmsg", paste(text)),
    footer = tagList(
      actionButton("no", "No"),
      actionButton("yes", "Yes")
    )
  ))

}


show_warning_msg <- function (old_n, new_n, achieved_precision, desired_precision) {

  #text = paste("Achieved Precision from the sample was ", achieved_precision, ", which is greater than the planned precision of ", desired_precision ,". Do you want to increase your sample size to match the specifications?")
  text = paste("The sample size was increased from ", old_n ," to " , new_n, " to achieve the planned precision of ", desired_precision,". Now the achieved precision is", achieved_precision, ".", sep = "")

  showModal(modalDialog(
    title = "Important message",
    div(id = "textmsg", paste(text)),
    footer = tagList(
      actionButton("ok", "OK")
    )
  ))

}

initGUI <- function() {

  ui <- fluidPage(
    useShinyjs(), # Use shinyjs
    titlePanel("Audit Sampling"),
    tabsetPanel(id = "tabs",
                tabPanel("Input Data",
                         sidebarLayout(
                           sidebarPanel(

                             materialSwitch(
                               inputId = "Id006",
                               label = "Use a pre-loaded data",
                               status = "primary",
                               right = TRUE,
                               value = TRUE
                             ),

                             fileInput("file1", "Choose a Csv or Excel File",
                                       accept = c(".xlsx, .csv")
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
                             actionButton("next_to_evaluation", "Generate Sample")
                           )
                         )
                ),
                tabPanel("Evaluation",
                         mainPanel(
                           tableOutput("dataTableEvaluate"), # This will display the table
                           downloadButton("downloadEvaluation", "Download Sampling Evaluation"), # This will create the download button
                           downloadButton("downloadSample", "Download Sample Units"), # This will create the download button
                           actionButton("new_sample", "New Sample")
                         )
                )
    )
  )

  server <- function(input, output, session) {
    #Sys.sleep(1) # This adds a 2-second pause
    # Hide Tab2 initially
    hideTab(inputId = "tabs", target = "Sampling")
    hideTab(inputId = "tabs", target = "Evaluation")
    shinyjs::hide("next_to_evaluation")
    shinyjs::hide("downloadDesign")
    shinyjs::hide("file1")
    shinyjs::hide("next_button")

    sample_data_react <- reactiveVal(TRUE)
    result_react <- reactiveVal(NULL)
    samplingDesign_react <- reactiveVal(NULL)
    sampleUnits_react <- reactiveVal(NULL)
    evaluation_react <- reactiveVal(NULL)
    new_sampleUnits_react <- reactiveVal(NULL)
    new_evaluation_react <- reactiveVal(NULL)
    is_new_sample_react <- reactiveVal(NULL)
    is_new_sample_react(FALSE)

    formData <- reactiveValues(data = list())
    data_react <- reactiveVal()
    #userResponse <- reactiveValues(response = NULL)
    userResponse <- reactiveVal(NULL)

          shinyjs::hide("next_to_evaluation")

    observeEvent(input$no, {
      userResponse("No")
      removeModal()
    })

    observeEvent(input$yes, {
      userResponse("Yes")
      removeModal()
    })

    observeEvent(input$ok, {
      removeModal()
    })

    # Define the download handler
    output$downloadEvaluation <- downloadHandler(
      filename = function() {
        paste("sampleEvaluation-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        # Call your custom function
        # Write the data to a file
        if (is_new_sample_react()) {
          dataframe <- new_evaluation_react()
        } else {
          dataframe <- evaluation_react()
        }
        write.csv(dataframe, file, row.names = FALSE)
      }
    )

    # Define the download handler
    output$downloadSample <- downloadHandler(
      filename = function() {
        paste("sampleUnits-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        # Define the dataframe based on a condition

        if (is_new_sample_react()) {
          dataframe <- new_sampleUnits_react()
        } else {
          dataframe <- sampleUnits_react()
        }
        # Write the data to a file
        write.csv(dataframe, file, row.names = FALSE)
      }
    )

    # Define the download handler
    output$downloadDesign <- downloadHandler(
      filename = function() {
        paste("samplingDesign-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(samplingDesign_react(), file, row.names = FALSE)
      }
    )

    observe({
      switch_status <- input$Id006
      if (!is.null(switch_status)) {
        if (switch_status) {
          sample_data_react(TRUE)
          shinyjs::hide("file1")
          shinyjs::hide("column_selector")
          data_react(AuditSampling::invoices)
          selected_column("Invoice_Amount")
          shinyjs::show("next_button")

        } else {
          sample_data_react(FALSE)
          shinyjs::show("file1")
          shinyjs::hide("next_button")
        }
      }
    })

    observe({
      if (!is.null(userResponse())) {
        if (userResponse() == "Yes") {
          new_samples <- take_more_samples(data_react(), selected_column(), formData$data$precision, result_react(), sampleUnits_react(), evaluation_react(), formData$data$confidence, formData$data$estimation_method, t_Student = FALSE)
          new_sampleUnits_react(new_samples$unitsToExamine)
          new_evaluation_react(new_samples$eval_dataframe)

          #print(new_evaluation_react())


          old_n <- evaluation_react() %>%
            filter(Stratum == "Total") %>%
            summarise(n = sum(nsample, na.rm = TRUE))

          new_n <- new_evaluation_react() %>%
            filter(Stratum == "Total") %>%
            summarise(n = sum(nsample, na.rm = TRUE))

          #print(old_n$n)
          #print(new_n$n)

          precision <- max(new_evaluation_react()$precision, na.rm = TRUE)

          show_warning_msg(old_n$n, new_n$n, round(precision,2), round(formData$data$precision,2))

          output$dataTableEvaluate <- renderTable({
            #shinyjs::html("feedback2", "")
            as.data.frame(new_evaluation_react())
          })

          userResponse(NULL)
          is_new_sample_react(TRUE)

        } else {
          is_new_sample_react(FALSE)
        }
      }
    })

    observeEvent(input$file1, {

      df <- read_file(input$file1$datapath)
      #print(sample_data_react())
      data_react(df)
      shinyjs::show("column_selector")
      output$column_selector <- renderUI({
        selectInput("column_selector", "Select a column", choices = colnames(data_react()))
      })

      updateSelectInput(session, "column_selector", "Select a column", choices = colnames(df))
      shinyjs::show("next_button")

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

      precision <- max(evaluation_react()$precision, na.rm = TRUE)
      #print(precision)
      #print("precision")
      if (!is.na(precision) & precision > formData$data$precision) {

        show_warning_yes_no(round(precision,2),round(formData$data$precision,2))

      }

    })


    observeEvent(input$submit, {

      shinyjs::hide("downloadDesign")
      shinyjs::hide("next_to_evaluation")

      my_data <- data_react() # Corrected
      data_column_name <- selected_column() # Corrected

      # Hide the download button at the start
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
      #print(samplingDesign_react())

      unitsToExamine <- unitsToSample(data_react(), selected_column(), result_react())
      sampleUnits_react(unitsToExamine)
      #print(sampleUnits_react())

      #print(result_react()$achieved_precision)



      # Show the download button when the dataframe is ready
      shinyjs::show("downloadDesign")
      shinyjs::show("next_to_evaluation")

    })

  }

  shinyApp(ui = ui, server = server)

}
