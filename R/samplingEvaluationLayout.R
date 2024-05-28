samplingEvaluation_GUI <- function() {

  ui <- fluidPage(
    useShinyjs(), # Use shinyjs
    titlePanel("Audit Sampling"),
    tabsetPanel(id = "tabs",
                tabPanel("Input Sampling Design",
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
                                       accept = c(".xlsx, .xls, .xlsm, .xlsb, .csv")
                             ),
                             actionButton("next_button", "Next")
                           ),
                           tabPanel(
                             tableOutput("contents")
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
    hideTab(inputId = "tabs", target = "Input Sample Units")
    hideTab(inputId = "tabs", target = "Evaluation parameter")
    hideTab(inputId = "tabs", target = "Evaluation")
    shinyjs::hide("next_to_evaluation")
    shinyjs::hide("downloadDesign")
    shinyjs::hide("file1")
    shinyjs::hide("next_button")

    primaryKey = "primaryKey"
    booked_column_name = "Booked_Values"
    audit_column_name = "Audited_Values"

    # columnNamesClass <- columnNames(primaryKey = primaryKey, booked_column_name = booked_column_name, audit_column_name = audit_column_name)
    # assign("col_data", columnNamesClass, envir = .AuditSampling_env)
    # print(.AuditSampling_env$col_data)

    new_data_react <- reactiveVal(NULL)
    parameters_react <- reactiveVal(NULL)
    sample_units_data_react <- reactiveVal(NULL)
    sample_data_react <- reactiveVal(TRUE)
    result_react <- reactiveVal(NULL)
    samplingDesign_react <- reactiveVal(NULL)
    sampleUnits_react <- reactiveVal(NULL)
    evaluation_react <- reactiveVal(NULL)
    new_sampleUnits_react <- reactiveVal(NULL)
    new_evaluation_react <- reactiveVal(NULL)
    is_new_sample_react <- reactiveVal(NULL)
    is_new_sample_react(FALSE)
    selected_column <- reactiveVal(NULL)

    #selected_column("Valor")


    #samplingDesign_react(AuditSampling::samplingDesign)
    #sampleUnits_react(AuditSampling::sampleUnits)

    formData <- reactiveValues(data = list())
    data_react <- reactiveVal()
    #userResponse <- reactiveValues(response = NULL)
    userResponse <- reactiveVal(NULL)


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

    # # Define the download handler
    # output$downloadSample <- downloadHandler(
    #   filename = function() {
    #     paste("sampleUnits-", Sys.Date(), ".csv", sep="")
    #   },
    #   content = function(file) {
    #     # Define the dataframe based on a condition
    #
    #     if (is_new_sample_react()) {
    #       dataframe <- new_sampleUnits_react()
    #     } else {
    #       dataframe <- sampleUnits_react()
    #     }
    #     # Write the data to a file
    #     write.csv(dataframe, file, row.names = FALSE)
    #   }
    # )

    output$downloadSample <- downloadHandler(
      filename = function() {
        paste("sampleData-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        # Define the dataframe based on a condition

        # Create a new workbook
        wb <- createWorkbook()

        # Add each data frame to a separate sheet in the workbook
        addWorksheet(wb, "Parameters")
        parameter <- parameters_react()
        parameter$step[[1]] <- parameter$step[[1]] + 1
        writeData(wb, "Parameters", data.frame(parameter))

        addWorksheet(wb, "Sample Design")
        writeData(wb, "Sample Design", samplingDesign_react())

        addWorksheet(wb, "Stratified Data")
        writeData(wb, "Stratified Data", new_data_react())

        addWorksheet(wb, "Sample to Analyse")
        writeData(wb, "Sample to Analyse", sampleUnits_react())

        # Write the data to a file
        saveWorkbook(wb, file)
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
          #data_react(AuditSampling::invoices)
          #selected_column("Invoice_Amount")
          shinyjs::show("next_button")

        } else {
          sample_data_react(FALSE)
          shinyjs::show("file1")
          shinyjs::hide("next_button")
        }
      }
    })

    # observe({
    #   if (!is.null(userResponse())) {
    #     if (userResponse() == "Yes") {
    #       new_samples <- take_more_samples(data_react(), selected_column(), formData$data$precision, sampleUnits_react(), evaluation_react(), formData$data$confidence, formData$data$estimation_method, t_Student = FALSE)
    #       new_sampleUnits_react(new_samples$unitsToExamine)
    #       new_evaluation_react(new_samples$eval_dataframe)
    #
    #       #print(new_evaluation_react())
    #
    #
    #       old_n <- evaluation_react() %>%
    #         filter(Stratum == "Total") %>%
    #         summarise(n = sum(nsample, na.rm = TRUE))
    #
    #       new_n <- new_evaluation_react() %>%
    #         filter(Stratum == "Total") %>%
    #         summarise(n = sum(nsample, na.rm = TRUE))
    #
    #       #print(old_n$n)
    #       #print(new_n$n)
    #
    #       precision <- max(new_evaluation_react()$precision, na.rm = TRUE)
    #
    #       show_warning_msg(old_n$n, new_n$n, round(precision,2), round(formData$data$precision,2))
    #
    #       output$dataTableEvaluate <- renderTable({
    #         #shinyjs::html("feedback2", "")
    #         as.data.frame(new_evaluation_react())
    #       })
    #
    #       userResponse(NULL)
    #       is_new_sample_react(TRUE)
    #
    #     } else {
    #       is_new_sample_react(FALSE)
    #     }
    #   }
    # })

    observeEvent(input$file1, {

      df <- read_sampling_file(input$file1$datapath)


      parameters_react(df$parameters)
      selected_column(parameters_react()$data_column_name)
      samplingDesign_react(df$sampleDesign)
      data_react(df$stratifiedData)
      sampleUnits_react(df$sampletoAnalyse)

      #print(data_react())


      #print(sample_data_react())
      #samplingDesign_react(df)
      shinyjs::show("next_button")

    })


    observeEvent(input$next_button, {

      output$dataTableEvaluate <- renderTable({ data.frame() })

      hideTab(inputId = "tabs", target = "Input Sampling Design")
      showTab(inputId = "tabs", target = "Evaluation")

      # formData$data <- list(
      #   precision = input$precision,
      #   estimation_method = input$estimation_method,
      #   allocation_method = input$allocation_method,
      #   confidence = input$confidence
      # )

      evaluation_react(evaluate_sample(samplingDesign_react(), sampleUnits_react(), booked_column_name, audit_column_name, parameters_react()$confidence, parameters_react()$estimation_method))

      unitsToExamine <- sampleUnits_react()

      my_data <- data_react() %>%
        filter(unitToSample == 0) %>%
        select(-unitToSample)

      new_samples <- take_more_samples(my_data, selected_column(), booked_column_name, audit_column_name,  parameters_react()$precision, samplingDesign_react(), unitsToExamine, evaluation_react(), parameters_react()$confidence, parameters_react()$estimation_method)

      new_data_react(updateDataBaseUnitsToSample(data_react(), selected_column(), primaryKey, unitsToExamine))


      new_ni <- new_samples$eval_dataframe %>%
        filter(grepl("^\\d+$", Stratum)) %>%
        select(nsample)

      strata <- build_final_strata_update(samplingDesign_react(), new_ni$nsample)


      sampleUnits_react(new_samples$unitsToExamine)
      samplingDesign_react(strata)

      evaluation_react(evaluate_sample(samplingDesign_react(), sampleUnits_react(), booked_column_name, audit_column_name, parameters_react()$confidence, parameters_react()$estimation_method))

      output$dataTableEvaluate <- renderTable({
        #shinyjs::html("feedback2", "")
        as.data.frame(nicer_number_view(evaluation_react()))
      })


    })


    observeEvent(input$new_sample, {

      output$dataTableEvaluate <- renderTable({ data.frame() })

      print("here")

      precision <- max(evaluation_react()$precision, na.rm = TRUE)
      #print(precision)
      #print("precision")
      if (!is.na(precision) & precision > parameters_react()$precision) {

        show_warning_yes_no(round(precision,2),round(parameters_react()$precision,2))

      }

      print("end")

    })


  }

  shinyApp(ui = ui, server = server)

}
