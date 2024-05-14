
show_layout <- function(my_data, data_column_name) {

  #library(shiny)
  #library(shinyjs)

  ui <- fluidPage(
    useShinyjs(), # Initialize shinyjs

    titlePanel("Audit Sampling"),
    sidebarLayout(
      sidebarPanel(
        numericInput("precision", "Desired precision", value = 1925441129.31),
        #numericInput("precision", "Desired precision", value = 928003.97),
        numericInput("n_min", "Global minimum n", value = 30),
        numericInput("ni_min", "Minimum n per stratum", value = 5),
        selectInput("estimation_method", "Estimation method", choices = c("mean", "difference")),
        selectInput("allocation_method", "Allocation method", choices = c("Neyman", "proportional")),
        sliderInput("confidence", "Conficende Level", min = 0.50, max = 0.99, value = 0.95, step = 0.01), # Step size changed to 0.01
        #sliderInput("L", "Sequence Intervals", min = 3, max = 20, value = 3, step = 1),  New scrollbar for sequence
        sliderInput("L", "Sequence Intervals", min = 3, max = 20, value = c(5, 7)),
        actionButton("submit", "Submit")
      ),

      mainPanel(
        div(id = "feedback"), # This will display the feedback message
        tableOutput("dataTable"), # This will display the table
        downloadButton("downloadDesign", "Download Sampling Design"), # This will create the download button
        downloadButton("downloadSample", "Download Sample Units"), # This will create the download button

      )
    )
  )

  server <- function(input, output, session) {
    formData <- reactiveValues(data = list())

    # Hide the download button at the start
    shinyjs::hide("downloadDesign")
    shinyjs::hide("downloadSample")

    observeEvent(input$submit, {

      # Hide the download button at the start
      shinyjs::hide("downloadDesign")
      shinyjs::hide("downloadSample")

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

      # Now we will render the table when the submit button is clicked

      my_data <- my_data
      data_column_name <- data_column_name

      #my_data <- loadedData$data
      #data_column_name <- loadedData$data_column_name

      confidence <- formData$data$confidence
      # L <- formData$data$L
      # ni_min <- formData$data$ni_min
      # n_min <- formData$data$n_min
      # desired_precision <- formData$data$precision
      # estimation_method <- formData$data$estimation_method
      # allocation_method <- formData$data$allocation_method

      #break_n <- 4
      #user_cutoff = desired_precision/2


      # result <- execute(
      #   my_data = my_data,
      #   data_column_name = data_column_name,
      #   user_cutoff = user_cutoff,
      #   estimation_method = estimation_method,
      #   allocation_method = allocation_method,
      #   L = L,
      #   confidence = confidence,
      #   desired_precision = desired_precision,
      #   n_min = n_min,
      #   ni_min = ni_min,
      #   break_n = break_n)


      result <- execute(
        my_data = my_data,
        data_column_name = data_column_name,
        user_cutoff =  formData$data$precision/2,
        estimation_method = formData$data$estimation_method,
        allocation_method = formData$data$allocation_method,
        L = formData$data$L,
        confidence = formData$data$confidence,
        desired_precision = formData$data$precision,
        n_min = formData$data$n_min,
        ni_min = formData$data$ni_min,
        break_n = 4)



      print(result$sample_planning)
      #all_iteration_strata <- result$all_iteration_strata %>%
      #  select(-bins, -strata)

      strata <- result$sample_planning %>%
        select(-pi, -Sum_Squares)


      #all_iteration_strata <- data.frame(all_iteration_strata)
      # print(all_iteration_strata)

      output$dataTable <- renderTable({
        shinyjs::html("feedback", "")
        as.data.frame(strata)
      })

      # Define the download handler
      output$downloadDesign <- downloadHandler(
        filename = function() {
          paste("samplingDesign-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(strata, file, row.names = FALSE)
        }
      )

      # Define the download handler
      output$downloadSample <- downloadHandler(
        filename = function() {
          paste("sampleUnits-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          # Call your custom function
          unitsToExamine <- unitsToSample(my_data, result)

          # Write the data to a file
          write.csv(unitsToExamine, file, row.names = FALSE)
        }
      )

      # Show the download button when the dataframe is ready
      shinyjs::show("downloadDesign")
      shinyjs::show("downloadSample")

    })

  }

  shinyApp(ui, server)


}


