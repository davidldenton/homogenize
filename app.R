# Shiny application that standardizes variables for a given ID column

library(tidyverse)
library(shiny)
library(shinythemes)
library(rlang)

ui <- fluidPage(theme = shinytheme('cerulean'),
  
    tags$head(
        tags$style(HTML('h1 {background-color: #357FAA; text-align: center; color: #FFFFFF;}'))
    ),

    headerPanel('Data Standardization Machine',
                windowTitle = 'Data Standardization Machine'),
  
    br(),
  
    sidebarLayout(
    
        sidebarPanel(
            
            tags$h5('This app will standardize one or more variables for each value in a given ID column.'),
            
            tags$p('Upload a delimited text file, indiciate if it has a header, choose the delimiter, 
              and then enter the ID variable name or position as well as the names or positions 
              of the variables to standardize. Once submitted, the variables listed will be populated 
              with the most frequently occuring value for each ID. The standardized table is then 
              printed to the screen and can be downloaded as a csv file.'),
            
            tags$a(href="https://github.com/davidldenton/standardize",
                   tags$strong("Additional instructions & git repo", style = "color:purple")),
            
            tags$h3('Input controls'),

            # Choose text file to up;oad
            fileInput(inputId = 'in_file',
                      label = 'Choose text file:',
                      accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),
                      buttonLabel = 'Browse'),

            # Indicate if the file has a header or not
            radioButtons(inputId = 'header_check',
                         label = NULL,
                         choices = c('File with header', 'File without header')),
            
            # Choose delimiter
            selectInput(inputId = 'delimiter',
                        label = 'Delimiter:',
                        choices = c(',', '|', 'tab'),
                        selected = ','),
            
            # Input name or position of ID column
            textInput(inputId = 'id_var',
                      label = HTML('ID column name or numeric position'),
                      value = ''),
            
            # Input list of variables to standardize
            textInput(inputId = 'variables',
                      label = HTML('List of variable names or numeric positions <br/>(comma-separated)'),
                      value = ''),
            
            # Submit file for standardization process
            actionButton(inputId = 'submit',
                         label = 'Submit',
                         icon('upload'))
      
        ),
    
        mainPanel(
            
            tags$h3('Standardized data'),
            
            # Print standardized data frame
            tableOutput('data_table'),
            
            # Download standardized data
            downloadButton('download', 'Download')
      
        )
    )
)


server <- function(input, output){

    # Define function to standardize variables
    standardize_vars <- function(df, id_var, ...){

        id_var <- enquo(id_var)
        id_var_name <- quo_name(id_var)
        ns_vars <- quos(...)
        ns_var_names <- flatten_chr(map(ns_vars, quo_name))
        all_var_names <- c(id_var_name, ns_var_names)


        replacement_values <- df %>%
            select_at(.vars = vars(one_of(all_var_names))) %>%
            select(!!id_var, !!!ns_vars) %>%
            group_by(!!id_var) %>%
            mutate_at(.vars = vars(one_of(ns_var_names)),
                      .funs = funs(ifelse(is.na(.) | . == '', '0', .))) %>%
            gather(var_name, var_value, ns_var_names) %>%
            group_by(!!id_var, var_name, var_value) %>%
            summarise(n = n()) %>%
            group_by(!!id_var, var_name) %>%
            summarise(row_count = n(),
                first_value = first(var_value, order_by = desc(n)),
                second_value = nth(var_value, 2, order_by = desc(n)),
                replacement_value = case_when(
                  first_value != '0' ~ first_value,
                  !is.na(second_value) ~ second_value,
                  TRUE ~ '0')) %>%
            group_by(!!id_var) %>%
            select(!!id_var, var_name, replacement_value) %>%
            spread(var_name, replacement_value)


        df <- df %>%
            rename_at(.vars = vars(one_of(ns_var_names)),
                      .funs = funs(paste0(., '_tmp'))) %>%
            left_join(replacement_values, by = id_var_name) %>%
            select(-contains('_tmp'))
    }

    # Print standardized data frame to screen after submit
    observeEvent(input$submit, {
        output$data_table <- renderTable({
            
        # Return null if no file is uploaded
        in_file <- input$in_file
        isolate(if(is.null(in_file)) return(NULL))
        
        # Create boolean object to indicate the presence (or not) of a header in the file
        header_bool <- isolate(
            if_else(input$header_check == 'File with header', TRUE, FALSE))
        
        # Read text file and save as 'download_data'
        download_data <- isolate(
            if(input$delimiter == 'tab'){
                read_tsv(in_file$datapath,
                         col_names = header_bool,
                         col_types = cols(.default = "c"))
            }
            else{
                read_delim(in_file$datapath,
                           delim = input$delimiter,
                           col_names = header_bool,
                           col_types = cols(.default = "c"))
            }
        )
        
        # Create vector of variables names that require standardization
        vars_to_standardize <- isolate(
            if(header_bool){
                strsplit(input$variables, split = ',') %>%
                flatten_chr() %>%
                trimws(which = 'both')
            }
            else{
                strsplit(input$variables, split = ',') %>%
                map(function(x) paste0('X', x)) %>%
                flatten_chr() %>%
                trimws(which = 'both')
            }
        )
        
        # Identify name of ID column
        id_col <- isolate(
            if(header_bool){
                input$id_var
            }
            else{
                paste0('X', input$id_var)
            }
        )
        
        # Arguments to pass to standardize_vars() function
        args <- isolate(syms(c('download_data',
                               id_col,
                               vars_to_standardize)))
        
        # Create data frame with standardized data
        download_data <<- head(do.call(standardize_vars, args), 25)
        
        # Define table formatting
        }, striped = TRUE, align = 'c')
        
        # Create file of standardized data for download
        output$download <- downloadHandler(
            filename = 'clean_data.csv',
            content = function(file){
                write.csv(download_data, file, row.names = FALSE, na = '')
        
            }
        )
    
    })

}

# Create Shiny app
shinyApp(ui = ui, server = server)
