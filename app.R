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
            
      
            h3('Input controls'),
            
            fileInput(inputId = 'in_file',
                      label = 'Choose text file:',
                      accept = c('text/csv','text/comma-separated-values,text/plain','.csv'),
                      buttonLabel = 'Browse'),
      
            radioButtons(inputId = 'header_check',
                         label = NULL,
                         choices = c('File with header', 'File without header')),
      
            selectInput(inputId = 'delimiter',
                        label = 'Delimiter:',
                        choices = c(',', '|', 'tab'),
                        selected = ','),
      
            textInput(inputId = 'id_var',
                      label = HTML('ID column name or numeric position'),
                      value = ''),
      
            textInput(inputId = 'variables',
                      label = HTML('List of variable names or numeric positions <br/>(comma-separated)'),
                      value = ''),
      
            actionButton(inputId = 'submit',
                         label = 'Submit',
                         icon('upload'))
      
        ),
    
        mainPanel(
      
            h3('Standardized data'),
      
            tableOutput('data_table'),
      
            #div(style='text-align:left;',
            downloadButton('download', 'Download')
      
        )
    )
)


# Clean data and print table
server <- function(input, output){
  
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

    # Print data frame to screen
    observeEvent(input$submit, {
        output$data_table <- renderTable({
      
        in_file <- input$in_file
        isolate(if(is.null(in_file)) return(NULL))
      
        header_bool <- isolate(
            if_else(input$header_check == 'File with header', TRUE, FALSE))
      

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

        header_vars <- isolate(
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
        
        id_col <- isolate(
            if(header_bool){
                input$id_var
            }
            else{
                paste0('X', input$id_var)
            }
        )
      
        args <- isolate(syms(c('download_data',
                               id_col,
                               header_vars)))
        
        download_data <<- head(do.call(standardize_vars, args), 25)
    
        }, striped = TRUE, align = 'c')
    
        output$download <- downloadHandler(
            filename = 'clean_data.csv',
            content = function(file){
                write.csv(download_data, file, row.names = FALSE, na = '')
        
            }
        )
    
    })

}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
