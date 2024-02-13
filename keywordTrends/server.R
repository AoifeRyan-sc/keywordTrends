server <- function(input, output, session) {
  
  # Reactive values ----
  
  data <- shiny::reactive({
    shiny::req(input$data_upload)
    
    ext <- tools::file_ext(input$data_upload$name)
    df <- switch(ext,
                 csv = readr::read_csv(input$data_upload$datapath, skip = 2),
                 # tsv = vroom::vroom(input$data_upload$datapath, delim = "\t"),
                 xlsx = readxl::read_xlsx(input$data_upload$datapath, skip = 2),
                 shiny::validate("Invalid file; Please upload a .xlsx or .csv file")
    ) %>%
      # janitor::row_to_names(row_number = 2, remove_rows_above = FALSE) %>% # column names
      janitor::clean_names() %>%
      dplyr::filter(avg_monthly_searches != 0) %>%
      dplyr::mutate_at(15:26, as.numeric) %>% # monthly searches stored as character
      dplyr::mutate(yo_y_change = as.numeric(stringr::str_remove_all(yo_y_change, "%")),
                    three_month_change = as.numeric(stringr::str_remove_all(three_month_change, "%")),
                    avg_monthly_searches = as.numeric(avg_monthly_searches),
                    log_avg_posts = log(avg_monthly_searches)) %>%
      dplyr::select(-c(competition:in_plan)) %>%
      dplyr::rename_with(~sub("searches_(\\w+)", "\\1", .), tidyselect::starts_with("searches_")) %>%
      tidyr::drop_na()
    
  })
  
  shiny::observe({
    # dropdowns
    keyword_choices <- unique(data()$keyword)
    grouping_choices <- group_info() %>%
      dplyr::filter(!word %in% tm::stopwords(kind = "SMART")) %>%
      dplyr::arrange(dplyr::desc(n)) %>% dplyr::pull(word) %>% unique()
    
    shiny::updateSelectizeInput(inputId = "keyword", choices = keyword_choices, server = TRUE)
    shiny::updateSelectInput(inputId = "static_keyword_callout", choices = keyword_choices)
    shiny::updateSelectInput(inputId = "grouping_variable", selected = NULL, choices = grouping_choices)
    shiny::updateSelectInput(inputId = "xaxis",
                             choices = c("Average monthly searches" = "avg_monthly_searches", 
                                         stats::setNames(colnames(data())[6:17],
                                                         sapply(strsplit(colnames(data())[6:17], "_"), function(x) {
                                                           month <- month.name[match(tolower(x[1]), tolower(month.abb))]
                                                           paste0(substring(month, 1, 1), substring(month, 2), " ", x[2])
                                                         }))),
                             selected = "avg_monthly_searches")
    
    # sliders
    max_yval <- max(c(data()$yo_y_change, data()$three_month_change))
    min_yval <- min(c(data()$yo_y_change, data()$three_month_change))
    range_yval <- max_yval - min_yval
    shiny::updateSliderInput(inputId = "growth_range", min = min_yval - range_yval*0.05, max = max_yval + range_yval*0.05,
                             value = c(min_yval - range_yval*0.05, max_yval + range_yval*0.05))
    
  })
  
  output$test <- renderPrint({
    max(data() %>% dplyr::select(avg_monthly_searches, dplyr::everything()[6:17]))
  })
  
  shiny::observeEvent(input$data_upload, {
    max_searches <- max(data() %>% dplyr::select(avg_monthly_searches, dplyr::everything()[6:17]))
    # normaliser <- 10^(floor(log10(abs(max_searches))))
    # adjusted_max_searches <- ceiling(max_searches/normaliser)*normaliser
    adjusted_max_searches <- max_searches + 0.5*max_searches
    # max_xval <- floor(log10(adjusted_max_searches))
    max_xval <- log10(adjusted_max_searches)
    # max_xval <- log10(max_searches)
    shiny::updateSliderInput(inputId = "num_posts", max = max_xval, value = c(0.1, max_xval))
    
    js <- "
  setTimeout(function() {
    logifySlider('num_posts', sci = false);
  }, 10);
  "
    session$sendCustomMessage("jsCode", list(code = js))
  })
  
  random_state <- shiny::eventReactive(input$shuffle_label, {
    sample(1:1000, 1)
  }, ignoreNULL = FALSE)
  
  group_info <- shiny::reactive({
    data() %>%
      dplyr::select(keyword) %>%
      tidytext::unnest_tokens(word, keyword, drop = FALSE) %>%
      dplyr::group_by(word) %>%
      dplyr::filter(!word %in% tm::stopwords(kind = "SMART")) %>%
      dplyr::mutate(n=dplyr::n())
  })
  
  hover_df <- shiny::reactive({
    
    x_var <- rlang::sym(input$xaxis)
    y_var <- rlang::sym(input$yaxis)
    
    if(length(input$keyword)>0){
      plot_df <- data() %>%
        dplyr::arrange(log_avg_posts) %>%
        dplyr::mutate(rowid = dplyr::row_number()) %>%
        dplyr::filter(keyword %in% input$keyword) %>%
        dplyr::mutate(x = !!x_var, y = !!y_var)
    } else {
      plot_df <- data() %>%
        dplyr::arrange(log_avg_posts) %>%
        dplyr::mutate(rowid = dplyr::row_number(),
                      x = !!x_var, y = !!y_var)
      
    }
    
    hover_info <- plot_df %>%
      dplyr::group_by(x,y) %>% dplyr::mutate(group_number = dplyr::cur_group_id(), n = dplyr::n()) %>%
      dplyr::slice(1:3) %>%
      dplyr::mutate(combined_info = dplyr::case_when(n > 3 ~
                                                       paste0("<b>Keyword (Average monthly searches):</b> ",
                                                              paste0("<br>", paste0(keyword, " (", avg_monthly_searches, ")", collapse = ", "), "...")),
                                                     TRUE ~
                                                       paste0("<b>Keyword (Average monthly searches):</b> ", "<br>",
                                                              paste0(keyword, " (", avg_monthly_searches, ")", collapse = ", ")))) %>%
      dplyr::ungroup() %>% dplyr::select(group_number, combined_info) %>% dplyr::distinct()
    
    if(length(input$grouping_variable > 0)){
      
      keywords_to_highlight <- group_info() %>%
        dplyr::filter(word %in% input$grouping_variable) %>%
        dplyr::group_by(keyword) %>%
        dplyr::arrange(word) %>%
        dplyr::distinct(keyword, word) %>%
        dplyr::mutate(group_var = paste0(word, collapse = ", "))
      
      joined_df <- plot_df %>%
        dplyr::group_by(x,y) %>% dplyr::mutate(group_number = dplyr::cur_group_id()) %>%
        dplyr::left_join(hover_info, by = "group_number") %>% dplyr::ungroup() %>%
        dplyr::left_join(keywords_to_highlight %>% dplyr::select(keyword, group_var), by = "keyword") %>%
        # dplyr::mutate(selected_terms = dplyr::case_when(keyword %in% keywords_to_highlight ~ input$grouping_variable, TRUE ~ "Other")) %>%
        dplyr::mutate(group_var = dplyr::if_else(is.na(group_var), "Other", group_var)) %>%
        dplyr::mutate(rowid = dplyr::row_number())
    } else {
      joined_df <- plot_df %>%
        dplyr::group_by(x,y) %>% dplyr::mutate(group_number = dplyr::cur_group_id()) %>%
        dplyr::left_join(hover_info, by = "group_number") %>% dplyr::ungroup() %>%
        dplyr::mutate(group_var = 1) %>%
        dplyr::mutate(rowid = dplyr::row_number())
    }
    
    
    joined_df
  })
  
  df_highlighted <- shiny::reactive({
    plotly::event_data("plotly_selected")
  })
  
  coords <- shiny::reactive({
    dist0 <- abs(range(hover_df()$y)[2] -range(hover_df()$y)[1])/50
    xbuffer <- xrange()/20
    ybuffer <- yrange()/15
    # rand_coord_opt2(length(input$static_keyword_callout), xmin = 0, xmax = 1, ymin = 0, ymax = 1, min_distance = 0.05, random_state = random_state())
    rand_coord_opt2(length(input$static_keyword_callout), 
                    xmin = x_min() + xbuffer, xmax = x_max() - xbuffer, 
                    ymin = input$growth_range[1] + ybuffer, ymax = input$growth_range[2] - ybuffer, 
                    min_distance = 10, mediany = median(hover_df()$y), dist_from_median = dist0, random_state = random_state())
  })
  
  xrange <- reactive({abs(x_max() - x_min())})
  yrange <- reactive({abs(input$growth_range[2] - input$growth_range[1])})
  
  label_df <- shiny::reactive({
    x_var <- rlang::sym(input$xaxis)
    y_var <- rlang::sym(input$yaxis)
    
    
    data()%>%
      dplyr::filter(keyword %in% input$static_keyword_callout) %>%
      dplyr::mutate(x = !!x_var, y = !!y_var,
                    label = paste0(keyword,
                                   "<br><i>3 month growth: ", three_month_change, "%</i>",
                                   "<br><i> Yearly growth: ", yo_y_change, "%</i>"),
                    NA,
                    ax_col = coords()[,1],
                    ay_col = coords()[,2])
  })
  
  df_longer <- shiny::reactive({
    hover_df()[hover_df()$rowid %in% df_highlighted()$customdata, ] %>%
      tidyr::pivot_longer(dplyr::everything()[6:17], values_to = "n", names_to = "col_title") %>%
      dplyr::mutate(month = stringr::str_extract(col_title, "[A-Za-z]+"),
                    year = stringr::str_extract(col_title, "\\d{4}"),
                    date = as.Date(paste0("01-", stringr::str_to_title(month), "-", as.character(year)), format = "%d-%b-%Y")) %>%
      rename(Keyword = keyword) %>%
      dplyr::group_by(Keyword) 
  })
  
  basic_scatter <- shiny::reactive({
    
    plotly::plot_ly(data = hover_df(), x = ~x, y = ~y,
                    type = "scatter", mode = "markers",
                    # color = ~log_avg_posts,
                    color = ~as.factor(group_var),
                    customdata = ~rowid,
                    size = ~log_avg_posts, sizes = c(5, 30),
                    colors = "Set2",
                    fill = ~"",
                    # colors = viridis::viridis_pal(option = "A", begin = 0.08, end = 0.92)(dplyr::n_distinct(hover_df()$group_var)),
                    hoverinfo = "text",
                    text = ~combined_info,
                    marker = list(opacity = 0.5,
                                  # size = 12,
                                  showscale = FALSE,
                                  showlegend = FALSE,
                                  sizemode = "diameter",
                                  line = list(color = "black",
                                              width = 0.5)
                    )) %>%
      plotly::layout(
        dragmode = "lasso",
        shapes = list(hline(0, min_x = input$num_posts[1], max_x = 4*10^input$num_posts[2])),
        margin = list(b=100),
        xaxis = list( type = "log",
                      title = ~ifelse(input$xaxis == "avg_monthly_searches", "Average Number of Keyword Searches",
                                      # paste0(month.name[match(input$xaxis, tolower(month.abb))], " Number of Keyword Searches")),
                                      paste0(sapply(strsplit(input$xaxis, "_"), function(x) {
                                        month <- month.name[match(tolower(x[1]), tolower(month.abb))]
                                        paste0(substring(month, 1, 1), substring(month, 2), " ", x[2])
                                      }), " Number of Keyword Searches")),
                      range = c(x_min(), x_max()),
                      zeroline = FALSE, showline = TRUE, mirror = TRUE),
        yaxis = list(zeroline = FALSE,
                     ticksuffix = "%",
                     showline = TRUE, mirror = TRUE,
                     range = c(input$growth_range),
                     title = ifelse(input$yaxis == "three_month_change", "Three Month Growth", "Year-on-Year Growth")),
        showlegend = ~ifelse(all(group_var == 1), FALSE, TRUE),
        legend = list(title = list(
          text = "<b>Word(s) in search term:</b>",
          font = list(size = 12)
        ),
        itemsizing = "constant")) %>%
      plotly::add_annotations(text = "<span style='font-style:italic;text-decoration:underline;'>Note:</span><span style='font-style:italic;'> Bubble size corresponds to average monthly searches, a larger bubble represents a larger average monthly search</span>\n<span style='font-style:italic;'>*This is a logarithmic scale, values 1, 10, 100, 1000 etc. are equally spaced on the graph.\nA log scale means that that keywords with 0 posts will not be displayed as points on the graph</span>",
                              # "*",
                              xref = "paper", yref = "paper",
                              x = 0.5, y = -0.35, size = 3, showarrow = FALSE,
                              font = list(size = 10)) %>%
      plotly::config(
        displaylogo = FALSE,
        edits = list(
          shapePosition = TRUE,
          annotation = TRUE
        )
      )
    
  })
  
  x_min <- shiny::reactive({
    log10(10^input$num_posts[1])
  })
  
  x_max <- shiny::reactive({
    log10(10^input$num_posts[2])
  })
  
  display_table <- shiny::reactive({
    selected_indices <- df_highlighted()$customdata
    hover_df()[hover_df()$rowid %in% selected_indices, ] %>%
      dplyr::select("Keyword" = keyword, "Average Monthly Searches" = avg_monthly_searches,
                    "Three Month Growth" = three_month_change, "Year on Year Growth" = yo_y_change,
                    stats::setNames(colnames(data())[6:17],
                                    sapply(strsplit(colnames(data())[6:17], "_"), function(x) {
                                      month <- month.name[match(tolower(x[1]), tolower(month.abb))]
                                      paste0(substring(month, 1, 1), substring(month, 2), " ", x[2])
                                    })))
  })
  
  # Outputs ----

  output$filter_selected_tab1 <- DT::renderDataTable({
    display_table() %>%
      DT::datatable(options = list(scrollX=TRUE, scrollCollapse=TRUE))
  })
  
  output$filter_selected_tab2 <- DT::renderDataTable({
    display_table() %>%
      DT::datatable(options = list(scrollX=TRUE, scrollCollapse=TRUE))
  })
  
  output$keyword_dynamic_plot <- plotly::renderPlotly({
    if (length(input$static_keyword_callout) == 0){
      p <- basic_scatter()
      
    } else {
      
      p <- basic_scatter() %>%
        plotly::add_annotations(
          xref = "x",
          yref = "y",
          x = log10(label_df()$x), y = label_df()$y,
          text = label_df()$label,
          font = list(size = 8, color = "grey80"),
          axref = "x",
          ayref = "y",
          bgcolor = "rgba(255,255,255,0.6)",  # Background color of the box
          bordercolor = "rgba(0,0,0,0.6)",  # Border color of the box
          ay = ~label_df()$ay_col,
          ax = ~label_df()$ax_col,
          showarrow = TRUE,
          arrowhead = 2,
          standoff = 0.3,
          borderwidth = 0.5)
    }
    
    plotly::event_register(p, "plotly_selected")
    p
  })
  
  output$vot_plot <- plotly::renderPlotly({
    
    plotly::plot_ly(df_longer(), x = ~date, y = ~jitter(n), color = ~Keyword,
                    type = "scatter", mode = "markers + line",
                    hovertemplate = ~paste("<b>", Keyword, "</b><br>",
                                           "Month: %{x|%b}, Searches:", n, "<br><extra></extra>"),
                    colors = viridis::viridis_pal(option = "A", end = 0.92)(nrow(df_longer()))) %>%
      plotly::layout(xaxis = list(title = "Month"),
                     yaxis = list(title = "Number of Searches"))
  })
  
  output$data_download_tab1 <- shiny::downloadHandler(
    filename = function() {
      paste0("selected_data_", format(Sys.time(), "%d-%m-%Y_%H:%M:%S"), ".csv")
    },
    content = function(file) {
      utils::write.csv(display_table(), file, row.names = FALSE)
    }
  )
  
  output$data_download_tab2 <- shiny::downloadHandler(
    filename = function() {
      paste0("selected_data_", format(Sys.time(), "%d-%m-%Y_%H.%M.%S"), ".csv")
    },
    content = function(file) {
      utils::write.csv(display_table(), file, row.names = FALSE)
    }
  )
  
  
  # end ----
  
}
