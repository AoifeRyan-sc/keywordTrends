library(dplyr)
source("helper_functions.R")
# logifySlider javascript function ----
JS.logify <-
  "
// function to logify a shiny::sliderInput
function logifySlider (sliderId, sci = false) {
  if (sci) {
    // scientific style
    $('#'+sliderId).data('ionRangeSlider').update({
      'prettify': function (num) { return ('10<sup>'+num+'</sup>'); }
    })
  } else {
    // regular number style
    $('#'+sliderId).data('ionRangeSlider').update({
      'prettify': function (num) { return (Math.pow(10, num)).toFixed(0); }
    })
  }
}"

# call logifySlider for each relevant shiny::sliderInput ----
JS.onload <-
  "
// execute upon document loading
$(document).ready(function() {
  // wait a few ms to allow other scripts to execute
  setTimeout(function() {
    // include call for each slider
    // logifySlider('num_posts', sci = false)
    logifySlider('num_posts', sci = false)
  }, 5)})
"
JS.observeSliderChange <-
  "
// execute whenever slider input changes
$(document).on('change', '#data_upload', function() {
// wait a few ms to allow other scripts to execute
  setTimeout(function() {
    // include call for each slider
    // logifySlider('num_posts', sci = false)
    logifySlider('num_posts', sci = false);
}, 5)});
"
JS.sendCustomMessage <-
  "
$(document).on('shiny:connected', function(event) {
  Shiny.addCustomMessageHandler('jsCode', function(message) {
    eval(message.code);
  });
});
"
# ----
ui <- shiny::fluidPage(
  shiny::tags$head(shiny::tags$script(htmltools::HTML(JS.logify))),
  shiny::tags$head(shiny::tags$script(htmltools::HTML(JS.onload))),
  shiny::tags$head(shiny::tags$script(htmltools::HTML(JS.sendCustomMessage))),
  
  # Application title
  shiny::titlePanel("Explore Google Search Data"),
  
  # Sidebar with a slider input for number of bins 
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::fileInput("data_upload", "Upload your data", accept = c(".xlsx", ".csv", ".tsv"),
                       multiple = FALSE),
      shiny::selectInput("xaxis", "x-axis timeframe:",
                         choices = c("Average monthly searches" = "avg_monthly_searches", stats::setNames(paste0(tolower(month.abb)), month.name)),
                         selected = "avg_monthly_searches", multiple = FALSE),
      shiny::selectInput("yaxis", "Growth Metric:",
                         choices = c("Yearly growth" = "yo_y_change", "Three month growth" = "three_month_change"),
                         selected = "three_month_change", multiple = FALSE),
      shiny::selectizeInput("keyword", "Search Term",
                         choices = NULL, selected = NULL, multiple = TRUE),
      shiny::conditionalPanel(condition = "input.chart_type == 1",
                              shiny::sliderInput("num_posts", htmltools::div(htmltools::HTML("Number of Posts <i>(note log scale)</i>")),
                                                 min = 0.1, max = 3, step = 0.005, value = c(0.1,3), round = TRUE, tick = FALSE),
                              shiny::sliderInput("growth_range", "Growth (%)",
                                                 min = -1.5, max = 10, 
                                                 value = c(-1.5, 10), 
                                                 tick = FALSE)),
      shiny::selectInput("grouping_variable", "Grouping Variable",
                         choices = NULL, selected = NULL, multiple = TRUE),
      shiny::selectInput("static_keyword_callout", "Static Chart Callouts",
                         choices = NULL, selected = NULL, multiple = TRUE),
      shiny::actionButton("shuffle_label", "Shuffle Label Position")
    ),
    
    # Show a plot of the generated distribution
    shiny::mainPanel(
      shiny::tabsetPanel(type = "tabs",
                         shiny::tabPanel("Growth Chart",
                                         # absolutePanel(top = 10, right = 10, shiny::downloadButton("scatter_download")),
                                         plotly::plotlyOutput("keyword_dynamic_plot"),
                                         verbatimTextOutput("test"),
                                         DT::dataTableOutput("filter_selected_tab1"),
                                         shiny::downloadButton("data_download_tab1", label = "Download Data Table"),
                                         value = 1),
                         shiny::tabPanel("Volume Over Time",
                                         # absolutePanel(top = 10, right = 10, shiny::downloadButton("vot_download")),
                                         plotly::plotlyOutput("vot_plot"), 
                                         DT::dataTableOutput("filter_selected_tab2"),
                                         shiny::downloadButton("data_download_tab2",  label = "Download Data Table"), 
                                         value = 2),
                         id = "chart_type")
    )
  )
)
