# multiple correlations
library(shiny)
library(colourpicker)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Correlation: multiple measures"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(

      # Data input
      textAreaInput("myData", "DATA", "", width = 200, height = 200, placeholder = "[Paste, from spreadsheet, 2+ columns of data with non-number labels in top row]"),

      # Hacks
      tags$style("input[type='checkbox']:checked+span{font-weight:bold;}"), # hack to get checkboxes to show up bold when unchecked
      tags$style("input[type='checkbox']:not(:checked)+span{font-weight:bold;}"), # hack to get checkboxes to show up bold when unchecked
      tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), # hack to remove minor ticks on sliders
      
      # Options to select from
      selectInput(inputId="options", label="OPTIONS:",
                  choices=c("*** select ***" = "select",
                            "Manage data point visibility" = "dotvisibility",
                            "Transform data" = "perc",
                            "Show stats/data/colors" = "subplotcontents",
                            "Fit lines/curves" = "fit",
                            "Tweak axes" = "axes",
                            "Adjust labels & plot size" = "labels"),
                  selected = NULL),
      
      # Manage data point visibility
      conditionalPanel(condition="input.options=='dotvisibility'",
                       radioButtons("dottype", label = "type", choiceNames = list("ring", "dot"), choiceValues = list("1","16")),
                       sliderInput(inputId = "dotsize",
                                   label = "size",
                                   min = 1,
                                   max = 100,
                                   value = 30),
                       sliderInput(inputId = "jitter_perc",
                                   label = "jitter",
                                   min = 0,
                                   max = 100,
                                   value = 0),
                       sliderInput(inputId = "dotopacity",
                                   label = "opacity",
                                   min = 0,
                                   max = 100,
                                   value = 100)
      ),
      
      # Transform data
      conditionalPanel(condition="input.options=='perc'",
                  checkboxInput('spearman', 'percentile ranks', FALSE)
      ),
      

      # Show stats/data/colors
      conditionalPanel(condition="input.options=='subplotcontents'",
      radioButtons("upper", label = "upper/right plots", choices = list("stats", "data", "neither"), selected="data"),
      radioButtons("lower", label = "lower/left plots", choices = list("stats", "data", "neither"), selected="neither"),
      
      checkboxInput('tint', 'color by strength of correlation', FALSE),
      conditionalPanel(condition="input.tint",
                  colourInput(inputId="color1", label=NULL, value = "purple", showColour = c("both"), palette = c("square"), allowedCols = NULL, allowTransparent = TRUE, returnName = TRUE),
                  checkboxInput('tint2', 'separate color for negative values', FALSE),
                  conditionalPanel(condition="input.tint2",
                        colourInput(inputId="color2", label=NULL, value = "royalblue", showColour = c("both"), palette = c("square"), allowedCols = NULL, allowTransparent = TRUE, returnName = TRUE))
      ),
      
      conditionalPanel(condition="input.upper=='stats' | input.lower=='stats'",
                       checkboxInput('showp', 'replace n with p', FALSE)     
      )
      ),
      
      # Fit lines/curves
      conditionalPanel(condition="input.options=='fit'",
      sliderInput(inputId = "lw",
                  label = "line widths",
                  min = 0,
                  max = 100,
                  value = 25),
      radioButtons("fit", label = "fit", choices = list("none", "line", "curve", "both"), selected="line"),
      sliderInput(inputId = "smoothness",
                  label = "curve smoothness",
                  min = 0,
                  max = 100,
                  value = 50), 
      sliderInput(inputId = "digits",
                  label = "statistics digits",
                  min = 2,
                  max = 5,
                  value = 2)
      ),

      # Tweak axes
      conditionalPanel(condition="input.options=='axes'",
      checkboxInput('equateaxes', 'equate all axis ranges', FALSE),
      conditionalPanel(condition="input.equateaxes",
      textInput("axisranges", label = "enter specific axis ranges", value = "", width = "50%", placeholder = "min,max")
      )
      ),
      
      # Adjust labels and plot size
      conditionalPanel(condition="input.options=='labels'",
                       textInput("graphtitle", label = "title", width = "75%", placeholder = "Title"), 
                       textAreaInput("variablelabels", label = "variable labels", value = "", width = "100%", rows = "2", placeholder = "v1,v2,v3... (use [return] to split a label)"),
                       sliderInput(inputId = "ticklabelsize",
                                   label = "axis number size",
                                   min = 0,
                                   max = 100,
                                   value = 30),
                       sliderInput(inputId = "plotsize",
                                   label = "plot size",
                                   min = 0,
                                   max = 200,
                                   value = 100)
      )
      
      ),
    # Main panel
    mainPanel(
      uiOutput('ui_plot'),
      hr(style = "margin: 0px 30px 10px 30px; border: .5px solid #a6a6a6"),
      downloadButton(outputId = "down", label = "Download graph as..."),
      radioButtons("filetype", label = NULL, choices = list("pdf", "png")),
      hr(style = "margin: 0px 30px 10px 30px; border: .5px solid #a6a6a6"),
      tags$h6("Notes..."),
      tags$h6("1. Line slope: by default, the physical slope of a fitted least-squares line equals the correlation (r or rho) value due to x and y axis ranges being chosen to span an equal number of standard deviations."),
      tags$h6("2. Curve: fit via R's smooth.spline function, with smoothness set via 'spar' argument."),
      tags$h6("3. Primacy of non-jittered data: all lines/curves are fit, and all stats and residuals are computed, using non-jittered data; since shown residuals follow jittered data, they do not exactly touch the line/curve."),
      tags$h6("4. Jitter units: for raw data, unit is percentage of smallest distance between two dots, calculated separately for each variable; for ranked data, unit is percentage of 10 percentile units; each point is randomly jittered over a range equal to this unit."),
      tags$style(type="text/css",
                  ".shiny-output-error { visibility: hidden; }",
                  ".shiny-output-error:before { visibility: hidden; }")
    )
  )
))