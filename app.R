library(shiny)
library(ggfittext)
library(ggplot2)

# Function definitions and static variables --------------------------------
source("make-diagram.R")



if(!dir.exists("www")) dir.create("www") #tilføjet selv, nødvendigt for at kunne gemme pdf'er
addResourcePath("www", "www")
outfile <- tempfile(tmpdir = "www", fileext = ".pdf")
license <- '<p xmlns:dct="http://purl.org/dc/terms/" xmlns:cc="http://creativecommons.org/ns#" class="license-text">This work is licensed under <a rel="license" href="https://creativecommons.org/licenses/by-nc-sa/4.0">CC BY-NC-SA 4.0<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1" /><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1" /><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/nc.svg?ref=chooser-v1" /><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/sa.svg?ref=chooser-v1"/></a></p>'
p0 <- paste0

anchor1 <- c(
  "Cohort entry" = "Cohort entry date",
  "Event"        = "ED"
)

anchor2 <- c(
  "Please choose" = "",
  "Follow-up"     = "Follow-up window",
  "Eligibility"   = "Eligibility assessment window",
  "Exclusion"     = "Exclusion assessment window",
  "Covariates"    = "Covariate assessment window",
  "Washout"       = "Washout window",
  "Exposure"      = "Exposure assessment window"
)

make_row_inputs <- function(x, atype = 1, s = 0, e = 0) {
  tagList(
    tags$div(
      id = paste0("row", x),
      fluidRow(
        column(1, tags$div(
          title = "Order of display (1 = Bottom)",
          numericInput(paste0("line", x), "Order", x, min = 1, max = 12)
        )),
        column(2, tags$div(
          title = "Label to be displayed on the first line of the box",
          selectInput(paste0("lbl1_", x), "Second-order anchor", anchor2, selected = anchor2[atype])
        )),
        column(1, tags$div(
          title = "Label to be displayed on the box (line two)",
          textInput(paste0("lbl2_", x), "(line 2)", "")
        )),
        column(1, tags$div(
          title = "Start of interval in days (also placement on the x-axis)",
          numericInput(paste0("start", x), "Start", s)
        )),
        column(1, tags$div(
          title = "End of interval in days (also placement on the x-axis)",
          numericInput(paste0("end", x), "End", e)
        )),
        column(2, tags$div(
          title = "Optional label for interval. When left blank start/end is displayed.",
          textInput(paste0("start_lbl", x), "Optional label (start)", "")
        )),
        column(1, tags$div(
          title = "Optional label for interval. When left blank start/end is displayed.",
          textInput(paste0("end_lbl", x), "(end)", "")
        )),
        column(1, tags$div(
          title = "Colors are chosen automatically based on numbers",
          numericInput(paste0("boxcolor", x), "Color", x, min = 1, max = 8)
        )),
        column(2, selectInput(paste0("outbox", x), "Label outside box?", c("No" = 0, "Yes, left" = 1, "Yes, right" = 2)))
      )
    )
  )
}

make_index_inputs <- function(x) {
  tagList(
    tags$div(
      id = paste0("index", x),
      fluidRow(
        column(4, tags$div(
          title = "First order anchor (label to be displayed above vertical line)",
          selectInput(paste0("indexlbl1_", x), "First-order anchors", anchor1)
        )),
        column(4, tags$div(
          title = "Label for vertical line (line two)",
          textInput(paste0("indexlbl2_", x), "Optional label", "")
        )),
        column(4, tags$div(
          title = "Day for placement of vertical line (typically event date or cohort entry)",
          numericInput(paste0("index", x), "Index date", 0)
        ))
      )
    )
  )
}

# APP STARTS HERE ----------------------------------------------------------

ui <- fluidPage(
  # Application title
  titlePanel(
    "Diagrams for longitudinal study designs"
   ),
  
  mainPanel(width = 12,
    fluidRow(
      br(),
      column(7,
        tabsetPanel(
          # ------------ Input indices -------------
          tabPanel("First-order anchors", 
            br(),
            p("(Hover over input boxes for help.)", style = "color:grey"),
            p(
              actionButton("vlines", "Add anchor"),
              actionButton("remline", "Remove anchor"),
              actionButton("clearv", "Remove all")
            ),
            make_index_inputs(1)
          ),
          # ------------ Rows input ---------------
          tabPanel("Second-order anchors", 
            br(),
            p("(Hover over input boxes for help.)", style = "color:grey"),
            p(
              actionButton("add", "Add anchor"),
              actionButton("rem", "Remove anchor"),
              actionButton("clearrows", "Remove all")
            ),
            make_row_inputs(1)
          ),
          tabPanel("Output",
            br(),
            p("Text size may differ slightly between .pdf output and the preview. Adjust text size if necessary.", style = "color:grey"),
            fluidRow(
              column(2, selectInput("aratio", "Aspect ratio", c("4:3" = 1, "16:9" = 2))),
              column(2, numericInput("textsize", "Text size", 14, min = 4, max = 32)),
              column(2, numericInput("xpad", "Padding (x-axis)", 50, min = 0, max = 300)),
              column(2, br(), actionButton("build", "Build .pdf"))
            ),
            htmlOutput("pdf")
          )
          
        )
      ),
      column(5,
        br(),
        br(),
        plotOutput("longdiag")
      )
    ),
    tags$div(
      "This application is inspired by the work of", 
      a("Schneeweiss et al.,", 
        href="http://annals.org/article.aspx?doi=10.7326/M18-3079"),
      "please consider citing the original article when publishing a diagram 
       generated by this application.\n",
      HTML(license)
    )
  )
)

server <- function(input, output, session) {
  
  rownums <- reactiveVal(1)
  vnums   <- reactiveVal(1)
  
# Boxes -------------------------------------------
  observeEvent(input$add, {
    insertUI(
      selector = paste0("#row", rownums()),
      where = "beforeBegin",
      ui = make_row_inputs(rownums() + 1)
    )
    
    rownums(rownums() + 1)
  })
  
  observeEvent(input$rem, {
    if (rownums() > 1) {
      removeUI(selector = paste0("#row", rownums()))
      rownums(rownums() - 1)
    }
  })
  
  observeEvent(input$clearrows, {
    for (i in rownums():2)
      removeUI(selector = paste0("#row", i))
    rownums(1)
  })
  
# Lines -------------------------------------------------
  observeEvent(input$vlines, {
    insertUI(
      selector = paste0("#index", vnums()),
      where = "beforeBegin",
      ui = make_index_inputs(vnums() + 1)
    )
    
    vnums(vnums() + 1)
  })
  
  observeEvent(input$remline, {
    if (vnums() + 1) {
      removeUI(selector = paste0("#index", vnums()))
      vnums(vnums() - 1)
    }
  })
  
  observeEvent(input$clearv, {
    for (i in vnums():2)
      removeUI(selector = paste0("#index", i))
    vnums(1)
  })
  
  drows <- debounce(reactive(1:rownums()), 500)
  
  ggdf <- reactive({
    data.frame(
      line      = vapply(drows(), function(x) input[[paste0("line",      x)]], 1),
      lbl       = vapply(drows(), function(x) input[[paste0("lbl1_",     x)]], "a"),
      lbl2      = vapply(drows(), function(x) input[[paste0("lbl2_",     x)]], "a"),
      outside_box    = sapply(drows(), function(x) input[[paste0("outbox",    x)]]),
      start     = vapply(drows(), function(x) input[[paste0("start",     x)]], 1),
      end       = vapply(drows(), function(x) input[[paste0("end",       x)]], 1),
      start_lbl = vapply(drows(), function(x) input[[paste0("start_lbl", x)]], "a"),
      end_lbl   = vapply(drows(), function(x) input[[paste0("end_lbl",   x)]], "a"),
      color     = vapply(drows(), function(x) input[[paste0("boxcolor",  x)]], 1),
      stringsAsFactors = FALSE
    )
  })
  
  irows <- debounce(reactive(1:vnums()), 500)
  
  indf <- reactive({
    my_indices <- data.frame(
      vlines      = vapply(irows(), function(x) input[[paste0("index", x)]], 1),
      indexlabel  = vapply(irows(), function(x) input[[paste0("indexlbl1_", x)]], "a"),
      indexlabel2 = vapply(irows(), function(x) input[[paste0("indexlbl2_", x)]], "a"),
      stringsAsFactors = FALSE
    )
  })
  
  plt <- reactive({
    gen_long_diag(ggdf(), 
                  indf(),
                  txt_size = input$textsize - 2,
                  addpad = input$xpad)
  })

  
  output$longdiag <- renderPlot(plt(), width = 700, height = 600*0.75)
  
  observeEvent(input$build, {
    
    if (input$aratio == 1) {
      wdth <- 16
      hght <- 12
    } else {
      wdth <- 16
      hght <- 9
    }
    
    plt_out <- gen_long_diag(ggdf(), 
                             indf(),
                             txt_size = input$textsize,
                             addpad = input$xpad)
    
    ggsave(outfile,
           plot = plt_out,
           width = wdth,
           height = hght,
           units = "in")
  
    output$pdf <- renderUI({
      tags$iframe(
        style = "height:800px; width:100%",
        src = outfile
      )
    })
  })
    
    session$onEnded(function() unlink(outfile))
}

# Run the application 
shinyApp(ui = ui, server = server)

