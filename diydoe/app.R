#
library(shiny)
library(rhandsontable)
library(AlgDesign)
#library(rstanarm)
library(plotly)
library(reshape2)
library(glmnet)
set.seed(8675309)

ui <- shinyUI(
  navbarPage(
    "DoE Designer Lite [Alpha Version]",
    tabPanel(
      "1. Design DoE",
      sidebarPanel(
        p(
          "Select the number of continuous factors and ranges,
          the design assumes one response of interest,
          select the influence that is expected."
        ),
        numericInput(
          'n_x',
          label = p('Number Continuous Factors'),
          value = 3
        ),
        numericInput('n_y', label = p('Number Responses'), value = 1),
        sliderInput(
          "n_expts",
          "Number of experiments:",
          min = 6,
          max = 30,
          value = 8
        ),
        sliderInput(
          "n_centers",
          "Number of additional centerpoints:",
          min = 0,
          max = 4,
          value = 4
        )
        ),
      mainPanel(
        h4('Factors and Levels:'),
        rHandsontableOutput('table'),
        br(),
        downloadButton('downloadData', 'Download Table of Conditions to Run')
      )
    ),
    
    tabPanel(
      "2. Execute DoE",
      h4('Enter experimental info here:'),
      #dataTableOutput('expts'),
      rHandsontableOutput('rht_expts'),
      h4('OR Upload file with simple table'),
      fileInput('rht_file', "Choose CSV or XLSX File",
                accept = c('.csv', '.xlsx'))
    ),
    
    tabPanel(
      "3. Visualize DoE",
      sidebarPanel(uiOutput('color_by')),
      mainPanel(plotlyOutput("viz.plot"))
    ),
    tabPanel(
      "4. Analyze DoE",
      mainPanel(
        plotlyOutput("ana.plot"),
        br(),
        h4('Model Statistics'),
        verbatimTextOutput("mod.summary"),
        br(),
        h4('Predicted Performance'),
        plotlyOutput("contour.plot")
      )
    )
)
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  output$color_by <- renderUI({
    names <- names(hot_to_r(input$rht_expts))
    tagList(
      selectInput(
        'var.x',
        'select x-axis',
        choices = names,
        selected = names[1]
      ),
      selectInput(
        'var.y',
        'select y-axis',
        choices = names,
        selected = names[-1]
      ),
      selectInput(
        'var.c',
        'select color',
        choices = names,
        selected = names[2]
      )
    )
  })
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x,
         breaks = bins,
         col = 'darkgray',
         border = 'white')
  })
  
  df <- reactive({
    n <- input$n_x
    data.frame(
      Name = paste0('X', seq(n)),
      Min = rep(-1, n),
      Center = rep(0, n),
      Max = rep(1, n)
    )
  })
  
  output$table = renderRHandsontable(
    rhandsontable(df(), selectCallback = TRUE, readOnly = FALSE) %>%
      hot_col("Name", allowInvalid = TRUE)
  )
  
  doe_expts <- reactive({
    nx <- input$n_expts
    table_data <- hot_to_r(input$table)
    
    lvls <- table_data
    conditions <- lvls[, -1]
    names(conditions) <- c('-1', '0', '1')
    row.names(conditions) <- lvls$Name
    
    ff = gen.factorial(3, nrow(lvls), varNames = lvls$Name)
    
    #print(input$n_expts)
    des <- optFederov( ~ ., ff, nx)
    
    # add centerpoints
    centers = data.frame(matrix(rep(0, input$n_centers * nrow(lvls)), ncol = nrow(lvls)))
    names(centers) <- lvls$Name
    doe_expts <- rbind(des$design, centers)
    
    for (i in seq(ncol(doe_expts))) {
      doe_expts[, i] <-
        as.numeric(conditions[i, match(round(doe_expts[, i]), as.numeric(names(conditions)))])
    }
    row.names(doe_expts) <- NULL
    doe_expts
  })
  
  # visualize data
  output$viz.plot <- renderPlotly({
    dat <- hot_to_r(input$rht_expts)
    if (!is.null(input$var.x)) {
      plot_ly(
        dat,
        x = as.formula(paste0("~", input$var.x)),
        y = as.formula(paste0("~", input$var.y)),
        type = 'scatter',
        mode = 'markers',
        marker = list(color = as.formula(paste0(
          "~", input$var.c
        )),
        size = 20, opacity = 0.5, showscale = TRUE)
      ) %>%
        add_markers() %>% layout(showlegend = FALSE)
    } else{
      plot_ly(data.frame(), type = 'scatter', mode = 'markers')
    }
  })
  
  # make model- use lasso for regularization
  mod <- reactive({
    expts <- hot_to_r(input$rht_expts)
    X <- model.matrix(as.formula(paste0(
      input$var.y,
      '~quad(',
      paste0(names(expts[names(expts) !=
                           input$var.y]), collapse = ','),
      ')'
    )), expts)
    X <- X[, -1]
    lasso.mod <-
      cv.glmnet(
        X,
        expts[, input$var.y],
        family = 'gaussian',
        nfolds = input$n_expts,
        grouped = FALSE
      )
    #print(coef(lasso.mod,s='lambda.min'))
    features <-
      dimnames(coef(lasso.mod, s = 'lambda.min'))[[1]][coef(lasso.mod, s = 'lambda.min')@i +
                                                         1]
    #print(paste0(input$var.y,'~',paste0(features[-1],collapse='+')))
    mod <-
      lm(as.formula(paste0(
        input$var.y, '~', paste0(features[-1], collapse = '+')
      )), data = expts)
    return(mod)
  })
  
  
  output$mod.summary <- renderPrint({
    #print(str(mod()))
    m <- mod()
    #print(m)
    return(print(summary(m)))
  })
  
  output$ana.plot <- renderPlotly({
    # renders model parity plotting
    expts <- hot_to_r(input$rht_expts)
    if (var(expts[input$var.y]) > 0) {
      model <- mod()
      expts$pred <- model$fitted.values
      plot_ly(
        expts,
        x = as.formula(paste0("~", input$var.y)),
        y = as.formula(paste0("~pred")),
        type = 'scatter',
        mode = 'markers',
        hoverinfo = expts$pred,
        marker = list(color = as.formula(paste0(
          "~", input$var.c
        )),
        showscale = TRUE,
        size=10)
      ) %>%
        add_markers() %>% layout(showlegend = FALSE)
    }
  })
  
  output$contour.plot <- renderPlotly({
    # plot margins
    m <- list(
      l = 50,
      r = 50,
      b = 100,
      t = 100,
      pad = 4
    )
    # renders model contours
    expts <- hot_to_r(input$rht_expts)
    if (var(expts[input$var.y]) > 0) {
      model <- mod()
      #print(model)
      grid <- expand.grid(lapply(expts,
                                 function(x) {
                                   seq(min(x), max(x), length.out = 10)
                                 }))
      #print(head(grid))
      pts <-
        cbind(grid, data.frame(predict(model, grid, interval = 'prediction')))
      pts$var <-
        (max(abs(pts$upr - pts$lwr))) / ((abs(pts$upr - pts$lwr))) * 100
      #print(head(pts))
      k = acast(pts, as.formula(paste0(input$var.x, '~', input$var.c)), mean, value.var = 'fit')
      kl = acast(pts, as.formula(paste0(input$var.x, '~', input$var.c)), min, value.var = 'lwr')
      ku = acast(pts, as.formula(paste0(input$var.x, '~', input$var.c)), max, value.var = 'upr')
      #pts.mat <- acast()
      
      plot_ly(x = dimnames(k)[[1]], y = dimnames(k)[[2]]) %>% add_surface(z =
                                                                            k, showscale = FALSE) %>%
        add_surface(z = kl,
                    opacity = 0.66,
                    showscale = FALSE) %>% add_surface(z = ku,
                                                       opacity = 0.66,
                                                       showscale = FALSE) %>%
        add_markers(
          data = expts,
          x = as.formula(paste0('~', input$var.c)),
          y = as.formula(paste0('~', input$var.x)),
          z = as.formula(paste0('~', input$var.y)),
          mode = "markers",
          type = "scatter",
          marker = list(
            size = 10,
            color = "black",
            opacity=0.66,
            symbol = 104
          )
        ) %>%
        layout(
          title = "Surface",
          scene = list(
            xaxis = list(title = input$var.x),
            yaxis = list(title = input$var.c),
            zaxis = list(title = input$var.y),
            showlegend = FALSE
          )
        ) %>% layout(scene = list(aspectratio = list(
          x = 1, y = 1, z = 0.7
        )))
    }
  })
  
  output$expts = renderDataTable({
    doe_expts()
  })
  
  output$rht_expts = renderRHandsontable({
    expts <- doe_expts()
    # create dummy data here:
    y <-
      rnorm(expts[, 1], sd = 0.1) - expts[, 1] - expts[, 1] * expts[, 2] - 2 *
      expts[, 1] ^ 2 - expts[, 2] ^ 2
    rhandsontable(cbind(expts, y),
                  selectCallback = TRUE,
                  readOnly = FALSE)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('Design_', Sys.Date(), '.csv', sep = '')
    },
    content = function(file) {
      write.csv(doe_expts(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)