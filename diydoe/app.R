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
          "n_centers",
          "Number of additional centerpoints:",
          min = 0,
          max = 4,
          value = 4
        ),
        radioButtons("model.choice", label = p("Model Responses"),
                     choices = list("Main Effects" = '~.', "Interactions" = '~.^2', "Quadratic" = '~quad(.)'), 
                     selected = '~.'
        ),
        uiOutput('uiOutpt')),
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
      p('y columns have been pre-filled with random number placeholders'),
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
        uiOutput('select.selection'),
        uiOutput('analyze.selection'),width = 12)
    ), id = 'inNavbar'
)
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  min_expts <- reactive({
    if (input$model.choice=='~.'){
      minx=input$n_x+1+input$n_centers
    }else if(input$model.choice=='~.^2'){
      minx=choose(input$n_x,2)+input$n_x+1+input$n_centers
    }else if(input$model.choice=='~quad(.)'){
      minx=choose(input$n_x,2)+input$n_x*2+1+input$n_centers
    }

    minx
  })
  
  output$uiOutpt <- renderUI({
    sliderInput(
      "n_expts",
      "Total number of experiments:",
      min = min_expts(),
      max = 30,
      value = 8
    )
        })
  
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
  
  output$select.selection <- renderUI({

    names <- names(hot_to_r(input$rht_expts))
        selectInput(
          'var.response',
          'select response',
          choices = names[seq(input$n_x+1,length(names))],
          selected = names[length(names)]
        )
  })
  
  
  observeEvent(input$var.response,{
    output$analyze.selection <- renderUI({
    #expts <- hot_to_r(input$rht_expts)
    #print(paste0(
    #  input$var.response,
    #  '~',paste0(names(expts)[seq(input$n_x)], collapse = '+')))
    expts <- hot_to_r(input$rht_expts)
    if (!is.null(input$var.response)){
      mainPanel(
        br(),
        plotlyOutput("ana.plot"),
        br(),
        h4('Model Statistics'),
        verbatimTextOutput("mod.summary"),
        br(),
        h4('Predicted Performance'),
        plotlyOutput("contour.plot"))
    }
    })
  })# observe
    
    observeEvent(input$inNavbar,{
      output$analyze.selection <- renderUI({
          mainPanel()
      })
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
    nx <- input$n_expts-input$n_centers
    table_data <- hot_to_r(input$table)
    
    lvls <- table_data
    conditions <- lvls[, -1]
    names(conditions) <- c('-1', '0', '1')
    row.names(conditions) <- lvls$Name
    
    ff = gen.factorial(3, nrow(lvls), varNames = lvls$Name)
    
    #print(input$n_expts)
    des <- optFederov( as.formula(input$model.choice), ff, nx)
    
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
     if (input$model.choice=='~.'){
      X <- model.matrix(as.formula(paste0(
        input$var.response,
        '~',paste0(c('1',names(expts)[seq(input$n_x)]), collapse = '+'))), expts)
    }else if(input$model.choice=='~.^2'){
      X <- model.matrix(as.formula(paste0(
        input$var.response,
        '~(',paste0(names(expts)[seq(input$n_x)], collapse = '+'),')^2')), expts)
    }else if(input$model.choice=='~quad(.)'){
      X <- model.matrix(as.formula(paste0(
        input$var.response,
        '~quad(',paste0(names(expts)[seq(input$n_x)], collapse = ','),')')), expts)
    }
    
    
    #X <- X[, -1]
    #print(input$n_y)
    #print(paste0(input$var.response,'~quad(',paste0(names(expts)[seq(input$n_x)], collapse = ','),')'))
    #print(X)
    lasso.mod <-
      cv.glmnet(
        X,
        expts[, input$var.response],
        family = 'gaussian',
        nfolds = input$n_expts,
        grouped = FALSE
      )
    #print(coef(lasso.mod,s='lambda.min'))
    features <-
      dimnames(coef(lasso.mod, s = 'lambda.min'))[[1]][coef(lasso.mod, s = 'lambda.min')@i +
                                                         1]
    final.formula <- paste0(input$var.response,'~',paste0(c('1',features[-1]),collapse='+'))
    #print(final.formula)
    mod <-
      lm(as.formula(final.formula), data = expts)
    return(list(final.formula,mod))
  })
  
  
  output$mod.summary <- renderPrint({
    #print(str(mod()))
    m <- mod()
    #print(m)
    print(m[[1]])
    print(summary(m[[2]]))
  })

  output$ana.plot <- renderPlotly({
    # renders model parity plotting
    expts <- hot_to_r(input$rht_expts)
    if (var(expts[input$var.response]) > 0) {
      model <- mod()[[2]]
      expts$pred <- model$fitted.values
      plot_ly(
        expts,
        x = as.formula(paste0("~", input$var.response)),
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
    if (var(expts[input$var.response]) > 0) {
      m <- mod()
      model <- m[[2]]
      final.formula <- m[[1]]
      # TODO: figure out a way to extract the relevant factors 
      #predvars <- unique(c(names(attr(attr(model$model,'terms'),'dataClasses'))[-1],input$var.c,input$var.x))
      predvars <- names(expts)[1:input$n_x]
      #print(predvars)
      grid <- expand.grid(lapply(expts[(predvars)],
                                 function(x) {
                                   seq(min(x,na.rm=T), max(x,na.rm = T), length.out = 10)
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
      
      plot_ly(x = dimnames(k)[[1]], y = dimnames(k)[[2]]) %>% add_surface(z =k, showscale = FALSE) %>%
        add_surface(z = kl,
                    opacity = 0.66,
                    showscale = FALSE) %>% add_surface(z = ku,
                                                       opacity = 0.66,
                                                       showscale = FALSE) %>%
        add_markers(
          data = expts,
          x = as.formula(paste0('~', input$var.c)),
          y = as.formula(paste0('~', input$var.x)),
          z = as.formula(paste0('~', input$var.response)),
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
    y0 <- list(expts[, 1] ,
              expts[, 1] - expts[, 1] * expts[, 2] ,
              expts[, 1] - expts[, 1] * expts[, 2] - 2 *
      expts[, 1] ^ 2 - expts[, 2] ^ 2)
    
    doexy = expts
    for (i in seq(input$n_y)){
      y <- rnorm(nrow(expts), sd = 0.1*i) - y0[[i%%3+1]]
      doexy = cbind(doexy,y)
      names(doexy)[ncol(doexy)] = paste0('y',i)
    }
    #print(doexy)
    rhandsontable(doexy,
                  selectCallback = TRUE,
                  readOnly = FALSE)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('Design_', Sys.Date(), '.csv', sep = '')
    },
    content = function(file) {
      write.csv(doe_expts( ), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)