library(shinyjs); library(shinyalert); library(shinyBS); library(plotly);

options(shiny.maxRequestSize=50*1024^2);


shinyUI(fluidPage(
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico"))
  ,includeCSS('df.css'),useShinyjs(),useShinyalert()
  ,fluidRow(h3("CODEHR: Clinical Open Dashboard for Electronic Health Records")
              ,tags$em('Urology early adopter alpha edition'))
  ,mainPanel(
    width=12,fluidRow(
      column(2
             ,bsCollapse(id="filters",open="Basic",multiple=F
                         ,bsCollapsePanel(title=span("Basic",icon('angle-down')) 
                                          ,value="Basic"
                                          ,span(span(id='hBasic'
                                                     ,hidden(icon('question-circle')))
                                                ,'Choose category of variables'
                                                ,' to compare.')
                                          ,selectInput('selBasic',''
                                                       ,selBasicChoices[-1]
                                                       ,selected = 'UTHSCSA|FINCLASS' 
                                                       #,multiple=T
                                                       )
                                          )
                         ,bsCollapsePanel(title=span("Advanced"
                                                     ,icon('angle-down'))
                                          ,"Coming soon."))
             ,actionButton('bupdate','Update')
             ,actionButton('bdebug','Debug')
             )
      ,column(10,textOutput('maintext')
              ,plotlyOutput('plotmain',width = '79vmin',height = '70vmin'))
      )
    ,fluidRow(bsCollapse(id="details"
                         ,bsCollapsePanel(span("Counts and percentages"
                                               ,icon('angle-down'))
                                          ,dataTableOutput('tblsel'))))
    )
  )
  );

c()