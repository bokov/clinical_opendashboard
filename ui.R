library(shinyjs); library(shinyalert); library(shinyBS); library(plotly); 
library(DT);

options(shiny.maxRequestSize=50*1024^2);


shinyUI(fluidPage(
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico"))
  ,includeCSS('df.css'),useShinyjs(),useShinyalert()
  ,fluidRow(h3("CODEHR: Clinical Open Dashboard for Electronic Health Records")
              ,tags$em('Urology early adopter alpha edition'))
  ,mainPanel(
    width=12,fluidRow(
      column(2
             ,bsCollapse(id="filters",multiple=T #,open="Basic"
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
                                          ,sliderInput('slN','Count cutoff'
                                                       ,min=50,max=2000
                                                       ,value=300,step = 1)
                                          ,sliderInput('slOR'
                                                       ,'Odds ratio cutoff'
                                                       ,min=1.1,max=4,value=1.5
                                                       ,step=0.01)
                                          ,sliderInput('slChi'
                                                       ,'Chi-square cutoff'
                                                       ,min=0,max=1000
                                                       ,value=200)
                                          
                                          )
                         )
             ,actionButton('bupdate','Update')
             #,actionButton('bdebug','Debug')
             )
      ,column(10,textOutput('maintext'),br()
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