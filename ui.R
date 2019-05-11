library(shinyjs); library(shinyalert); library(shinyBS); library(plotly); 
library(DT);

options(shiny.maxRequestSize=50*1024^2);
# apparently in some environments loading this from global.R doesn't reach here
# or doesn't reach here immediately
if(file.exists('project_uitext.R')) source('project_uitext.R');

shinyUI(fluidPage(
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico"))
  ,includeCSS('df.css'),useShinyjs(),useShinyalert()
  ,fluidRow(h3("CODEHR: Clinical Open Dashboard for Electronic Health Records")
              ,tags$em(txtPageSubtitle))
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
                                                       ,selected = selBasicDefault
                                                       #,multiple=T
                                                       )
                                          )
                         ,bsCollapsePanel(title=span("Advanced"
                                                      ,icon('angle-down'))
                                          ,sliderInput('slN','Count cutoff'
                                                       ,min=50,max=2000
                                                       ,value=slidevals$N
                                                       ,step = 1)
                                          ,sliderInput('slOR'
                                                       ,'Odds ratio cutoff'
                                                       ,min=1.1,max=4
                                                       ,value=slidevals$OR
                                                       ,step=0.01)
                                          ,sliderInput('slChi'
                                                       ,'Chi square cutoff'
                                                       ,min=0
                                                       ,max=2000
                                                       #,max=1
                                                       ,value=slidevals$Chi)
                                          ,actionButton('breset'
                                                        ,'Reset Sliders')
                                          )
                         )
             ,actionButton('bupdate','Update')
             ,if(file.exists('.debug')) actionButton('bdebug','Debug') else c()
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