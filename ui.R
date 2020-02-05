library(shinyjs); library(shinyalert); library(shinyBS); library(plotly); 
library(DT);

options(shiny.maxRequestSize=50*1024^2);
# apparently in some environments loading this from global.R doesn't reach here
# or doesn't reach here immediately
#if(file.exists('project_uitext.R')) source('project_uitext.R');

shinyUI(fluidPage(
  shiny::tags$head(shiny::tags$link(rel="shortcut icon", href="favicon.ico"))
  ,includeCSS('df.css'),useShinyjs(),useShinyalert()
  #,tags$script(src = "codehr.js")
  ,fluidRow(h3("CODEHR: Clinical Open Dashboard for Electronic Health Records")
            ,em('A free, open-source webapp made possible by support from'
                ,'NIH/NCATS UL1TR001120 (IIMS) and the'
                ,'Long School of Medicine KL2 Award.'
                ,'Source code available on',a('GitHub',href=gitlink
                                              ,target='_blank'))
            # customized via project_uitext.R file
            ,br(),uiOutput('subtitle',inline = TRUE))
  ,mainPanel(
    width=12,fluidRow(
      column(2,id='leftcol'
             ,bsCollapse(id="filters",multiple=T #,open="Basic"
                         ,bsCollapsePanel(title=span("Basic",icon('angle-down')) 
                                          ,value="Basic"
                                          ,span(span(id='hBasic'
                                                     ,hidden(icon('question-circle')))
                                                ,'Choose category of variables'
                                                ,' to compare.')
                                          ,selectInput('selBasic',''
                                                       ,choices=c()
                                                       #,selBasicChoices
                                                       #,selected = selBasicDefault
                                                       #,multiple=T
                                                       )
                                          )
                         ,bsCollapsePanel(title=span("Advanced"
                                                      ,icon('angle-down'))
                                          ,value="Advanced"
                                          ,sliderInput('slN','Count cutoff'
                                                       ,min=10,max=2000
                                                       ,value=slidevals$N
                                                       ,step = 1)
                                          ,hr()
                                          ,sliderInput('slOR'
                                                       ,'Odds ratio cutoff'
                                                       ,min=1,max=4
                                                       ,value=slidevals$OR
                                                       ,step=0.01)
                                          ,hr()
                                          ,numericInput('slChi'
                                                        ,'False Discovery Rate'
                                                        ,min=0
                                                        ,max=1
                                                        ,step=0.01
                                                        ,value=slidevals$Chi)
                                          # ,sliderInput('slChi'
                                          #              ,'Chi square cutoff'
                                          #              ,min=.Machine$double.eps
                                          #              #,max=2000
                                          #              ,max=0.3
                                          #              ,step=0.00001
                                          #              ,value=slidevals$Chi)
                                          ,hr()
                                          ,actionButton('breset'
                                                        ,'Reset Sliders')
                                          )
                         )
             ,hidden(actionButton('bupdate',tagList('Update plot',br()
                                                    ,'and counts')))
             #,if(file.exists('.debug')) actionButton('bdebug','Debug') else c()
             ,uiOutput('uidebug')
             )
      ,column(9,textOutput('maintext'),br()
              ,plotlyOutput('plotmain',width = '79vmin',height = '70vmin'))
      )
    ,fluidRow(bsCollapse(id="details"
                         ,bsCollapsePanel(span("Counts and percentages"
                                               ,icon('angle-down'))
                                          ,dataTableOutput('tblsel'))))
    ,uiOutput('uidebuginfo')
    )
  )
  );

c()