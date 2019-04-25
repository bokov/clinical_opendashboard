library(shinyjs); library(shinyalert); library(shinyBS); library(plotly);

options(shiny.maxRequestSize=50*1024^2);


shinyUI(fluidPage(
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico"))
  ,includeCSS('df.css'),useShinyjs(),useShinyalert()
  ,navbarPage("CODEHR: Clinical Open Dashboard for Electronic Health Records"
              ,div(id="sidebar"
                   ,sidebarPanel(
                     bsCollapse(
                       id="filters",open="Basic",multiple=F
                       ,bsCollapsePanel("Basic" #,style='info'
                                        ,span(id='hBasic'
                                              ,icon('question-circle'))
                                        ,'Compare variables using basic filters.'
                                        ,selectInput(
                                          'selBasic',''
                                          ,unique(demogcodes[[1]])[-1]
                                          , selected = tail(demogcodes[[1]],1)
                                          #,multiple=T
                                          )
                                )
                       ,bsCollapsePanel("Advanced",style='primary'
                                                 ,"Coming soon."))
                     ,actionButton('bupdate','Update')
                     ,actionButton('bsave','Save')
                   )
                   )
              ,mainPanel('Main Panel'
                         ,plotlyOutput('plotmain')
                         ,bsCollapse(id="details"
                                     ,bsCollapsePanel(
                                       "Selected Variables"
                                       ,dataTableOutput('tblsel')))
                         )
              )
  ));

c()