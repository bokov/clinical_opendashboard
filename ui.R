library(shinyjs); library(shinyalert); library(shinyBS);

options(shiny.maxRequestSize=50*1024^2);


shinyUI(fluidPage(
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico"))
  ,includeCSS('df.css'),useShinyjs(),useShinyalert()
  ,navbarPage("Open Clinical Dashboard"
              ,div(id="sidebar"
                   ,sidebarPanel(
                     bsCollapse(id="filters",open="basic",multiple=F
                                ,bsCollapsePanel("basic",style='info'
                                                 ,"Basic filters")
                                ,bsCollapsePanel("advanced",style='primary'
                                                 ,"Advanced filters"))
                     ,actionButton('bupdate','Update')
                     ,actionButton('bsave','Save')
                   )
                   )
              ,mainPanel('Main Panel'
                         ,plotOutput('plotmain')
                         ,bsCollapse(id="details"
                                     ,bsCollapsePanel("filtered",'Coming soon.'))
                         )
              )
  ));

c()