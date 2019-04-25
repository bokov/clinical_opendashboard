#' ---
#' title: "Chinotype Dashboard Prototype"
#' author: "Alex F. Bokov"
#' date: "April 24, 2019"
#' ---

# ---- Libraries ----
library(dplyr); library(ggplot2); library(scales);

source('functions.R');


# ---- Global variables ---- 
n_all <- 'AllUrolPatients'
n_groupnames <- c(n_all,'Hispanic','LowIncome');
dfiles <- c('ALL_HISPANIC.csv','ALL_LOWINCOME.csv');
#                          rename from, rename to
renamepatterns <- rbind(c('ODDS_RATIO','ODDSRATIO')
                        # if the left three columns in every data file
                        # are not named precisely 'PREFIX','CCD',and 'NAME'
                        # then each rename pattern should be added as a 
                        # separate row in this part of the script
                        );
totalcode <- 'TOTAL';
mincountfrac <- 0.01;

# ---- Default Arguments ----
formals(quickreshape)[c('groups','other')] <- list(n_groupnames[-1]
                                                   ,c('NAME','FRC_All'));
formals(chifilter)$groups <- n_groupnames[-1];
formals(selectcodegrps)$codemap <- demogcodes;

# ---- Read Data ----
if('cached_data.rdata' %in% list.files()){
  load('cached_data.rdata');}
if(!exists('dat')||!exists('dat_totals')){
  if(!all(dfiles %in% list.files())){
    stop('The input datafiles are missing. In addition to these scripts, you '
         ,'or whoever is responsible for deploying this webapp also needs to '
         ,'obtain data for it to process.')}
  dat <- read_chis(dfiles[1],dfiles[2],groupnames=n_groupnames
                   ,submulti = renamepatterns);
  dat_totals <- subset(dat,CCD==totalcode);
  dat <- subset(dat,dat[[n_all]]>mincountfrac*dat_totals[[n_all]]);
  save(dat,dat_totals,file='cached_data.rdata');
}
# ---- Test Filtering and Plotting ----
dat_test0 <- chifilter(dat);
#dat_test_plot0 <- quickpoints(dat_test0,groups=n_groupnames[-1]);
#print(dat_test_plot0);
dat_test1 <- left_join(demogcodes,dat);
#dat_test_plot1 <- quickpoints(dat_test1,groups = n_groupnames[-1]);
#print(dat_test_plot1);

# ---- Server ----
shinyServer(function(input, output, session) {
  # ---- Server init ----
  rv <- reactiveValues(rgroups=n_groupnames[-1]
                       ,rprefix='UTHSCSA|FINCLASS'
                       ,rshowcols=c('PREFIX','CCD','NAME',n_groupnames
                                    ,paste0('FRC_',n_groupnames))
                       ,rdat=selectcodegrps(dat,prefix='UTHSCSA|FINCLASS'));
  # ---- Update Button Clicked ----
  observeEvent(input$bupdate,{
    message('starting update button click');
    if(length(input$selBasic)==0){
      updateSelectInput(session,inputId='selBasic',selected=rv$rprefix)
      } else {
        rv$rprefix <- input$selBasic;
        message('updating rdat');
        rv$rdat <- selectcodegrps(dat,prefix=rv$rprefix);
      }
    message('update button click done');
  });
  # ---- Main Plot ---- 
  output$plotmain <- renderPlotly({
    message('About to render main plot');
    rcols <- setNames(brewer_pal(type='qua')(length(rv$rgroups)),rv$rgroups);
    rv$rcols <- rcols;
    ggplotly(quickbars(rv$rdat,groups=rv$rgroups) +
               ggtitle(paste0(gsub('^.*\\|','',rv$rprefix),collapse=', ')) +
               theme(axis.text.x=element_text(angle=30)
                     ,plot.margin = margin(30,30,60,40)),tooltip='text'
             )
    # ggplotly(quickplot(rv$rdat,groups=rv$rgroups,cols=rcols) +
    #         ggtitle(paste0(gsub('^.*\\|','',rv$rprefix),collapse=', ')) +
    #         xlab(paste('%',paste0(rv$rgroups,collapse=', '),'Patients')) +
    #         ylab(paste('%',n_all,'\n')),tooltip=c('text','name'));
  });
  # ---- Table of Selected Data ----
  output$tblsel <- renderDataTable(rv$rdat[,names(dat) %in% rv$rshowcols]);
  message('Done!')
});

c()
