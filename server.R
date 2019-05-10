#' ---
#' title: "Chinotype Dashboard Prototype"
#' author: "Alex F. Bokov"
#' date: "April 24, 2019"
#' ---

# ---- Libraries ----
library(dplyr); library(ggplot2); library(scales); #library(DT);

source('functions.R');


# ---- Global variables ---- 
#dfiles <- 'ALL_HISPANIC.csv';
dfiles <- c('ALL_HISPANIC.csv','ALL_LOWINCOME.csv') #,'foo.csv');
#                          rename from, rename to
chirename <- rbind(c('ODDS_RATIO','OR')
                        # if the left three columns in every data file
                        # are not named precisely 'PREFIX','CCD',and 'NAME'
                        # then each rename pattern should be added as a 
                        # separate row in this part of the script
                        );

totalcode <- 'TOTAL';
mincountfrac <- 0.01;

renameforplots <- rbind(
  c('REF','Reference Population')
  ,c('HISPANIC','Hispanic')
  ,c('LOWINCOME','Low Income')
);

# inherit the reference population from above
refgroupname <- renameforplots[1,2];

# give deployer of this app the option to override any of the above by creating
# a script named 'project_custom.R'
if(file.exists('project_custom.R')) source('project_custom.R',local = T);

# ---- Default Arguments ----
#formals(quickreshape)[c('groups','other')] <- list(n_groupnames[-1]
                                                   # ,c('Category','NAME'
                                                   #    ,paste0('FRC_',n_all)));
#formals(chifilter)[c('groups','sortby')] <- list(n_groupnames[-1],n_all);
formals(selectcodegrps)$codemap <- demogcodes;
formals(quickbars)[c('searchrep')] <- list(renameforplots);
formals(quickpoints)[c('refgroupname','searchrep')] <- list(refgroupname
                                                            ,renameforplots);
formals(read_chis)$searchrep <- chirename;
# ---- Read Data ----
if('cached_data.rdata' %in% list.files()){
  load('cached_data.rdata');}
if(!exists('dat')||!exists('dat_totals')){
  if(!all(dfiles %in% list.files())){
    stop('The input datafiles are missing. In addition to these scripts, you '
         ,'or whoever is responsible for deploying this webapp also needs to '
         ,'obtain data for it to process.')}
  raw <- if(length(dfiles)>1) Reduce(read_chis,dfiles) else {
    standardize_chis(dfiles)};
  dat_totals <- subset(raw,CCD==totalcode);
  dat <- subset(raw,raw$N_REF>mincountfrac*dat_totals$N_REF);
  attr(dat,'sectioncols') <- attr(raw,'sectioncols');
  save(dat,dat_totals,file='cached_data.rdata');
}
# adapt slidevals sample size default based on smallest cohort size
slidevals$N <- round(min(dat_totals[,grep('^N_',names(dat_totals))]) * 
                       mincountfrac);
# ---- Test Filtering and Plotting ----
message('Running test0');
dat_test0 <- chifilter(dat);
#dat_test_plot0 <- quickpoints(dat_test0,groups=n_groupnames[-1]);
#print(dat_test_plot0);
message('Running test1');
dat_test1 <- left_join(demogcodes,dat);
#dat_test_plot1 <- quickpoints(dat_test1,groups = n_groupnames[-1]);
#print(dat_test_plot1);

# ---- Server ----
message('Defining shinyServer');
shinyServer(function(input, output, session) {
  # ---- Server init ----
  rv <- reactiveValues(rprefix='UTHSCSA|FINCLASS'
                       ,rshowcols=c('Category','NAME'
                                    ,grep('^(N_|FRC_)',names(dat),val=T))
                       ,rdat=selectcodegrps(dat,prefix='UTHSCSA|FINCLASS')
                       ,rchicut=slidevals$Chi
                       ,rncut=slidevals$N,roddscut=slidevals$OR);
  message('One-time clicking bupdate on init...');
  click('breset');
  # ---- Reset ----
  # allow the user to reset the sliders to their starting values
  observeEvent(input$breset,{
    message('Processing reset click');
    updateSliderInput(session,inputId='slN',value=slidevals$N);
    updateSliderInput(session,inputId='slChi',value=slidevals$Chi);
    updateSliderInput(session,inputId='slOR',value=slidevals$OR);
    message('Clicking update');
    # Don't know why, but have to programatically click bupdate twice 
    # to trigger it
    click('bupdate'); click('bupdate');
    message('Done with reset click');
  });
  # ---- Update Button Clicked ----
  observeEvent({input$bupdate},{
    message('starting update button click');
    if(length(input$selBasic)==0){
      updateSelectInput(session,inputId='selBasic',selected=rv$rprefix)};
    message('updating rdat');
    rdat <- selectcodegrps(dat,prefix=input$selBasic,ncutoff=input$slN
                           #,ncutoff=rv$rncut
                           ,chicutoff=input$slChi,oddscutoff=input$slOR
                           #,chicutoff=rv$rchicut,oddscutoff=rv$roddscut
                           );
    # if the filtering returns a non-null group, update reactive values with
    # new filtered data and cutoffs
    if(nrow(rdat)>1){ rv$rdat <- rdat; rv$rncut <- input$slN;
    rv$rchicut <- input$slChi; rv$roddscut <- input$slOR
    rv$rprefix <- input$selBasic} else {
      # otherwise, reset the settings to what they were before the update button
      # got pressed
      updateSelectInput(session,inputId='selBasic',selected=rv$rprefix);
      updateSliderInput(session,inputId='slN',value=rv$rncut);
      updateSliderInput(session,inputId='slChi',value=rv$rchicut);
      updateSliderInput(session,inputId='slOR',value=rv$roddscut);
    };
    message('update button click done');
  });
  # ---- Main Plot ---- 
  output$plotmain <- renderPlotly({
    message('About to render main plot');
    if(any(rv$rdat$PREFIX %in% subset(demogcodes,is.na(CCD))$PREFIX)){
      out <- quickpoints(rv$rdat,alpha=0.3
                         ,targetodds=rv$roddscut) + 
        theme(plot.margin=margin(15,15,30,20),aspect.ratio=1);
      txtMainVar <- txtMainVarDynamic;
    } else {
      out <- quickbars(rv$rdat) +
        theme(axis.text.x=element_text(angle=30)
              ,plot.margin = margin(30,30,60,40));
      txtMainVar <- txtMainVarStatic;
    }
    title <- submulti(rv$rprefix,unique(demogcodes[,c('PREFIX','Category')])
                      ,method='exact') %>% unlist %>% paste0(collapse=', ');
    output$maintext <- renderText(sprintf(paste(txtMainVarCommon,txtMainVar)
                                          ,title));

    ggplotly(out + ggtitle(title)
             ,tooltip='text');
  });
  # ---- Table of Selected Data ----
  output$tblsel <- renderDataTable({
    dd <- (rv$rdat[,names(rv$rdat) %in% rv$rshowcols]) %>% 
      bind_rows(dat_totals[,intersect(names(.),names(dat_totals))]) %>%
      setNames(.,submulti(names(.),renameforplots))
    message('renderDataTable Done!'); dd;}
    ,extensions = c('Buttons', 'Scroller')
    ,autoHideNavigation=T,rownames=F,fillContainer=T
    ,options=list(processing=T,searching=F,scroller=T
                  ,scrollx='100%',scrolly='20vh'
                  #,scroller=T,scrollx=T,scrolly=T
                  ,dom='Bfrtip',buttons=c('copy','csv','excel','print'))
  );
  
  # ---- Debug ----
  observeEvent(input$bdebug,{
    browser();
    });
});

c()
