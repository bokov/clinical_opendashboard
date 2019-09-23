#' ---
#' title: "Chinotype Dashboard Prototype"
#' author: "Alex F. Bokov"
#' date: "April 24, 2019"
#' ---

# ---- Global variables ---- 

totalcode <- 'TOTAL';
#mincountfrac <- 0.01;
namecol <- 'NAME'; ccdcol <- 'CCD'; prefixcol <- 'PREFIX';

# renameforplots <- rbind(
#   c('REF','Reference Population')
#   ,c('HISPANIC','Hispanic')
#   ,c('LOWINCOME','Low Income')
# );
# 
# inherit the reference population from above
#refgroupname <- renameforplots[1,2];

# give deployer of this app the option to override any of the above by creating
# a script named 'project_custom.R'
#if(file.exists('project_custom.R')) source('project_custom.R',local = T);

# ---- Default Arguments ----
#formals(quickreshape)[c('groups','other')] <- list(n_groupnames[-1]
                                                   # ,c('Category','NAME'
                                                   #    ,paste0('FRC_',n_all)));
#formals(chifilter)[c('groups','sortby')] <- list(n_groupnames[-1],n_all);
#formals(selectcodegrps)$codemap <- demogcodes;
#formals(quickbars)[c('searchrep')] <- list(renameforplots);
#formals(quickpoints)[c('refgroupname','searchrep')] <- list(refgroupname
#                                                            ,renameforplots);
formals(read_chis)$searchrep <- chirename;
# ---- Read Data ----
# if('cached_data.rdata' %in% list.files()){
#   load('cached_data.rdata');}
# if(!exists('dat')||!isValidChi(dat)){
#   if(!all(dfiles %in% list.files())){
#     stop('The input datafiles are missing. In addition to these scripts, you '
#          ,'or whoever is responsible for deploying this webapp also needs to '
#          ,'obtain data for it to process.')}
  # Note: here is sed code for removing doubled quotes that could be
  # output by current chinotype:
  # sed -i 's/"\([^"]*\)"",/\1",/g' FILE.csv 
  # sed -i 's/,""\([^"]*\)"/,"\1/g' FILE.csv 
  
  # raw <- if(length(dfiles)>1) Reduce(read_chis,dfiles) else {
  #   standardize_chis(dfiles)};
  # rolled into standardize_chis()
  #dat_totals <- subset(raw,CCD==totalcode);
  # part of read_chi_list()
  #dat <- subset(raw,raw$N_REF>mincountfrac*dat_totals$N_REF);
  # fix label-less columns ...moving to standardize_chis()
  #dat[[namecol]] <- coalesce(dat[[namecol]]
  #                           ,paste(dat[[prefixcol]],dat[[ccdcol]],sep = ':'));
  #attr(dat,'sectioncols') <- attr(raw,'sectioncols');
#   save(dat,dat_totals,file='cached_data.rdata');
# }
# code for later dynamically obtaining demogcodes
# which codes in this dataset are empirically eligible for column vs
# point treatment (vs, for now, none if there is only one code for
# that prefix)
#.colpref <- table(dat$PREFIX) %>% subset((.)>1&(.)<=15) %>% names;
#.pntpref <- table(dat$PREFIX) %>% subset((.)>15) %>% names;
# dynamic equivalent of demogcodes. This is going to replace demogcodes. The 
# object called 'demogcodes' in the test code below will be replaced with a
# dictionary object that is part of the repo. Either this object or some other 
# one will also be used for joins on information associated with prefixes that 
# we want to persist

# The first part is for the column variables
# .codes <- subset(dat,PREFIX %in% .colpref)[,c('PREFIX','CCD')] %>% 
#   mutate(NAME=unlist(submulti(PREFIX,unique(demogcodes[,c('PREFIX','Category')])
#                               ,method='exact'))) %>% arrange(PREFIX) %>%
#   # it's row-bound to the part for scatterplot variables
#   bind_rows(data.frame(PREFIX=.pntpref,CCD=NA
#                        ,NAME=unlist(submulti(.pntpref,demogcodes
#                                              ,method='exact'))));
# # there may also be additional rows bound on for abnormal labs and perhaps other 
# # future data elements that behave in similar ways

# adapt slidevals sample size default based on smallest cohort size
.GlobalEnv$slidevals$N <- round(min(dat_totals[
  ,grep('^N_',names(dat_totals))]) * minfrac);

# ---- Server ----
message('Defining shinyServer');
shinyServer(function(input, output, session) {

# ---- initialize reactive values ----
  rv <- reactiveValues(
    rprefix=.GlobalEnv$selBasicDefault
    ,rchicut=slidevals$Chi
    ,rncut=slidevals$N,roddscut=slidevals$OR
    ,starting=TRUE
    ,rsysinfo=unclass(c(Sys.info(),sessionInfo()
                        ,filesys=list(BASEPATH=getwd()
                                      ,FILES=list.files(all.files=TRUE))
                        ,ENV=Sys.getenv()))
    ,spath=file.path(
      infiles,c(isolate(parseQueryString(session$clientData$url_search))$dfile
                ,'default')[1])
    ,srcid=c(isolate(parseQueryString(session$clientData$url_search))$rcid
             ,NA)[1]
    ,log=list());
  rv$sv <- codehr_init(isolate(rv$spath),rcid=isolate(rv$srcid));
  rv$rshowcols <- with(isolate(rv$sv)
                       ,c('Category','NAME','CCD'
                          ,grep('^(N_|FRC_|CHISQ_|OR_)',names(dat)
                                ,value =TRUE)));
  rv$rdat <- with(isolate(rv$sv)
                  ,selectcodegrps(get('dat'),prefix=.GlobalEnv$selBasicDefault
                                  ,codemap = demogcodes));

  observe({
    updateSelectInput(session,inputId='selBasic',selected=rv$rprefix);
    },label = 'selBasic_update');
  hide('bupdate');
  # ---- System/Session Info ----
  if(file.exists('.debug')){
    output$uidebug <- renderUI(actionButton('bdebug','Debug'))};
  if(any(file.exists('remote_debug','.debug'))){
    output$uidebuginfo <- renderUI({
      fluidRow(id='debuginfo'
               ,bsCollapse(id="systeminfo"
                           ,bsCollapsePanel(span("System Info"
                                                 ,icon('angle-down'))
                                            ,verbatimTextOutput('strSysinfo')
                                            ,shinyTree('trSysinfo')))
               ,actionButton('brmdebug','Remove Debug Capability'))});
    output$trSysinfo <- renderTree(rv$rsysinfo);
  }
  # ---- Reset ----
  # allow the user to reset the sliders to their starting values
  observeEvent(input$breset,{
    message('Processing reset click');
    updateSliderInput(session,inputId='slN',value=slidevals$N);
    updateSliderInput(session,inputId='slChi',value=slidevals$Chi);
    updateSliderInput(session,inputId='slOR',value=slidevals$OR);
    message('Done with reset click');
  });
  # ---- Hide/Show Update Button
  observeEvent({input$selBasic;input$slChi;input$slOR; input$slN;}
               ,if(rv$starting) {
                 rv$starting <- F; hide('bupdate'); 
                 message('\n HIDING UPDATE DUE TO STARTUP');
                 } else {
                 if(input$selBasic != rv$rprefix ||
                    input$slChi != rv$rchicut ||
                    input$slN != rv$rncut ||
                    input$slOR != rv$roddscut) {
                   show('bupdate'); 
                   message('\n SHOWING UPDATE');
                   } else {
                     hide('bupdate');
                     message('\n HIDING UPDATE');
                   }
                   }
               );
  # ---- Update Button Clicked ----
  observeEvent({input$bupdate},{
    message('starting update button click');
    if(length(input$selBasic)==0){
      updateSelectInput(session,inputId='selBasic',selected=rv$rprefix)};
    message('updating rdat');
    rdat <- selectcodegrps(get('dat',rv$sv),codemap = get('demogcodes',rv$sv)
                           ,prefix=input$selBasic,ncutoff=input$slN
                           ,chicutoff=input$slChi
                           ,oddscutoff=input$slOR);
    # if the filtering returns a non-null group, update reactive values with
    # new filtered data and cutoffs
    if(nrow(rdat)>1){ rv$rdat <- rdat; rv$rncut <- input$slN;
    rv$rchicut <- input$slChi; rv$roddscut <- input$slOR
    rv$rprefix <- input$selBasic} else {
      # otherwise, reset the settings to what they were before the update button
      # got pressed
      showNotification('No results match criteria, restoring previous ones.'
                       ,type='error');
      updateSelectInput(session,inputId='selBasic',selected=rv$rprefix);
      updateSliderInput(session,inputId='slN',value=rv$rncut);
      updateSliderInput(session,inputId='slChi',value=rv$rchicut);
      updateSliderInput(session,inputId='slOR',value=rv$roddscut);
    };
    hide('bupdate');
    message('update button click done');
  },label = 'update_button_click');
  # ---- Main Plot ---- 
  output$plotmain <- renderPlotly({
    message('About to render main plot');
    # prefixpoints defined in global.R, using demogcodes
    # i.e. plot these results as a scatterplot
    if(any(rv$rdat$PREFIX %in% prefixpoints)){
      out <- quickpoints(rv$rdat,alpha=0.3
                         ,searchrep = get('renameforplots',rv$sv)
                         ,refgroupname = get('renameforplots',rv$sv)[1,2]
                         ,targetodds=rv$roddscut) + 
        theme(plot.margin=margin(15,15,30,20),aspect.ratio=1);
      txtMainVar <- txtMainVarDynamic;
    } else {
      # otherwise, plot them as side-by-side bars
      # TODO: think about what happens if somebody picks only non prefixpoints
      #       variables but a lot of them
      out <- quickbars(rv$rdat,searchrep = get('renameforplots',rv$sv)) +
        theme(axis.text.x=element_text(angle=30)
              ,plot.margin = margin(30,30,60,40));
      txtMainVar <- txtMainVarStatic;
    }
    title <- submulti(isolate(rv$rprefix)
                      ,unique(demogcodes[,c('PREFIX','Category')])
                      ,method='exact') %>% unlist %>% paste0(collapse=', ');
    output$maintext <- renderText(sprintf(paste(txtMainVarCommon,txtMainVar)
                                          ,title));
    #rv$currentplot <- out;
    ggplotly(out + ggtitle(title)
             ,tooltip='text');
  });
  
  # ---- Table of Selected Data ----
  output$tblsel <- renderDataTable({
    dat_totals <- attr(rv$rdat,'totalrow');
    dat_totals <- dat_totals[,names(dat_totals) %in% rv$rshowcols];
    dd <- (rv$rdat[,names(rv$rdat) %in% rv$rshowcols]) %>% 
      bind_rows(dat_totals) %>%
      setNames(.,submulti(names(.),renameforplots)) %>% 
      DT::datatable(extensions = c('Buttons', 'Scroller')
                    ,autoHideNavigation=T,rownames=F,fillContainer=T
                    ,options=list(processing=T,searching=F,scroller=T
                                  ,scrollx='100%',scrolly='20vh'
                                  ,dom='Bfrtip'
                                  ,buttons=c('copy','csv','excel' #,'print'
                                             ,'colvis'))
                    ) %>% 
      DT::formatPercentage(.,grep('^FRC_',dimnames(.)[[2]]),digits=2)
    #message('renderDataTable Done!'); dd;
    },server=F);
  # ---- Disable or Enable Advanced Filters ----
  observeEvent(input$selBasic,{
    if(input$selBasic %in% prefixpoints) {
      show(selector=".panel[value='Advanced']");
      #enable(selector=".panel[value='Advanced']>div.panel-heading>
      #        .panel-title>.collapsed");
      showNotification('Advanced filters available for this data element')
      } else {
        hide(selector=".panel[value='Advanced']");
        #updateCollapse(session,'filters',close='Advanced');
      #disable(selector=".panel[value='Advanced']>div.panel-heading>
      #      .panel-title>.collapsed");
    }
  });
  # ---- Logging ----
  observe({
    if(file.exists('applog.csv')){
      logentry <- c(reactiveValuesToList(input)
                    ,time=as.character(Sys.time())
                    ,ip=session$request$REMOTE_ADDR
                    ,agent=session$request$HTTP_USER_AGENT
                    ,token=session$token);
      logentry <- data.frame(rbind(logentry[!sapply(logentry, is.null)]));
      isolate(rv$log[[length(rv$log)+1]]<-logentry);
    }
  });
  
  endsession <- session$onSessionEnded(function() {
    message('\n\n*** Closing session');
    writeLog(rv);
    message('\n\n*** Done logging');
    #click('breset'); click('bupdate');
    #message('Done resetting');
  });
  # ---- Debug ----
  # print system info about remote instance
  observeEvent(c(input$trSysinfo,rv$rsysinfo),{
    output$strSysinfo <- renderPrint(lapply(get_selected(input$trSysinfo)
                                            ,function(xx){
                                              c(attr(xx,'ancestry'),xx)}) %>% 
                                       lapply(function(xx){
                                         setNames(list(`[[`(rv$rsysinfo,xx))
                                                  ,paste(xx,collapse='$'))}) %>%
                                       Reduce(c,.))});
  # drop into interactive debug on local instance
  observeEvent(input$bdebug,{
    browser();
    });
  
  # permanently disable debug button and system info panel by 
  # deleting the files that signal that they should be enabled
  observeEvent(input$brmdebug,{
    file.remove('.debug','remote_debug');
    removeUI('#uidebug');
    removeUI('#uidebuginfo');
    message('Removed debug functionality');
  });
});

c()
