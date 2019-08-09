# for environments needing user-space packages
if(dir.exists('R-lib')) .libPaths(c(normalizePath('R-lib'),.libPaths()));

# ---- Enforce version specific dependencies ----
# The data.table update has to be done before plotly is loaded otherwise plotly
# errors when trying to render numeric (scatterplot) variables
.packagedebug <- try(if(R.version$major>=3 && R.version$minor >= 6 && 
   packageVersion('data.table')<'1.11.2'){
  install.packages('data.table',repos='https://cloud.r-project.org')});
if(is(.packagedebug,'try-error')) warning(.packagedebug);

library(readr); library(shinyTree);
# selected prefixes and concept codes 
# TODO: consider dynamically creating demogcodes.csv from the data if it doesn't
#       exist.
# TODO: What would happen if there was a row in demogcodes.csv with PREFIX='ALL'
#       instead of being added on at runtime? Would this be a cleaner solution?
demogcodes <- if(file.exists('demogcodes.csv')){
  read_csv('demogcodes.csv')} else {read_csv('demogcodes_demo.csv')};
# prefixes whose variables should be rendered as scatter-plots
prefixpoints <- c(subset(demogcodes,is.na(CCD))$PREFIX,'ALL');
# demogcodes should include the following columns: 'PREFIX','Category','CCD'
# PREFIX and CCD are the same as used by ChinoType. Category is a human-readable
# version of PREFIX.
selBasicChoices <- with(unique(demogcodes[,c('PREFIX','Category')])
                        ,c(grep('TOTAL',inv=T,val=T,setNames(PREFIX,Category))
                           ,c(`All Codes`='ALL')));
# slider defaults
slidevals <- list(N=300,OR=1.5
                  #,Chi=200
                  ,Chi=0.05
                  );
# UI text
source('uitext.R');
# to override various stuff
if(file.exists('project_uitext.R')) source('project_uitext.R');
c()
