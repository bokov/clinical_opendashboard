# for environments needing user-space packages
if(dir.exists('R-lib')) .libPaths(c(normalizePath('R-lib'),.libPaths()));
library(readr);
# selected prefixes and concept codes 
demogcodes <- read_csv('demogcodes.csv');
# demogcodes should include the following columns: 'PREFIX','Category','CCD'
# PREFIX and CCD are the same as used by ChinoType. Category is a human-readable
# version of PREFIX.
selBasicChoices <- with(unique(demogcodes[,c('PREFIX','Category')])
                        ,setNames(PREFIX,Category));
# slider defaults
slidevals <- list(N=300,OR=1.5
                  ,Chi=200
                  #,Chi=0.2
                  );
# UI text
source('uitext.R');
# to override various stuff
if(file.exists('project_uitext.R')) source('project_uitext.R');
c()
