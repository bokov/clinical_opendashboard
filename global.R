library(readr);
# selected prefixes and concept codes 
demogcodes <- read_csv('demogcodes.csv');
# demogcodes should include the following columns: 'PREFIX','Category','CCD'
# PREFIX and CCD are the same as used by ChinoType. Category is a human-readable
# version of PREFIX.
selBasicChoices <- with(unique(demogcodes[,c('PREFIX','Category')])
                        ,setNames(PREFIX,Category));
# UI text
source('uitext.R');
c()
