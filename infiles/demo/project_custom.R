# The minimum fraction of the reference group that must have a code for that
# code to be included
minfrac <- 0.01;

# The minimun count of positives from the reference group for a code to be 
# included
mincount <- 100;

# Any other custom selection criteria for codes
customfilter <- TRUE;

# Which files to load in this directory
dfiles <- c('urology_lowincome.csv','urology_hispanic.csv');

# Rename to standardized column names
chirename <- rbind(c('ODDS_RATIO','OR')
                   );
# Display names
renameforplots <- rbind( c('REF','All Urology')
                         ,c('M6921.I6921.R10661','Hispanic')
                         ,c('M6922.I6922.R10663','Low Income')
                   );
c()
