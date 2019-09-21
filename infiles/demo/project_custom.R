# The minimum fraction of the reference group that must have a code for that
# code to be included
minfrac <- 0.05;

# The minimun count of positives from the reference group for a code to be 
# included
mincount <- 500;

# Any other custom selection criteria for codes
customfilter <- TRUE;

# Which files to load in this directory
dfiles <- c('rand85.csv','rand89_5.csv','rand95.csv');

# Rename to standardized column names
chirename <- rbind(c('ODDS_RATIO','OR')
                   );
# Display names
renameforplots <- rbind( c('REF','Overall Population')
                         ,c('M7002.I7002.R10840','Random Subcohort 85')
                         ,c('M7001.I7001.R10838','Random Subcohort 95')
                         ,c('M7003.I7003.R10842','Random Subcohort 89_5')
                         );
c()
