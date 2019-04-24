#' ---
#' title: "Chinotype Dashboard Prototype"
#' author: "Alex F. Bokov"
#' date: "April 24, 2019"
#' ---

# ---- Libraries ----
library(dplyr); library(readr); library(ggplot2);

source('functions.R');


# ---- Global variables ---- 
n_all <- 'All'
n_groupnames <- c(n_all,'Hispanic','LowIncome');
dfiles <- c('ALL_HISPANIC.csv','ALL_LOWINCOME.csv');
# selected prefixes and concept codes 
demogcodes <- read_csv('demogcodes.csv');
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

# ---- Read Data ----
dat <- read_chis(dfiles[1],dfiles[2],groupnames=n_groupnames
                 ,submulti = renamepatterns);
dat_totals <- subset(dat,CCD==totalcode);
demogcodes <- 
dat <- subset(dat,dat[[n_all]]>mincountfrac*dat_totals[[n_all]]);

# ---- Test Filtering and Plotting ----
dat_test0 <- chifilter(dat);
dat_test_plot0 <- quickplot(dat_test0);
print(dat_test_plot0);
dat_test1 <- left_join(demogcodes,dat);
dat_test_plot1 <- quickplot(dat_test1);
print(dat_test_plot1);


c()