# ---- Small Utility Functions ----
# p1 and p2 are proportions between 0 and 1
# in our case, p2 corresponds to the overall population and p2 to
# a cohort of interest
or <- function(p1,p2){p2*(1-p1)/(p1*(1-p2))};
# given a p1, calculate the p2 needed for a target odds ratio
otherpr <- function(p1,target=1.5){target/(1/p1+target-1)};

# split vector xx at the positions indicated by vectir pos
# as per https://stackoverflow.com/questions/16357962/r-split-numeric-vector-at-position#comment28556464_19274414
splitAt <- function(xx, pos) {
  pos <- c(1L, pos, length(xx) + 1L); 
  Map(function(xx, i, j) xx[i:j], list(xx), head(pos, -1L), tail(pos, -1L) - 1L)
  };

# ---- Rename/Remap ----
#' Usage: `xx<-mapnames(xx,lookup)` where lookup is a named character vector
#' the names of the elements in the character vector are what you are renaming
#' things TO and the values are what needs to be matched, i.e. what renaming things
#' FROM. If you set namesonly=T then it just returns the names, not the original
#' object.
#' Take a character vector and perform multiple search-replace 
#' operations on it.
#' @param xx A \code{vector} of type \code{character} (required)
#' @param searchrep A \code{matrix} with two columns of type \code{character} (required). The left column is the pattern and the right, the replacement.
#' @param method One of 'partial','full', or 'exact'. Controls whether to replace only the matching regexp, replace the entire value that contains a matching regexp, or replace the entire value if it's an exact match.
submulti <- function(xx,searchrep
                     ,method=c('partial','full','exact'
                               ,'starts','ends','startsends')){
  # if no method is specified by the user, this makes it take the first value
  # if a method is only partially written out, this completes it, and if the
  # method doesn't match any of the valid possibilities this gives an informativ
  # error message
  method<-match.arg(method);
  # if passed a single vector of length 2 make it a matrix
  if(is.null(dim(searchrep))&&length(searchrep)==2) searchrep<-rbind(searchrep);
  # rr is a sequence of integers spanning the rows of the searchrep matrix
  rr <- 1:nrow(searchrep);
  # oo will hold the result that this function will return
  oo <- xx;
  switch(method
         ,partial = {for(ii in rr)
           oo <- gsub(searchrep[ii,1],searchrep[ii,2],oo)}
         ,full =    {for(ii in rr)
           oo[grepl(searchrep[ii,1],oo)]<-searchrep[ii,2]}
         ,exact = {for(ii in rr)
           oo[grepl(searchrep[ii,1],oo,fixed=T)]<-searchrep[ii,2]}
         ,starts = {for(ii in rr)
           oo <- gsub(paste0('^',searchrep[ii,1]),searchrep[ii,2],oo)}
         ,ends = {for(ii in rr)
           oo <- gsub(paste0(searchrep[ii,1],'$'),searchrep[ii,2],oo)}
         ,startsends = {for(ii in rr)
           oo <- gsub(paste0('^',searchrep[ii,1],'$'),searchrep[ii,2],oo)}
  );
  oo;
}


# ---- Manage Data ----

standardize_chis <- function(dat,keepextra=F
                             # set to NULL or c() to not do searchrep
                             ,searchrep=rbind(c('ODDS_RATIO','OR'))
                             ,readfn=readr::read_csv,...){
  if(!is(dat,'data.frame') && is.character(dat)) dat <- readfn(dat);
  if(!all(c('PREFIX','CCD') %in% names(dat))){
    stop("The object specified by the 'dat' argument is not a valid Chinotype"
         ,"table. It is missing a 'PREFIX' and/or a 'CCD' column")};
  .orignames <- names(dat);
  if(NROW(dat)==0) warning("The object specified by the 'dat' argument has"
                           ," 0 rows. This may cause errors further on.");
  # any up-front search replaces
  if(NROW(searchrep)>0) names(dat) <- submulti(names(dat),searchrep);
  names(dat) <- toupper(names(dat));
  # make sure there is a TOTAL row, warn if not
  if(nrow(subset(dat,PREFIX=='TOTAL'&CCD=='TOTAL'))==0){
    warning("The object specified by the 'dat' arguments is missing a"
            ," 'TOTAL' row, and this may cause errors further on.")};
  # get the base names
  basenames <- gsub('^FRC_','',grep('^FRC_',names(dat),val=T));
  names(dat) <- submulti(names(dat),cbind(paste0('^',basenames,'$')
                                          ,paste0('N_',basenames)));
  # replace ^basename$ with N_basename
  # replace first basename with N_REF, FRC_REF and error if either not found
  names(dat) <- gsub(paste0('_',basenames[1]),'_REF',names(dat));
  basenames[1] <- 'REF';
  splitcols <- paste0('N_',basenames); splitidx <- match(splitcols,names(dat));
  if(!all(c('N_REF','FRC_REF') %in% names(dat))){
    # TODO: make this error easier to understand and the warning after it
    stop("The object specified by the 'dat' argument seems to be missing the"
         ," count and/or the fraction column for the reference group.")};
  if(length(splitcols)!=length(splitidx)){
    warning("The object specified by the 'dat' argument is missing one or more"
         ," count columns. This may cause errors later.")};
  # splitAt N_foo columns and label with basenames
  sections <- setNames(splitAt(seq_along(names(dat)),splitidx)
                       ,c('Info',basenames));
  # TODO: warn/error if OR or CHISQ exists in the N_REF/FRC_REF range
  canonicalnames <- c('PREFIX','CCD'
                      ,if('NAME' %in% names(dat)) 'NAME' else NULL
                      ,'N_REF','FRC_REF');
  for(ii in basenames[-1]){
    iisection <- sections[[ii]]; iinames <- names(dat)[iisection];
    iichi <- paste0('CHISQ_',ii); iior <- paste0('OR_',ii);
    # standardize chisq column names
    if(!iichi %in% names(dat)) iinames <- gsub('CHISQ',iichi,iinames);
    if(!iior %in% names(dat)) iinames <- gsub('OR',iior,iinames);
    names(dat)[iisection] <- iinames;
    if(length(.missing <- setdiff(c(iichi,iior),names(dat)))>0){
      stop(sprintf("Cannot identify a '%s' column",paste(.missing
                                                         ,collapse=', '))
           ," in the object specified by the 'dat' argument")};
    canonicalnames <- c(canonicalnames,paste0(c('N_','FRC_'),ii),iichi,iior);
  }
  # detect duplicate N_,FRC_,OR_,CHISQ_ columns and warn if any
  if(length(match(canonicalnames,names(dat)))!=length(canonicalnames)){
    warning("Duplicate column names found in object specified by the 'dat'"
            ," argument. This may cause errors further down.")};
  # generate canonical output header, including non-required columns if they
  # exist, put all unknown column names at the end
  if(keepextra){
    canonicalnames <- c(canonicalnames,setdiff(names(dat),canonicalnames))};
  # return result
  dat[,canonicalnames];
}

read_chis <- function(t1,t2,searchrep=rbind(c('ODDS_RATIO','OR'))){
  if(!is(t1,'data.frame')) t1 <- standardize_chis(t1,searchrep = searchrep);
  if(!is(t2,'data.frame')) t2 <- standardize_chis(t2,searchrep = searchrep);
  out <- dplyr::full_join(t1,t2);
  if(!all.equal(nrow(out),nrow(t1),nrow(t2))){
    warning('Rows from the individual tables have been either lost '
           ,'or duplicated')};
  return(out);
}

quickreshape <- function(data,groups,pattern=c('FRC_%s')
                         ,other=c(),sep='_',timevar='Cohort',...){
  varyingarg <- setdiff(c(sapply(groups,function(xx) sprintf(pattern,xx)))
                        ,other);
  out <- data[,c(other,varyingarg)];
  out <- reshape(out,direction='long',varying=varyingarg,sep=sep
                 ,timevar=timevar);
}

chifilter <- function(data,groups,ncutoff=300,chicutoff=200,oddscutoff=1.5
                      ,npattern='%1$s',chipattern='CHISQ_%1$s'
                      # other is where to put additional filter terms
                      # start them with '& '
                      ,oddspattern='ODDSRATIO_%1$s'
                      ,varclass='PREFIX',sortby='All',other='',...){
  template <- paste('(',npattern,'>',ncutoff,'&',chipattern,'>',chicutoff
                    ,'&','abs(log(',oddspattern,'))','>',abs(log(oddscutoff))
                    ,other,')');
  filter <- paste(sapply(groups,function(xx) sprintf(template,xx))
                  ,collapse='|');
  out <- subset(data,eval(parse(text=filter)));
  if(!varclass %in% names(out)){
    varclass <- '';
    warning('"varclass" variable not found, ignoring');
  } else varclass <- paste(',',varclass);
  if(!sortby %in% names(out)){
    sortby <- '';
    warning('"sortby" variable not found, ignoring');
  } else sortby <- paste(',','desc(',sortby,')');
  eval(parse(text=paste('arrange(out',varclass,sortby,')')));
}

# subset from 'data' based on codes in 'codemap' (PREFIX,Category,CCD)
# with the selection and order specified by 'prefix' argument
selectcodegrps <- function(data,codemap,groups
                           ,prefix=c('DEM|SEX','DEM|ETHNICITY'
                                     ,'DEM|LANGUAGE','DEM|RACE')
                           # ... passed to chifilter() function
                           ,...){
  # validate input
  if(!all.equal(names(codemap)[1:4]
                ,c('PREFIX','Category','CCD','PseudoPrefix'))){
    stop("The 'codemap' argument must be a data.frame like object"
         ," that has columns 'PREFIX','Category', and 'CCD'.")};
  if(!identical(unique(codemap[,c('PREFIX','CCD')])
                ,codemap[,c('PREFIX','CCD')])){
    stop("The data.frame like object specified by the 'codemap'"
         ," variable must have only unique pairs of 'PREFIX' and 'CCD'.")};
  # static selectors-- variables set explicitly and not filtered
  # doing this lapply/bind_rows thing in order to preserve the user-specified
  # ordering of the selected categories.
  selst <- bind_rows(lapply(prefix,function(ii){
    subset(codemap,!is.na(CCD) & PREFIX==ii)}));
  # dynamic selectors-- only the prefix is set, and which variables
  # to include determined by ChiSq, OR, and N
  seldn <- bind_rows(lapply(prefix,function(ii){
    subset(codemap,is.na(CCD) & PREFIX==ii)}))[,c('PREFIX','Category')] %>%
    subset(!grepl('^CUSTOM=',PREFIX));
  oost <- if(nrow(selst)>0) left_join(selst,data) else c();
  oodn <- if(nrow(seldn)>0) left_join(seldn,chifilter(data,groups,...)) else c();
  for(ii in grep('^CUSTOM=',prefix,val=T)){
    # custom pseudo-prefixes
    ooii <- chifilter(data,groups,other=gsub('^CUSTOM=',' & ',ii),...);
    ooii$Category <- subset(codemap,PREFIX==ii)$Category;
    oodn <- rbind(oodn,ooii);
  }
  # return static and then dynamically selected variables
  rbind(oost,oodn);
}

# ---- Visualization ----
quickbars <- function(data,groups,labels='NAME',colprefix='FRC_'
                      ,yy=paste0(colprefix,'All'),xs){
  if(missing(xs)) xs <- paste0(colprefix,groups);
  data0<-bind_rows(lapply(c(yy,xs),function(xx) {
    setNames(cbind(select(data,labels,xx),gsub(colprefix,'',xx))
             ,c('Item','Percent','Group'))}));
  data0$tip <- with(data0,sprintf('<b>%s</b><br>%s: %s'
                                  ,Item,Group,percent(Percent)));
  # the factor thing is so the order of the columns is the same as
  # their first occurrence in the input 
  data0$Item <- factor(data0$Item,levels = unique(data0$Item));
  out <- ggplot(data0,aes(x=Item,y=Percent,fill=Group,text=tip));
  out + geom_col(width=0.6,position = position_dodge(width = 0.6)) +
    scale_y_continuous(limits = 0:1,labels=percent) + xlab('')
}
quickpoints <- function(
  data,groups,labels='NAME',colprefix='FRC_'
  ,yy=paste0(colprefix,'All'),xs
  #,cols=setNames(brewer_pal(type='qua')(length(groups)),groups)
  ,alpha=0.5,other=c('Category','NAME',yy),targetodds=1.5
  ,bandclr='orange'
  # tooltip template
  ,ttemplate='<b>%s</b><br>All Urology: %s<br>%s: %s'
  ,...){
  if(missing(xs)) xs <- paste0(colprefix,groups);
  # validate
  if(!all((.neededcols <- c(yy,xs,other)) %in% names(data))){
    stop("The following columns were expected in the data and were not found:"
         ,"\n",paste0(setdiff(.neededcols),names(data)))};
  # put the data in long format for ggplot
  data0 <- quickreshape(data,groups,other=other);
  # rename the reference column to something predictable
  names(data0) <- sub(yy,'FRC_REF',names(data0));
  # create the tooltip column
  # TODO: NAME isn't guaranteed to be used in future applications, think about
  # better factoring.
  data0$tooltip <- with(data0,sprintf(ttemplate,NAME,percent(FRC_REF)
                                      ,Cohort,percent(FRC)));
  out <- ggplot(data0,aes(y=FRC_REF,x=FRC,color=Cohort,text=tooltip)
                ,alpha=alpha) + geom_point(alpha=alpha);
  maxy <- max(c(data0$FRC_REF,data0$FRC),na.rm = T);
  bands <- data.frame(pr=seq(0,maxy,len=40)) %>% 
    mutate(ub=otherpr(pr,targetodds),lb=otherpr(pr,1/targetodds)
           ,tooltip=paste('Odds Ratio =',targetodds));
  out + geom_abline(slope=1,intercept = 0) +
    geom_line(aes(x=pr,y=ub),data=bands,linetype=3,color=bandclr) +
    geom_line(aes(x=pr,y=lb),data=bands,linetype=3,color=bandclr) +
    scale_x_continuous(trans=log1p_trans(),limits = c(0,maxy),labels=percent) +
    scale_y_continuous(trans=log1p_trans(),limits = c(0,maxy),labels=percent) +
    xlab('Percent of each Cohort') + ylab('Percent of All Urology');
}
