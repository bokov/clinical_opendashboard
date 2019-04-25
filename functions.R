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
read_chis <- function(t1,t2,varinfo=1:3,ref=4:5,cohort=6:10,groupnames='All'
                      ,submulti=matrix(ncol=2,nrow=0)){
  if(!is(t1,'data.frame')) t1 <- read_csv(t1);
  if(!is(t2,'data.frame')) t2 <- read_csv(t2);
  if(missing(groupnames)){
    groupnames <- c(groupnames,names(t1)[cohort[1]],names(t2)[cohort[1]]);
  } else{
    if(length(groupnames)!=3){
      stop('If you manually set a groupnames variable then it must be '
           ,'a character vector of three names')}
    # rename the cohorts in their respective columns
    submulti <- rbind(submulti
                      ,c(names(t1)[cohort][1],groupnames[2])
                      ,c(names(t2)[cohort][1],groupnames[3]))
  }
  # rename the reference population columns
  submulti <- rbind(submulti
                    ,c(names(t1)[ref][1],groupnames[1])
                    ,c(names(t2)[ref][1],groupnames[1]))
  names(t1) <- submulti(names(t1),submulti);
  names(t2) <- submulti(names(t2),submulti);
  if(any(names(t1)[c(varinfo,ref)]!=names(t1)[c(varinfo,ref)])){
    warning('Some variable names or reference column names do not match.'
           ,'Attempting to fix');
    names(t2)[c(varinfo,ref)] <- names(t1)[c(varinfo)];
  }
  byarg <- setNames(names(t1)[c(varinfo,ref)],names(t2)[c(varinfo,ref)]);
  out <- full_join(t1,t2,by=byarg,suffix=paste0('_',groupnames[2:3]));
  if(!all.equal(nrow(out),nrow(t1),nrow(t2))){
    warning('Rows from the individual tables have been either lost '
           ,'or duplicated');}
  return(out);
}

quickreshape <- function(data,groups,pattern=c('FRC_%s')
                         ,other=c(),sep='_',timevar='Group'){
  varyingarg <- c(sapply(groups,function(xx) sprintf(pattern,xx)));
  out <- data[,c(other,varyingarg)];
  out <- reshape(out,direction='long',varying=varyingarg,sep=sep
                 ,timevar=timevar);
}

chifilter <- function(data,groups,ncutoff=300,chicutoff=200,oddscutoff=1.5
                      ,npattern='%1$s',chipattern='CHISQ_%1$s'
                      # other is where to put additional filter terms
                      # start them with '& '
                      ,oddspattern='ODDSRATIO_%1$s'
                      ,varclass='PREFIX',sortby='All',other=''){
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

# subset from 'data' based on codes in 'codemap' (PREFIX,CCD) with the
# selection and order specified by 'prefix' argument
selectcodegrps <- function(data,codemap
                           ,prefix=c('DEM|SEX','DEM|ETHNICITY'
                                     ,'DEM|LANGUAGE','DEM|RACE')
                           ,prefixcol='PREFIX'){
  sel <- try(bind_rows(lapply(prefix,function(ii){
    subset(codemap,codemap[[prefixcol]]==ii)})));
  left_join(sel,data);
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
  ,cols=setNames(brewer_pal(type='qua')(length(groups)),groups)
  ,alpha=1){
  ttmpl <- paste0("sprintf('<b>%s</b><br>All Urology:%s<br>%s:%s',"
      ,labels[1],",percent(",yy,")");
  out <- ggplot(data,aes_string(y=yy
                                # ,text=labels[1]
                                )
                );
  if(missing(xs)) xs <- paste0(colprefix,groups);
  cols <- setNames(brewer_pal(type='qua')(length(groups)),groups);
  for(ii in seq_along(groups)){
    # browser();
    out <- out + geom_point(
      aes_string(x=xs[ii]
                 ,text=paste0(ttmpl,",'",groups[ii],"',percent(",xs[ii],'))'))
      ,colour=cols[ii],alpha=alpha);
    #col <- col + 1;
  }
  out <- out + geom_abline(slope=1,intercept = 0) +
    #scale_color_manual(name='Group',values=cols,guide=guide_legend()) +
    scale_x_continuous(trans=log1p_trans(),limits = 0:1,labels=percent) +
    scale_y_continuous(trans=log1p_trans(),limits = 0:1,labels=percent);
}
