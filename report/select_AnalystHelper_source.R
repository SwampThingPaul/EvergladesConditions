DBHYDRO_daily=function(SDATE, EDATE, DBK,dataonly=TRUE,period = "uspec",v_target_code = "file_csv",vert_datum=1,...) 
{
  # Legacy Code
  #Returns daily data from SFWMD DBHydro
  # DBK.val=paste("",DBK,"",collapse="/",sep="")
  # SDATE=paste(format(SDATE,"%Y"),toupper(format(SDATE,"%m")),format(SDATE,"%d"),sep="");#In YYYYMMDD format
  # EDATE=paste(format(EDATE,"%Y"),toupper(format(EDATE,"%m")),format(EDATE,"%d"),sep="");#In YYYYMMDD format
  # link=paste("http://my.sfwmd.gov/dbhydroplsql/web_io.report_process?v_period=uspec&v_start_date=",SDATE,"&v_end_date=",EDATE,"&v_report_type=format6&v_target_code=file_csv&v_run_mode=onLine&v_js_flag=Y&v_dbkey=",DBK.val,sep="")
  # REPORT=read.csv(link,skip=length(DBK)+offset)
  # REPORT$Date=with(REPORT,as.POSIXct(as.character(Daily.Date),format="%d-%b-%Y",tz="America/New_York"))
  # REPORT=subset(REPORT,is.na(Date)==F)
  # return(REPORT)
  
  # offset variable is a legecy
  
    # Code inspired by dbhydroR
    # period <- "uspec"
    # v_target_code <- "file_csv"
    # vert_datum <- 1 #1 = NGVD29; 2 = NAVD88
    
  if(is.Date(SDATE)==F&is.Date(EDATE)==F){# |!(nchar(SDATE)==10&nchar(EDATE)==10)){
    stop("Enter dates as a date. ")
    # stop("Enter dates as character strings in YYYY-MM-DD format or as.Date(...)")
  }
  if(all(is.na(DBKEY))==T){
    stop("Must specify either a dbkey")
  }
  
    DBK.val=paste("",DBK,"",collapse="/",sep="")
    SDATE=paste(format(SDATE,"%Y"),toupper(format(SDATE,"%m")),format(SDATE,"%d"),sep="");#In YYYYMMDD format
    EDATE=paste(format(EDATE,"%Y"),toupper(format(EDATE,"%m")),format(EDATE,"%d"),sep="");#In YYYYMMDD format
    
    qy <- list(v_period = period, v_start_date = SDATE, v_end_date = EDATE,
               v_report_type = "format6", v_target_code = v_target_code,
               v_run_mode = "onLine", v_js_flag = "Y", v_dbkey = DBK.val,v_datum = vert_datum,...)
    qy=qy[is.na(qy)==FALSE]
    
    servfull <- "http://my.sfwmd.gov/dbhydroplsql/web_io.report_process"
    
    link=paste(servfull,paste(paste(names(qy),qy,sep="="),collapse="&"),sep="?")
    res=readLines(link)
    
    raw <- suppressMessages(read.csv(text = res, skip = 1,stringsAsFactors = FALSE, row.names = NULL))
    base_skip <- 1
    i= 1 + min(which(apply(raw[,10:16], 1, function(x) all(is.na(x) |nchar(x) == 0))))
    
    metadata <- suppressMessages(read.csv(text = res, skip = base_skip,stringsAsFactors = FALSE, row.names = NULL))[1:(i - 1),]
    name.vals=names(metadata)
    metadata <- subset(metadata,is.na(AGENCY)==F)[,1:(ncol(metadata)-1)]
    names(metadata) <- c(name.vals[2:(length(name.vals))])
    metadata
    
    # dat.col.names=as.character(raw[i,])# dput(as.character(raw[i,]))
    # dat.col.names=dat.col.names[!(dat.col.names%in%c("NA",""))]
    # dat.col.names=gsub(" ",".",dat.col.names);# just incase
    
    if(metadata$FQ%in%c("BK")){
      head.val=c("DATETIME","Station","DBKEY","Data.Value","Flag","Comment")
      
      REPORT=suppressMessages(read.csv(text = res, skip = i+1,stringsAsFactors = FALSE, row.names = NULL,col.names = head.val))
      REPORT$DATETIME=as.POSIXct(REPORT$DATETIME,format="%d-%b-%Y %H:%M",tz="EST")
      REPORT$DATE=as.POSIXct(format(REPORT$DATETIME,format="%Y-%m-%d"),tz="EST")
      REPORT=subset(REPORT,is.na(DATE)==F);# clean up
    }else{
      REPORT=suppressMessages(read.csv(text = res, skip = i+1,stringsAsFactors = FALSE, row.names = NULL))
      REPORT$Daily.Date=with(REPORT,as.POSIXct(as.character(Daily.Date),format="%d-%b-%Y",tz="America/New_York"))
      REPORT$Date=REPORT$Daily.Date;# legacy variable
      REPORT$Revision.Date=with(REPORT,as.POSIXct(as.character(Revision.Date),format="%d-%b-%Y",tz="America/New_York"))
      REPORT=subset(REPORT,is.na(Date)==F);# clean up
    }
    
    final=list(METADATA = metadata,REPORT = REPORT)
    
    if(dataonly==TRUE){
      return(final$REPORT)
    }else{final}
  
}

DBHYDRO_breakpoint=function(SDATE,EDATE,DBK,col.names=c("DATETIME","Station","DBKEY","Data.Value","Flag","Comment"),timeout=200,offset=2){
  DBK.val=paste("",DBK,"",collapse="/",sep="")
  SDATE=paste(format(SDATE,"%Y"),toupper(format(SDATE,"%m")),format(SDATE,"%d"),sep="");#In YYYYMMDD format
  EDATE=paste(format(EDATE,"%Y"),toupper(format(EDATE,"%m")),format(EDATE,"%d"),sep="");#In YYYYMMDD format
  link=paste("http://my.sfwmd.gov/dbhydroplsql/web_io.report_process?v_period=uspec&v_start_date=",SDATE,"&v_end_date=",EDATE,"&v_report_type=format6&v_target_code=file_csv&v_run_mode=onLine&v_js_flag=Y&v_dbkey=",DBK.val,sep="")
  
  # Rcurl issues
  # tmp=RCurl::getURL(link,timeout=timeout)
  # REPORT=read.csv(textConnection(tmp),skip=length(DBK)+offset,col.names=col.names,header=F)
  
  REPORT=read.csv(link,skip=length(DBK)+offset,col.names=col.names,header=F)
  REPORT$DATETIME=as.POSIXct(REPORT$DATETIME,format="%d-%b-%Y %H:%M",tz="EST")
  REPORT$DATE=as.POSIXct(format(REPORT$DATETIME,format="%Y-%m-%d"),tz="EST")
  REPORT=subset(REPORT,is.na(DATETIME)==F)
  return(REPORT)
}

date.fun=function (x, tz = "EST", form = "%F") 
{
  as.POSIXct(strptime(x, form), tz = tz)
}

hydro.day=function (Date, WY.type = "FL") 
{
  if (WY.type == "Fed") {
    start.month = 10
  }
  if (WY.type == "FL") {
    start.month = 5
  }
  Date = as.Date(Date)
  tz.val <- if (is.null(attr(Date, "tzone"))) {
    Sys.timezone()
  }
  else {
    attr(Date, "tzone")
  }
  start.yr = as.numeric(format(Date, "%Y")) - (as.numeric(format(Date, 
                                                                 "%m")) < start.month)
  start.date = as.Date(paste(start.yr, start.month, 1, sep = "-"), 
                       tz = tz.val)
  DOWY = as.integer(Date - as.Date(start.date) + 1L)
  return(DOWY)
}

WY=function (date, WY.type = "FL") 
{
  if (WY.type == "FL") {
    ifelse(as.numeric(format(date, "%m")) > 4, as.numeric(format(date, 
                                                                 "%Y")) + 1, as.numeric(format(date, "%Y")))
  }
  else if (WY.type == "Fed") {
    ifelse(as.numeric(format(date, "%m")) > 9, as.numeric(format(date, 
                                                                 "%Y")) + 1, as.numeric(format(date, "%Y")))
  }
  else {
    NA
  }
}

SalinityCalc=function (SpCond, Temp, Ref.Cond = 42914) 
{
  Cond = SpCond * (1 + 0.0191 * (Temp - 25))
  rt = 0.6766097 + 0.0200564 * Temp + 0.0001104259 * Temp^2 + 
    (-6.9698 * 10^-7) * Temp^3 + (1.0031 * 10^-9) * Temp^4
  Rt = (Cond/Ref.Cond)/rt
  dS = ((Temp - 15)/(1 + 0.0162 * (Temp - 15))) * (5e-04 + 
                                                     (-0.0056) * Rt^0.5 + (-0.0066) * Rt + (-0.0375) * Rt^1.5 + 
                                                     (0.0636) * Rt^2 + (-0.0144) * Rt^2.5)
  Sal = 0.008 + (-0.1692) * Rt^0.5 + 25.3851 * Rt + 14.0941 * 
    Rt^1.5 + (-7.0261) * Rt^2 + 2.7081 * Rt^2.5 + dS
  return(Sal)
}

N.obs=function (x, na.rm = FALSE) 
{
  ind <- is.na(x) | is.nan(x) | is.infinite(x)
  return(length(x[!ind]))
}


cfs.to.acftd=function (x)   x * 1.98347114207859



axis_fun=function (side, at, at2, labels, cex.axis = 1, line = -0.25, 
          lwd = 1, maj.tcl = -0.6, min.tcl = -0.3, las = 1, axisLine = 0, 
          ...) 
{
  axis(side, line = line, at = at, labels = labels, las = las, 
       tcl = maj.tcl, lty = 0, cex.axis = cex.axis, ...)
  axis(side, at = at, labels = F, las = las, tcl = maj.tcl, 
       lwd = lwd, line = axisLine)
  axis(side, at = at2, labels = F, tcl = min.tcl, lwd = lwd, 
       line = axisLine)
}
pt_line=function (x, y, ln.lty, ln.col, ln.lwd, pch, bg, cex = 1, pt.lwd = 0.25, 
          pt.lty = 1, pt.col = "black") 
{
  lines(x, y, lty = ln.lty, col = ln.col, lwd = ln.lwd)
  points(x, y, pch = pch, lty = pt.lty, col = pt.col, lwd = pt.lwd, 
         bg = bg, cex = cex)
}

dat.interp=function (x) 
{
  val = zoo::na.approx(x, na.rm = F)
  val = zoo::na.locf(val, na.rm = F, fromLast = F)
  val = zoo::na.locf(val, na.rm = F, fromLast = T)
  return(val)
}

decimal.WY=function (date, WY.type = "FL") 
{
  Y <- as.numeric(format(date, "%Y"))
  WY <- WY(date, WY.type = WY.type)
  tz.val <- if (is.null(attr(date, "tzone"))) {
    Sys.timezone()
  }
  else {
    attr(date, "tzone")
  }
  if (WY.type == "FL") {
    start <- as.POSIXct(paste(WY - 1, 5, 1, sep = "-"), 
                        tz = tz.val)
    end <- as.POSIXct(paste(WY, 5, 1, sep = "-"), tz = tz.val)
  }
  else if (WY.type == "Fed") {
    start <- as.POSIXct(paste(WY - 1, 10, 1, sep = "-"), 
                        tz = tz.val)
    end <- as.POSIXct(paste(WY, 10, 1, sep = "-"), 
                      tz = tz.val)
  }
  else {
    NA
  }
  sofar <- as.numeric(difftime(date, start, units = "secs"))
  total <- as.numeric(difftime(end, start, units = "secs"))
  dec.dateWY <- WY + sofar/total
  return(dec.dateWY)
}


shaded.range=function(x,y.L,y.U,bg,col=bg,lty=3,col.adj=0.25,lwd=1){
    xx=c(x,rev(x))
    yy=c(y.L,rev(y.U))
    polygon(xx,yy,col=adjustcolor(bg,col.adj),border=col,lty=lty,lwd=lwd)
  }