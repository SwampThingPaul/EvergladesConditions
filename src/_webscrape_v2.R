## Libraries
library(AnalystHelper)
library(reshape2)
library(plyr)
library(zoo)
library(rvest)
library(lubridate)
library(flextable)
library(magrittr)
library(grid)
library(RcppRoll)
library(downloadthis)
library(dataRetrieval)

library(jsonlite)
# library(sp)
# library(rgdal)
# library(rgeos)

# library(tmap)
library(httr)
library(raster)
##
wgs84=CRS("+init=epsg:4326")
utm17=CRS("+init=epsg:26917")
# tmap_mode("view")
###

#### GIS data
shore=spTransform(readOGR("C:/Julian_LaCie/_GitHub/EvergladesConditions/report/GIS","FWC_Shoreline_simp"),wgs84)
# shore=readOGR("./GISData","FWC_Shoreline_simp") 
shore=spTransform(shore,wgs84)

lakeO=readOGR("C:/Julian_LaCie/_GitHub/EvergladesConditions/report/GIS","LakeOkeechobee_general")
# lakeO=readOGR("./GISData","LakeOkeechobee_general")
lakeO=spTransform(lakeO,utm17)

struct=readOGR("C:/Julian_LaCie/_GitHub/EvergladesConditions/report/GIS","SFWMDStructures")
# writeOGR(struct,"C:/Julian_LaCie/_GitHub/EvergladesConditions/report/GIS","SFWMDStructures",driver="ESRI Shapefile")


CurWY=WY(date.fun(Sys.time()))

Start.Date=date.fun(paste(CurWY-4,05,01,sep="-"))
End.Date=date.fun(Sys.time())

YEST=date.fun(End.Date-ddays(1))
# Data from USACE Daily Map -----------------------------------------------
dates=seq(date.fun(paste(format(Sys.Date(),"%Y"),"01","01",sep="-")),date.fun(Sys.Date()),"1 days")

## Maps and Archived data
mapdata=readLines("https://w3.saj.usace.army.mil/h2o/reports/StatusDaily/archive/1217/StatusDaily.htm")

map.q=data.frame()
for(i in 1:(length(dates))){
  map.url=paste0("https://w3.saj.usace.army.mil/h2o/reports/StatusDaily/archive/",format(dates[i],"%m%d"),"/StatusDaily.htm")
  # download.file(map.url, destfile = "map_dat.html", quiet=TRUE)
  mapdata=readLines(map.url)
  
  val=grep("S354",mapdata)
  S354=strsplit(strsplit(mapdata[val],"\\s+")[[1]][8],"</div>")[[1]][1]
  
  val=grep("S351",mapdata)
  S351=strsplit(strsplit(mapdata[val],"\\s+")[[1]][8],"</div>")[[1]][1]
  
  val=grep("S352",mapdata)
  S352=strsplit(strsplit(mapdata[val],"\\s+")[[1]][8],"</div>")[[1]][1]
  
  val=grep("S-271",mapdata)
  S271=strsplit(strsplit(mapdata[val],"\\s+")[[1]][6],"</div>")[[1]][1]
  
  val=grep("CA1IN",mapdata)
  WCA1=strsplit(strsplit(mapdata[val],"\\s+")[[1]][13],"</div>")[[1]][1]
  
  val=grep("CA2IN",mapdata)
  WCA2=strsplit(strsplit(mapdata[val],"\\s+")[[1]][13],"</div>")[[1]][1]
  
  val=grep("CA3IN",mapdata)
  WCA3=strsplit(strsplit(mapdata[val],"\\s+")[[1]][13],"</div>")[[1]][1]
  
  val=grep("S10",mapdata)
  S10s=as.numeric(strsplit(strsplit(mapdata[val],"\\s+")[[1]][8],"</div>")[[1]][1])
  
  val=grep("S11",mapdata)
  S11s=as.numeric(strsplit(strsplit(mapdata[val],"\\s+")[[1]][8],"</div>")[[1]][1])
  
  val=grep("S12",mapdata)
  S12s=as.numeric(strsplit(strsplit(mapdata[val],"\\s+")[[1]][8],"</div>")[[1]][1])
  
  val=grep("S333",mapdata)
  S333=as.numeric(strsplit(mapdata[val],"<br>|\\s+")[[1]][12])
  S333N=as.numeric(strsplit(strsplit(mapdata[val],"<br>|\\s+")[[1]][14],"</div>")[[1]][1])
  
  val=grep("S356",mapdata)
  S356=as.numeric(strsplit(strsplit(mapdata[val],"\\s+")[[1]][8],"</div>")[[1]][1])
  
  ENP=S12s+(S333+S333N)-S356
  
  val=grep("Istokpoga</a>",mapdata)
  Istok=strsplit(strsplit(mapdata[val],"\\s+")[[1]][6],"</div>")[[1]][1]
  
  val=grep("S-65E</a>",mapdata)
  S65E=strsplit(strsplit(mapdata[val],"\\s+")[[1]][6],"<br>")[[1]][1]
  
  val=grep("S-65EX1</a>",mapdata)
  S65EX1=strsplit(strsplit(mapdata[val],"\\s+")[[1]][6],"<br>")[[1]][1]
  
  val=grep("Fisheating Creek",mapdata)
  FEC=strsplit(strsplit(mapdata[val],"\\s+")[[1]][7],"</div>")[[1]][1]
  
  val=grep("/plots/s79h.pdf",mapdata)
  S79=strsplit(strsplit(mapdata[val],"\\s+")[[1]][6],"</a>")[[1]][1]
  # 
  # val=grep("/plots/s78h.pdf",mapdata)
  # S78=strsplit(strsplit(mapdata[val],"\\s+")[[1]][6],"</a>")[[1]][1]
  
  val=grep("/plots/s77",mapdata)
  S77=strsplit(strsplit(mapdata[val],"\\s+")[[1]][6],"</a>")[[1]][1]
  
  val=grep("/plots/s308",mapdata)
  S308=strsplit(strsplit(mapdata[val],"\\s+")[[1]][6],"</a>")[[1]][1]
  
  val=grep("/plots/s80",mapdata)
  S80=strsplit(strsplit(mapdata[val],"\\s+")[[1]][6],"</a>")[[1]][1]
  
  val=grep("S155A",mapdata);# Lake Worth Lagoon
  S155A=strsplit(strsplit(mapdata[val],"\\s+")[[1]][8],"</")[[1]][1]
  
  # other LOK inflows
  grep("inflows",mapdata)
  mapdata[213]
  tmp=strsplit(mapdata[213],"<br>|>")[[1]]
  OtherLOKInflow=sum(as.numeric(sapply(strsplit(tmp[grepl(" inflows",tmp)],":"),"[")[2,]))
  
  val=grep("../plots/ok8hhp.pdf",mapdata)
  LakeStage=as.numeric(strsplit(mapdata[val[1]],"\\s+")[[1]][6])
  
  # date.val=dates[i]-ddays(1)
  date.val=dates[i]
  rslt=data.frame(Date=date.val,
                  OtherLOKInflow=OtherLOKInflow,
                  FEC=as.numeric(FEC),
                  Istok=as.numeric(Istok),
                  S65E=as.numeric(S65E),
                  S65EX1=as.numeric(S65EX1),
                  S354=as.numeric(S354),
                  S351=as.numeric(S351),
                  S352=as.numeric(S352),
                  S271=as.numeric(S271),
                  S77=as.numeric(S77),
                  S79=as.numeric(S79),
                  S80=as.numeric(S80),
                  S308=as.numeric(S308),
                  WCA1=as.numeric(WCA1),
                  WCA2=as.numeric(WCA2),
                  WCA3=as.numeric(WCA3),
                  S10s=as.numeric(S10s),
                  S11s=as.numeric(S11s),
                  ENP=as.numeric(ENP),
                  S12s=as.numeric(S12s),
                  S333=as.numeric(S333),
                  S333N=as.numeric(S333N),
                  S356=as.numeric(S356),
                  S155A=as.numeric(S155A),
                  LOK.stage=as.numeric(LakeStage)
                  )
  map.q=rbind(map.q,rslt)
  print(i)
}


map.SDCS=data.frame()
for(i in 1:(length(dates))){
  map.url=paste0("https://w3.saj.usace.army.mil/h2o/reports/SDCSDaily/archive/",format(dates[i],"%m%d"),"/SDCSDaily.htm")
  # download.file(map.url, destfile = "map_dat.html", quiet=TRUE)
  mapdata=readLines(map.url)
  
  val=grep("L-29",mapdata)
  L29Canal=as.numeric(strsplit(mapdata[val],"Canal:|ft")[[1]][3])
  
  date.val=dates[i]
  rslt=data.frame(Date=date.val,L29Canal.map=L29Canal)
  map.SDCS=rbind(map.SDCS,rslt)
  print(i)
}

map.q[,2:ncol(map.q)]=sapply(map.q[,2:ncol(map.q)], FUN=function(x) ifelse(is.na(x)==T,0,x))

map.q$NthLake=rowSums(map.q[,c("FEC","Istok","S65E","S65EX1")],na.rm=T)
map.q$LOIN=rowSums(map.q[,c("OtherLOKInflow","FEC","Istok","S65E","S65EX1")],na.rm=T)
map.q$LOOUT=rowSums(map.q[,c("S77","S354","S351","S352","S271","S308")],na.rm=T)

map.q$LOK_to_EAA=(rowSums(map.q[,c("S354","S351","S352","S271")],na.rm=T))
map.q$EAA_to_WCAs=(rowSums(map.q[,c("WCA1","WCA2","WCA3")],na.rm=T))

subset(map.q,WCA2<0)
map.q$WCA2_in=with(map.q,ifelse(WCA2<0,0,WCA2))
map.q$EAA_WCA1.out=apply(map.q[,c("S10s","WCA1")],1,min,na.rm=T)# EAA flow through WCA1
map.q$EAA_WCA2.out=apply(map.q[,c("S11s","WCA2_in")],1,min,na.rm=T)# EAA flow through WCA2

map.q$EAA_WCA2=with(map.q,WCA2+EAA_WCA1.out)
map.q$EAA_WCA3=with(map.q,WCA3+EAA_WCA2.out)



# DBHydro -----------------------------------------------------------------

dates=c(Start.Date,End.Date)


## Lake Okeechobee ---------------------------------------------------------

comp.dbkey=data.frame(DBKEY=c("N3466","06832"),Priority=c("P2","P1"))

stg.da=data.frame()
for(i in 1:nrow(comp.dbkey)){
  tmp=DBHYDRO_daily(dates[1],dates[2],comp.dbkey$DBKEY[i])
  tmp$DBKEY=as.character(comp.dbkey$DBKEY[i])
  stg.da=rbind(stg.da,tmp)
}
stg.da=merge(stg.da,comp.dbkey,"DBKEY")
stg.da$DATE=date.fun(stg.da$Date)

LakeO.xtab=dcast(stg.da,DATE~Priority,value.var="Data.Value",mean)
LakeO.xtab$Mean=with(LakeO.xtab,ifelse(is.na(P1)==T,P2,P1))

LakeO.xtab=merge(LakeO.xtab,map.q[,c("Date","LOK.stage")],by.x="DATE",by.y="Date",all.x=T)
LakeO.xtab$LOK.stage=with(LakeO.xtab,ifelse(is.na(Mean)==T,LOK.stage,Mean))

## WCA1 --------------------------------------------------------------------
WCA1.dbkeys=data.frame(STATION=c("CA1-8T","CA1-9","CA1-7"),
                       DBKEY=c("15809","15811","15808"))
# data.frame(STATION="3GAvg",DBKEY="15943")

stg.da=data.frame()
for(i in 1:nrow(WCA1.dbkeys)){
  tmp=DBHYDRO_daily(dates[1],dates[2],WCA1.dbkeys$DBKEY[i])
  tmp$DBKEY=as.character(WCA1.dbkeys$DBKEY[i])
  stg.da=rbind(stg.da,tmp)
}
stg.da=merge(stg.da,WCA1.dbkeys,"DBKEY")
stg.da$DATE=date.fun(stg.da$Date)

WCA1.xtab=dcast(stg.da,DATE~STATION,value.var="Data.Value",mean)
WCA1.xtab$Avg.8T97=rowMeans(WCA1.xtab[,c("CA1-8T","CA1-9","CA1-7")],na.rm=T)

WCA1.xtab$DOWY=hydro.day(WCA1.xtab$DATE)
WCA1.xtab$WY=WY(WCA1.xtab$DATE)
WCA1.xtab$dec.WY=decimal.WY(WCA1.xtab$DATE)

# readLines("https://apps.sfwmd.gov/sfwmd/weatherdata/hydrographs/WCA1_In.html",warn=F)
WCA1.ZA1=jsonlite::read_json("https://apps.sfwmd.gov/sfwmd/weatherdata/hydrographs/data/WCA 1_ZONE A1.json",simplifyVector=T)
# ZA1=data.frame(date=unlist(sapply(WCA1.ZA1,"[",1)),A1=unlist(sapply(WCA1.ZA1,"[",2)))
ZA1=data.frame(date=WCA1.ZA1[,1],A1=WCA1.ZA1[,2])
ZA1$date=date.fun(as.POSIXct(ZA1$date/1000,origin="1970-01-01",tz="EST"))
# subset(ZA1,as.numeric(format(date,"%Y"))%in%c(2022)&A1<=15.75)
# subset(ZA1,as.numeric(format(date,"%Y"))%in%c(2022)&A1>=17.5)

WCA1A1=data.frame(month=c(5,7,9,12),day=c(12,8,24,1),A1=c(15.75,15.75,17.50,17.50))
# WCA1A1$date=with(WCA1A1,date.fun(paste(CurWY-1,month,day,sep="-")))
fill=data.frame(date=seq(date.fun(paste(CurWY-3,"01-01",sep="-")),date.fun(paste(CurWY+1,"04-30",sep="-")),"1 days"))
vals=c(-3,-2,-1,0,1)
WCA1A1.ts=data.frame()
for(i in 1:length(vals)){
  tmp=WCA1A1
  tmp$date=with(tmp,date.fun(paste(CurWY+vals[i],month,day,sep="-")))
  WCA1A1.ts=rbind(tmp,WCA1A1.ts)
}
WCA1A1.ts=merge(WCA1A1.ts,fill,"date",all.y=T)
WCA1A1.ts$A1=dat.interp(WCA1A1.ts$A1)
WCA1A1.ts$DOWY=hydro.day(WCA1A1.ts$date)
WCA1A1.ts$WY=WY(WCA1A1.ts$date)
WCA1A1.ts$dec.WY=decimal.WY(WCA1A1.ts$date)
WCA1A1.ts=WCA1A1.ts[order(WCA1A1.ts$dec.WY),]

##
WCA1.ZA2=jsonlite::read_json("https://apps.sfwmd.gov/sfwmd/weatherdata/hydrographs/data/WCA 1_ZONE A2.json",simplifyVector=T)
ZA2=data.frame(date=WCA1.ZA2[,1],A2=WCA1.ZA2[,2])
ZA2$date=date.fun(as.POSIXct(ZA2$date/1000,origin="1970-01-01",tz="EST"))
# subset(ZA2,as.numeric(format(date,"%Y"))%in%c(2022)&A2<=15.75)
# subset(ZA2,as.numeric(format(date,"%Y"))%in%c(2022)&A2>=17.0)

WCA1A2=data.frame(month=c(1,5,8,10),day=c(16,12,23,17),A2=c(17.00,15.75,15.75,17.00))
fill=data.frame(date=seq(date.fun(paste(CurWY-3,"01-01",sep="-")),date.fun(paste(CurWY+1,"04-30",sep="-")),"1 days"))
vals=c(-3,-2,-1,0,1)
WCA1A2.ts=data.frame()
for(i in 1:length(vals)){
  tmp=WCA1A2
  tmp$date=with(tmp,date.fun(paste(CurWY+vals[i],month,day,sep="-")))
  WCA1A2.ts=rbind(tmp,WCA1A2.ts)
}
WCA1A2.ts=merge(WCA1A2.ts,fill,"date",all.y=T)
WCA1A2.ts$A2=dat.interp(WCA1A2.ts$A2)
WCA1A2.ts$DOWY=hydro.day(WCA1A2.ts$date)
WCA1A2.ts$WY=WY(WCA1A2.ts$date)
WCA1A2.ts$dec.WY=decimal.WY(WCA1A2.ts$date)
WCA1A2.ts=WCA1A2.ts[order(WCA1A2.ts$dec.WY),]

wsfloor=14

lwd.val=1
xlim.vals=date.fun(c(paste(CurWY-1,05,01,sep="-"),paste(CurWY,04,30,sep="-")))
xmaj=seq(xlim.vals[1],xlim.vals[2],by="2 months");xmin=seq(xlim.vals[1],xlim.vals[2],by="1 months")
xlim.vals2= hydro.day(xlim.vals)# decimal.WY(xlim.vals)#

ylim.val=c(13,18);by.y=1
ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

par(family="serif",mar=c(0.5,2,1,1),oma=c(0.5,3,1,1));
layout(matrix(1:2,2,1,byrow=T),heights=c(1,0.25))
plot(A1~DOWY,WCA1A1.ts,ylim=ylim.val,xlim=xlim.vals2,type="n",ann=F,axes=F,yaxs="i",xaxs="i")
abline(h=ymaj,v=hydro.day(xmin),lwd=1,col="grey",lty=3)
lines(A2~DOWY,subset(WCA1A2.ts,WY==CurWY),col="purple",lwd=1.5)
lines(A1~DOWY,subset(WCA1A1.ts,WY==CurWY),col="red",lwd=1.5)
lines(Avg.8T97~DOWY,subset(WCA1.xtab,WY==CurWY-2),lwd=2,col=adjustcolor("forestgreen",0.5),lty=1)
lines(Avg.8T97~DOWY,subset(WCA1.xtab,WY==CurWY-1),lwd=2,col=adjustcolor("blue",0.5),lty=1)
lines(Avg.8T97~DOWY,subset(WCA1.xtab,WY==CurWY),lwd=2,col="red",lty=1)

if(is.na(subset(WCA1.xtab,DATE==YEST)$Avg.8T97)==T|nrow(subset(WCA1.xtab,DATE==YEST))==0){NA}else{
  with(subset(WCA1.xtab,DATE==YEST),points(DOWY,Avg.8T97,pch=21,bg=adjustcolor("red",0.5),col="red",lwd=0.1,cex=1.25))
  with(subset(WCA1.xtab,DATE==YEST),segments(DOWY,Avg.8T97,DOWY,14.1,lty=2))
  with(subset(WCA1.xtab,DATE==YEST),text(DOWY,14,paste(format(round(Avg.8T97,2),nsmall=2),"Ft"),cex=0.8,xpd=NA))
}
axis_fun(1,hydro.day(xmaj),hydro.day(xmin),format(xmaj,"%b"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj)
box(lwd=1)
mtext(side=1,"Month",line=2,cex=1.25)
mtext(side=2,"Stage Elevation (Feet, NGVD29)",line=2.5,cex=1.25)
mtext(side=3,adj=0,paste("Date:",format(date.fun(Sys.Date()-ddays(1)),"%b %d, %Y")))
mtext(side=3,adj=1,"Data are provisional and subject to change",col="red")

plot(0:1,0:1,type = 'n', axes = F,xlab=NA, ylab=NA)
legend(0.5,0.5,
       legend=c(paste0("WY",CurWY),paste0("WY",CurWY-1),paste0("WY",CurWY-2)),
       col=c("red",adjustcolor(c("blue","forestgreen"),0.5)),lty=c(1,1),lwd=c(4,4,4),ncol=3,cex=1,bty="n",y.intersp=1,x.intersp=0.5,xpd=NA,xjust=0.5)



## WCA2 --------------------------------------------------------------------
WCA2.dbkeys=data.frame(STATION=c("S11B_H","CA217"),
                       DBKEY=c("WN126","16531"))
# data.frame(STATION="3GAvg",DBKEY="15943")

stg.da=data.frame()
for(i in 1:nrow(WCA2.dbkeys)){
  tmp=DBHYDRO_daily(dates[1],dates[2],WCA2.dbkeys$DBKEY[i])
  tmp$DBKEY=as.character(WCA2.dbkeys$DBKEY[i])
  stg.da=rbind(stg.da,tmp)
}
stg.da=merge(stg.da,WCA2.dbkeys,"DBKEY")
stg.da$DATE=date.fun(stg.da$Date)

WCA2.xtab=dcast(stg.da,DATE~STATION,value.var="Data.Value",mean)

WCA2.xtab$recess_7day=with(WCA2.xtab,c(rep(NA,7),diff(CA217,lag=7)))
WCA2.xtab$recess_30day=with(WCA2.xtab,c(rep(NA,30),diff(CA217,lag=30)))
WCA2.xtab$DoY=as.numeric(format(WCA2.xtab$DATE,"%j"))
WCA2.xtab$CY=as.numeric(format(WCA2.xtab$DATE,"%Y"))
WCA2.xtab$DOWY=hydro.day(WCA2.xtab$DATE)
WCA2.xtab$WY=WY(WCA2.xtab$DATE)



# readLines("https://apps.sfwmd.gov/sfwmd/weatherdata/hydrographs/WCA2A_In.html",warn=F)
WCA2.ZA=jsonlite::read_json("https://apps.sfwmd.gov/sfwmd/weatherdata/hydrographs/data/WCA 2A_ZONE A.json",simplifyVector=T)
ZA=data.frame(date=WCA2.ZA[,1],ZoneA=WCA2.ZA[,2])
ZA$date=date.fun(as.POSIXct(ZA$date/1000,origin="1970-01-01",tz="EST"))
subset(ZA,as.numeric(format(date,"%Y"))%in%c(2022)&ZoneA<=11)
subset(ZA,as.numeric(format(date,"%Y"))%in%c(2022)&ZoneA>=13)


WCA2A=data.frame(month=c(1,6,9),day=c(31,30,30),ZoneA=c(11,11,13))
fill=data.frame(date=seq(date.fun(paste(CurWY-3,"01-01",sep="-")),date.fun(paste(CurWY+1,"04-30",sep="-")),"1 days"))
vals=c(-3,-2,-1,0,1)
WCA2A.ts=data.frame()
for(i in 1:length(vals)){
  tmp=WCA2A
  tmp$date=with(tmp,date.fun(paste(CurWY+vals[i],month,day,sep="-")))
  WCA2A.ts=rbind(tmp,WCA2A.ts)
}
WCA2A.ts=merge(WCA2A.ts,fill,"date",all.y=T)
WCA2A.ts$ZoneA=dat.interp(WCA2A.ts$ZoneA)
WCA2A.ts$DOWY=hydro.day(WCA2A.ts$date)
WCA2A.ts$WY=WY(WCA2A.ts$date)
WCA2A.ts$dec.WY=decimal.WY(WCA2A.ts$date)
WCA2A.ts=WCA2A.ts[order(WCA2A.ts$dec.WY),]



xlim.vals=date.fun(c(paste(CurWY-1,05,01,sep="-"),paste(CurWY,04,30,sep="-")))
xmaj=seq(xlim.vals[1],xlim.vals[2],by="2 months");xmin=seq(xlim.vals[1],xlim.vals[2],by="1 months")
xlim.vals2= hydro.day(xlim.vals)# decimal.WY(xlim.vals)#

ylim.val=c(10,15);by.y=1
ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

par(family="serif",mar=c(0.5,2,1,1),oma=c(0.5,3,1,1));
layout(matrix(1:2,2,1,byrow=T),heights=c(1,0.25))
plot(ZoneA~DOWY,WCA2A.ts,ylim=ylim.val,xlim=xlim.vals2,type="n",ann=F,axes=F,yaxs="i",xaxs="i")
abline(h=ymaj,v=hydro.day(xmin),lwd=1,col="grey",lty=3)
lines(ZoneA~DOWY,subset(WCA2A.ts,WY==CurWY),col="red",lwd=1.5)

lines(CA217~DOWY,subset(WCA2.xtab,WY==CurWY-2),lwd=2,col=adjustcolor("forestgreen",0.5),lty=1)
lines(CA217~DOWY,subset(WCA2.xtab,WY==CurWY-1),lwd=2,col=adjustcolor("blue",0.5),lty=1)
lines(CA217~DOWY,subset(WCA2.xtab,WY==CurWY),lwd=2,col="red",lty=1)

lines(S11B_H~DOWY,subset(WCA2.xtab,WY==CurWY-2),lwd=1,col=adjustcolor("forestgreen",0.5),lty=2)
lines(S11B_H~DOWY,subset(WCA2.xtab,WY==CurWY-1),lwd=1,col=adjustcolor("blue",0.5),lty=2)
lines(S11B_H~DOWY,subset(WCA2.xtab,WY==CurWY),lwd=1,col="red",lty=2)



## WCA3 --------------------------------------------------------------------
WCA3.dbkeys=data.frame(STATION=c("CA3-62","CA3-63","CA3-64","CA3-65"),
                       DBKEY=c("16536","16532","16537","16538"))
# data.frame(STATION="3GAvg",DBKEY="15943")

stg.da=data.frame()
for(i in 1:nrow(WCA3.dbkeys)){
  tmp=DBHYDRO_daily(dates[1],dates[2],WCA3.dbkeys$DBKEY[i])
  tmp$DBKEY=as.character(WCA3.dbkeys$DBKEY[i])
  stg.da=rbind(stg.da,tmp)
}
stg.da=merge(stg.da,WCA3.dbkeys,"DBKEY")
stg.da$DATE=date.fun(stg.da$Date)

WCA3.xtab=dcast(stg.da,DATE~STATION,value.var="Data.Value",mean)
WCA3.xtab$Avg.636465=rowMeans(WCA3.xtab[,c("CA3-63","CA3-64","CA3-65")],na.rm=T)
WCA3.xtab$Avg.6263=rowMeans(WCA3.xtab[,c("CA3-62","CA3-63")],na.rm=T)

plot(Avg.636465~DATE,WCA3.xtab)
# FWCC Action Line:                         11.6
# FWCC Muck Fire Closure                    9.30
# Taylor Slough Water Deliveries Floor      9.80
WCA3.xtab$recess_7day=with(WCA3.xtab,c(rep(NA,7),diff(Avg.636465,lag=7)))
WCA3.xtab$recess_30day=with(WCA3.xtab,c(rep(NA,30),diff(Avg.636465,lag=30)))
WCA3.xtab$DoY=as.numeric(format(WCA3.xtab$DATE,"%j"))
WCA3.xtab$CY=as.numeric(format(WCA3.xtab$DATE,"%Y"))
WCA3.xtab$DOWY=hydro.day(WCA3.xtab$DATE)
WCA3.xtab$WY=WY(WCA3.xtab$DATE)


EHWL=data.frame(Date.EST=date.fun(c("2023-01-01","2023-05-01","2023-10-31","2023-12-31")),
                val=c(12,11,12,12))
EHWL$DOY=as.numeric(format(EHWL$Date.EST,"%j"))

ZoneA=data.frame(Date.EST=date.fun(c("2023-01-01","2023-05-31","2023-10-31","2023-12-31")),
                 val=c(10.49,9.5,10.50,10.5))
ZoneA$DOY=as.numeric(format(EHWL$Date.EST,"%j"))

# readLines("https://apps.sfwmd.gov/sfwmd/weatherdata/hydrographs/WCA3A_In.html",warn=F)
WCA3.EML=jsonlite::read_json("https://apps.sfwmd.gov/sfwmd/weatherdata/hydrographs/data/WCA 3A_COP EXTREME HIGH WATER LINE TMP.json",simplifyVector=T)
WCA3=data.frame(date=WCA3.EML[,1],ZoneA=WCA3.EML[,2])
WCA3$date=date.fun(as.POSIXct(WCA3$date/1000,origin="1970-01-01",tz="EST"))

subset(WCA3,as.numeric(format(date,"%Y"))%in%c(2022)&ZoneA<=11)
subset(WCA3,as.numeric(format(date,"%Y"))%in%c(2022)&ZoneA>=12)

EMWL=data.frame(month=c(5,10,12),day=c(30,30,30),zone=c(11,12,12))
fill=data.frame(date=seq(date.fun(paste(CurWY-3,"01-01",sep="-")),date.fun(paste(CurWY+1,"04-30",sep="-")),"1 days"))
vals=c(-3,-2,-1,0,1)
EMWL.ts=data.frame()
for(i in 1:length(vals)){
  tmp=EMWL
  tmp$date=with(tmp,date.fun(paste(CurWY+vals[i],month,day,sep="-")))
  EMWL.ts=rbind(tmp,EMWL.ts)
}
EMWL.ts=merge(EMWL.ts,fill,"date",all.y=T)
EMWL.ts$zone=dat.interp(EMWL.ts$zone)
EMWL.ts$DOWY=hydro.day(EMWL.ts$date)
EMWL.ts$WY=WY(EMWL.ts$date)
EMWL.ts$dec.WY=decimal.WY(EMWL.ts$date)
EMWL.ts=EMWL.ts[order(EMWL.ts$dec.WY),]


WCA3.ZA=jsonlite::read_json("https://apps.sfwmd.gov/sfwmd/weatherdata/hydrographs/data/WCA 3A_ZONE A.json",simplifyVector=T)
WCA3.ZA=data.frame(date=WCA3.ZA[,1],Zone=WCA3.ZA[,2])
WCA3.ZA$date=date.fun(as.POSIXct(WCA3.ZA$date/1000,origin="1970-01-01",tz="EST"))

subset(WCA3.ZA,as.numeric(format(date,"%Y"))%in%c(2022)&Zone<=9.5)
subset(WCA3.ZA,as.numeric(format(date,"%Y"))%in%c(2022)&Zone>=10.5)

WCA3.ZA=data.frame(month=c(5,10,12),day=c(30,30,30),zone=c(9.5,10.5,10.5))
fill=data.frame(date=seq(date.fun(paste(CurWY-3,"01-01",sep="-")),date.fun(paste(CurWY+1,"04-30",sep="-")),"1 days"))
vals=c(-3,-2,-1,0,1)
WCA3.ZA.ts=data.frame()
for(i in 1:length(vals)){
  tmp=WCA3.ZA
  tmp$date=with(tmp,date.fun(paste(CurWY+vals[i],month,day,sep="-")))
  WCA3.ZA.ts=rbind(tmp,WCA3.ZA.ts)
}
WCA3.ZA.ts=merge(WCA3.ZA.ts,fill,"date",all.y=T)
WCA3.ZA.ts$zone=dat.interp(WCA3.ZA.ts$zone)
WCA3.ZA.ts$DOWY=hydro.day(WCA3.ZA.ts$date)
WCA3.ZA.ts$WY=WY(WCA3.ZA.ts$date)
WCA3.ZA.ts$dec.WY=decimal.WY(WCA3.ZA.ts$date)
WCA3.ZA.ts=WCA3.ZA.ts[order(WCA3.ZA.ts$dec.WY),]


xlim.vals=date.fun(c(paste(CurWY-1,05,01,sep="-"),paste(CurWY,04,30,sep="-")))
xmaj=seq(xlim.vals[1],xlim.vals[2],by="2 months");xmin=seq(xlim.vals[1],xlim.vals[2],by="1 months")
xlim.vals2= hydro.day(xlim.vals)# decimal.WY(xlim.vals)#

ylim.val=c(6,13);by.y=1
ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

par(family="serif",mar=c(0.5,2,1,1),oma=c(0.5,3,1,1));
layout(matrix(1:2,2,1,byrow=T),heights=c(1,0.25))
plot(zone~DOWY,WCA3.ZA.ts,ylim=ylim.val,xlim=xlim.vals2,type="n",ann=F,axes=F,yaxs="i",xaxs="i")
abline(h=ymaj,v=hydro.day(xmin),lwd=1,col="grey",lty=3)
lines(zone~DOWY,subset(WCA3.ZA.ts,WY==CurWY),col="black",lwd=1.5)
lines(zone~DOWY,subset(EMWL.ts,WY==CurWY),col="black",lwd=1.5,lty=3)

lines(Avg.636465~DOWY,subset(WCA3.xtab,WY==CurWY-2),lwd=2,col=adjustcolor("forestgreen",0.5),lty=1)
lines(Avg.636465~DOWY,subset(WCA3.xtab,WY==CurWY-1),lwd=2,col=adjustcolor("blue",0.5),lty=1)
lines(Avg.636465~DOWY,subset(WCA3.xtab,WY==CurWY),lwd=2,col="red",lty=1)

lines(Avg.6263~DOWY,subset(WCA3.xtab,WY==CurWY-2),lwd=1,col=adjustcolor("forestgreen",0.5),lty=2)
lines(Avg.6263~DOWY,subset(WCA3.xtab,WY==CurWY-1),lwd=1,col=adjustcolor("blue",0.5),lty=2)
lines(Avg.6263~DOWY,subset(WCA3.xtab,WY==CurWY),lwd=1,col="red",lty=2)
abline(h=c(11.6,9.3))

# FWCC Action Line:                         11.6
# FWCC Muck Fire Closure                    9.30
# Taylor Slough Water Deliveries Floor      9.80
## ENP --------------------------------------------------------------------
ENP.dbkeys=data.frame(STATION=c("NESRS2","NP201","L29Canal"),
                       DBKEY=c("01218","06719","00627"))
# data.frame(STATION="3GAvg",DBKEY="15943")

stg.da=data.frame()
for(i in 1:nrow(ENP.dbkeys)){
  tmp=DBHYDRO_daily(dates[1],dates[2],ENP.dbkeys$DBKEY[i])
  tmp$DBKEY=as.character(ENP.dbkeys$DBKEY[i])
  stg.da=rbind(stg.da,tmp)
}
stg.da=merge(stg.da,ENP.dbkeys,"DBKEY")
stg.da$DATE=date.fun(stg.da$Date)

ENP.xtab=dcast(stg.da,DATE~STATION,value.var="Data.Value",mean)

tail(ENP.xtab)
# colnames(map.SDCS)=c("DATE","L29Canal.map")
ENP.xtab=merge(ENP.xtab,map.SDCS,by.x="DATE",by.y="Date",all.x=T)
ENP.xtab$L29Canal.f=with(ENP.xtab,ifelse(is.na(L29Canal)==T|is.nan(L29Canal)==T,L29Canal.map,L29Canal))

plot(L29Canal.f~DATE,ENP.xtab,type="l")

# WCA3A.PET=DBHYDRO_daily(dates[1],dates[2],c("US347","LA375"))
# https://troyhill.github.io/TTFF/#:~:text=The%20Tamiami%20Trail%20Flow%20Formula,S333%2C%20S333N%2C%20and%20S334.

ENP.xtab$recess_7day=with(ENP.xtab,c(rep(NA,7),diff(L29Canal.f,lag=7)))
ENP.xtab$recess_30day=with(ENP.xtab,c(rep(NA,30),diff(L29Canal.f,lag=30)))
ENP.xtab$DoY=as.numeric(format(ENP.xtab$DATE,"%j"))
ENP.xtab$CY=as.numeric(format(ENP.xtab$DATE,"%Y"))
ENP.xtab$DOWY=hydro.day(ENP.xtab$DATE)
ENP.xtab$WY=WY(ENP.xtab$DATE)


xlim.vals=date.fun(c(paste(CurWY-1,05,01,sep="-"),paste(CurWY,04,30,sep="-")))
xmaj=seq(xlim.vals[1],xlim.vals[2],by="2 months");xmin=seq(xlim.vals[1],xlim.vals[2],by="1 months")
xlim.vals2= hydro.day(xlim.vals)# decimal.WY(xlim.vals)#


par(family="serif",mar=c(0.5,2,1,1),oma=c(0.5,3,1,1));
layout(matrix(1:4,2,2,byrow=T),heights=c(1,0.25))

ylim.val=c(6,10);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

plot(L29Canal.f~DOWY,ENP.xtab,ylim=ylim.val,xlim=xlim.vals2,type="n",ann=F,axes=F,yaxs="i",xaxs="i")
abline(h=ymaj,v=hydro.day(xmin),lwd=1,col="grey",lty=3)
lines(L29Canal.f~DOWY,subset(ENP.xtab,WY==CurWY-2),lwd=2,col=adjustcolor("forestgreen",0.5),lty=1)
lines(L29Canal.f~DOWY,subset(ENP.xtab,WY==CurWY-1),lwd=2,col=adjustcolor("blue",0.5),lty=1)
lines(L29Canal.f~DOWY,subset(ENP.xtab,WY==CurWY),lwd=2,col="red",lty=1)
axis_fun(1,hydro.day(xmaj),hydro.day(xmin),format(xmaj,"%b"),line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)

ylim.val=c(6,10);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(L29Canal.f~DOWY,ENP.xtab,ylim=ylim.val,xlim=xlim.vals2,type="n",ann=F,axes=F,yaxs="i",xaxs="i")
abline(h=ymaj,v=hydro.day(xmin),lwd=1,col="grey",lty=3)
lines(NESRS2~DOWY,subset(ENP.xtab,WY==CurWY-2),lwd=2,col=adjustcolor("forestgreen",0.5),lty=1)
lines(NESRS2~DOWY,subset(ENP.xtab,WY==CurWY-1),lwd=2,col=adjustcolor("blue",0.5),lty=1)
lines(NESRS2~DOWY,subset(ENP.xtab,WY==CurWY),lwd=2,col="red",lty=1)

lines(NP201~DOWY,subset(ENP.xtab,WY==CurWY-2),lwd=1.5,col=adjustcolor("forestgreen",0.5),lty=2)
lines(NP201~DOWY,subset(ENP.xtab,WY==CurWY-1),lwd=1.5,col=adjustcolor("blue",0.5),lty=2)
lines(NP201~DOWY,subset(ENP.xtab,WY==CurWY),lwd=1.5,col="red",lty=2)

axis_fun(1,hydro.day(xmaj),hydro.day(xmin),format(xmaj,"%b"),line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)

labs.val=c(paste0("WY",c(CurWY,CurWY-1,CurWY-2)," L29 Canal"))
cols.vals=c("red",adjustcolor(c("blue","forestgreen"),0.5))
lty.vals=c(1,1,1)
lwd.vals=c(1,1,1)
plot(0:1,0:1,type = 'n', axes = F,xlab=NA, ylab=NA)
legend(0.5,0.5,
       legend=labs.val,
       col=cols.vals,
       lty=lty.vals,lwd=lwd.vals,
       ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.5,xpd=NA,xjust=0.5)

labs.val=c(paste0("WY",c(CurWY,CurWY-1,CurWY-2)," NESRS2"), paste0("WY",c(CurWY,CurWY-1,CurWY-2)," NP201"))
cols.vals=rep(c("red",adjustcolor(c("blue","forestgreen"),0.5)),2)
lty.vals=c(1,1,1,2,2,2)
lwd.vals=c(1,1,1)
plot(0:1,0:1,type = 'n', axes = F,xlab=NA, ylab=NA)
legend(0.5,0.5,
       legend=labs.val,
       col=cols.vals,
       lty=lty.vals,lwd=lwd.vals,
       ncol=2,cex=1,bty="n",y.intersp=1,x.intersp=0.5,xpd=NA,xjust=0.5)

## CRE ---------------------------------------------------------------------
cal.flow=data.frame(DBKEY=c("DJ235","88280","DJ237","00865"),SITE=c(rep("S77",2),rep("S79",2)),priority=rep(c("P1","P2"),2))
cal.flow.dat=data.frame()
for(i in 1:nrow(cal.flow)){
  tmp=DBHYDRO_daily(date.fun(paste(CurWY-3,"05-01",sep="-")),End.Date,cal.flow$DBKEY[i])
  tmp$DBKEY=as.character(cal.flow$DBKEY[i])
  cal.flow.dat=rbind(tmp,cal.flow.dat)
}
cal.flow.dat$Date=date.fun(cal.flow.dat$Date)
cal.flow.dat$WY=WY(cal.flow.dat$Date)
cal.flow.dat=merge(cal.flow.dat,cal.flow,"DBKEY")
cal.flow.dat.xtab=dcast(cal.flow.dat,SITE+Date+WY~priority,value.var="Data.Value",mean,na.rm=T)
cal.flow.dat.xtab$Data.Value=with(cal.flow.dat.xtab,ifelse(is.na(P1)==T,P2,P1))

cal.flow.dat.xtab=dcast(cal.flow.dat.xtab,Date+WY~SITE,value.var="Data.Value",mean)
tmp=map.q[,c("Date","S77","S79")]
colnames(tmp)=c("Date","S77.map","S79.map")
cal.flow.dat.xtab=merge(cal.flow.dat.xtab,tmp,"Date",all.x=T)
cal.flow.dat.xtab$S77=with(cal.flow.dat.xtab,ifelse(is.na(S77),S77.map,S77))
cal.flow.dat.xtab$S79=with(cal.flow.dat.xtab,ifelse(is.na(S79),S79.map,S79))

cal.flow.dat.xtab$hydro.day=hydro.day(cal.flow.dat.xtab$Date)
cal.flow.dat.xtab$S77=with(cal.flow.dat.xtab,ifelse(S77<0,0,S77))
cal.flow.dat.xtab$C43=with(cal.flow.dat.xtab,ifelse(S79<S77,0,S79-S77))

cal.flow.dat.xtab$cum.S79=with(cal.flow.dat.xtab,ave(cfs.to.acftd(S79),WY,FUN = function(x)cumsum(ifelse(is.na(x),0,x))))
cal.flow.dat.xtab$cum.S77=with(cal.flow.dat.xtab,ave(cfs.to.acftd(S77),WY,FUN = function(x)cumsum(ifelse(is.na(x),0,x))))
cal.flow.dat.xtab$Q.14=with(cal.flow.dat.xtab,roll_meanr(S79,n=14))
# cal.flow.dat.xtab$Q.30=with(cal.flow.dat.xtab,roll_meanr(S79,n=30))
cal.flow.dat.xtab$SalEnv.cat=with(cal.flow.dat.xtab,ifelse(Q.14<750,"low",
                                                           ifelse(Q.14>=750&Q.14<2100,"optimum",
                                                                  ifelse(Q.14>=2100&Q.14<2600,"stress",
                                                                         ifelse(Q.14>2600,"damaging",NA)))))


consec.startend=function(var){
  runs=rle(var)
  myruns = which(runs$values == TRUE)
  runs.lengths.cumsum = cumsum(runs$lengths)
  ends = runs.lengths.cumsum[myruns]
  newindex = ifelse(myruns>1, myruns-1, 0)
  starts = runs.lengths.cumsum[newindex] + 1
  if (0 %in% newindex) starts = c(1,starts)
  rslt=list(starts=starts,ends=ends)
  return(rslt)
}

cal.flow.dat.xtab$Low=with(cal.flow.dat.xtab,ifelse(Q.14<750,1,0))
cal.flow.dat.xtab$Opt=with(cal.flow.dat.xtab,ifelse(Q.14>=750&Q.14<2100,1,0))
cal.flow.dat.xtab$Stress=with(cal.flow.dat.xtab,ifelse(Q.14>=2100&Q.14<2600,1,0))
cal.flow.dat.xtab$Dam=with(cal.flow.dat.xtab,ifelse(Q.14>=2600,1,0))

cal.flow.dat.xtab$Low.consec=0
for(i in 2:nrow(cal.flow.dat.xtab)){
  cal.flow.dat.xtab$Low.consec[i]=with(cal.flow.dat.xtab,ifelse(Low[i-1]==0&Low[i]>0,1,
                                            ifelse(Low[i-1]>0&Low[i]>0,1,0)))
}
Low.consec.val=consec.startend(cal.flow.dat.xtab$Low.consec)
cal.flow.dat.xtab$sum.Low.consec=0
if(length(Low.consec.val$ends)!=0){
  for(i in 1:length(Low.consec.val$ends)){
    cal.flow.dat.xtab[Low.consec.val$ends[i],]$sum.Low.consec=with(cal.flow.dat.xtab[c(Low.consec.val$starts[i]:Low.consec.val$ends[i]),],sum(Low.consec,na.rm=T))
  }
}
#
cal.flow.dat.xtab$Opt.consec=0
for(i in 2:nrow(cal.flow.dat.xtab)){
  cal.flow.dat.xtab$Opt.consec[i]=with(cal.flow.dat.xtab,ifelse(Opt[i-1]==0&Opt[i]>0,1,
                                            ifelse(Opt[i-1]>0&Opt[i]>0,1,0)))
}
Opt.consec.val=consec.startend(cal.flow.dat.xtab$Opt.consec)
#
cal.flow.dat.xtab$sum.Opt.consec=0
if(length(Opt.consec.val$ends)!=0){
  for(i in 1:length(Opt.consec.val$ends)){
    cal.flow.dat.xtab[Opt.consec.val$ends[i],]$sum.Opt.consec=with(cal.flow.dat.xtab[c(Opt.consec.val$starts[i]:Opt.consec.val$ends[i]),],sum(Opt.consec,na.rm=T))
  }
}
# 
cal.flow.dat.xtab$Stress.consec=0
for(i in 2:nrow(cal.flow.dat.xtab)){
  cal.flow.dat.xtab$Stress.consec[i]=with(cal.flow.dat.xtab,ifelse(Stress[i-1]==0&Stress[i]>0,1,
                                               ifelse(Stress[i-1]>0&Stress[i]>0,1,0)))
}
Stress.consec.val=consec.startend(cal.flow.dat.xtab$Stress.consec)
cal.flow.dat.xtab$sum.Stress.consec=0
if(length(Stress.consec.val$ends)!=0){
  for(i in 1:length(Stress.consec.val$ends)){
    cal.flow.dat.xtab[Stress.consec.val$ends[i],]$sum.Stress.consec=with(cal.flow.dat.xtab[c(Stress.consec.val$starts[i]:Stress.consec.val$ends[i]),],sum(Stress.consec,na.rm=T))
  }
}
#
cal.flow.dat.xtab$Dam.consec=0
for(i in 2:nrow(cal.flow.dat.xtab)){
  cal.flow.dat.xtab$Dam.consec[i]=with(cal.flow.dat.xtab,ifelse(Dam[i-1]==0&Dam[i]>0,1,
                                            ifelse(Dam[i-1]>0&Dam[i]>0,1,0)))
}
Dam.consec.val=consec.startend(cal.flow.dat.xtab$Dam.consec)
cal.flow.dat.xtab$sum.Dam.consec=0
if(length(Dam.consec.val$ends)!=0){
  for(i in 1:length(Dam.consec.val$ends)){
    cal.flow.dat.xtab[Dam.consec.val$ends[i],]$sum.Dam.consec=with(cal.flow.dat.xtab[c(Dam.consec.val$starts[i]:Dam.consec.val$ends[i]),],sum(Dam.consec,na.rm=T))
  }
}
# cal.flow.dat.xtab=subset(cal.flow.dat.xtab,is.na(Q.14)==F)
salenv.vals=subset(cal.flow.dat.xtab,Date==max(cal.flow.dat.xtab$Date))

salenv.vals1=salenv.vals[,paste("sum",c("Low","Opt","Stress","Dam"),"consec",sep=".")]
colnames(salenv.vals1)=c("low","optimal","stress","damaging")

flow.envs=c("< 750","750 - 2100", "2100 - 2600","> 2600")
cols=c("red","green","orange","red")


max.q=max(subset(cal.flow.dat.xtab,Date%in%seq(End.Date-ddays(30),End.Date+ddays(3),"1 days"))$S79,na.rm=T)
ylim.max=round(max.q+max.q*0.25,-3)

ylim.val=c(0,ylim.max);by.y=ylim.max/4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c(End.Date-ddays(30),End.Date+ddays(3)));xmaj=seq(xlim.val[1],xlim.val[2],by="7 days");xmin=seq(xlim.val[1],xlim.val[2],by="1 days")

layout(matrix(1:2,2,1,byrow=F),widths=c(2,1),heights=c(1,0.5))
par(family="serif",cex.axis=1.2,mar=c(2,2,1,2),oma=c(0.5,3,1,2));
plot(S79~Date,cal.flow.dat.xtab,type="n",ylim=ylim.val,xlim=xlim.val,xaxs="i",yaxs="i",xaxt="n",yaxt="n",xlab=NA,ylab=NA)
xx=c(xlim.val[1],xlim.val[1],xlim.val[2],xlim.val[2])
yy3=c(2600,ylim.max,ylim.max,2600);polygon(x=xx,y=yy3,col=adjustcolor("red",0.5))
yy4=c(2100,2600,2600,2100);polygon(x=xx,y=yy4,col=adjustcolor("yellow",0.5))
yy5=c(750,2100,2100,750);polygon(x=xx,y=yy5,col=adjustcolor("green",0.5))
abline(h=ymaj,v=xmaj,lty=3,col="grey")
abline(h=457,col="red")
with(cal.flow.dat.xtab,pt_line(Date,S79,2,"black",1,21,"grey",cex=1.25,pt.lwd=0.1))
with(cal.flow.dat.xtab,lines(Date,Q.14,lwd=3,col=adjustcolor("dodgerblue3",0.5)))
axis_fun(1,xmaj,xmin,format(xmaj,"%m/%d/%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj,cex.axis=1)
axis_fun(4,ymaj,ymin,format(round(cfs.to.acftd(ymaj)/1000,1)),cex.axis=1)

box(lwd=lwd.val)
mtext("Date\n(MM/DD/YYYY)",side=1,line=2.75,cex=1.25)
mtext("Discharge (cfs)",side=2,line=3.5,cex=1.25)
mtext("Discharge (kAc-Ft D\u207B\u00B9)",side=4,line=2.5,cex=1.25)

plot(0:1,0:1,type="n",yaxt="n",xaxt="n",bty="n")
legend(0.25,0.75,
       legend=c( "Damaging (>2600 cfs)","Stress (2100 - 2600 cfs)","Optimum (750 - 2100 cfs)"),pch=22,
       pt.bg=adjustcolor(c("red","yellow","green"),0.5),pt.cex=2,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,title.adj = 0,title='RECOVER PM')

legend(0.75,0.75,
       legend=c("Daily Discharge","14-Day moving average"),
       pch=c(21,NA),pt.bg=c("grey",NA),pt.cex=c(1.5,NA),
       lty=c(NA,1),lwd=c(0.5,2),col=c("black",adjustcolor(c("dodgerblue1"),0.5)),
       ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=1,xpd=NA,xjust=0.5,title.adj = 0,title='')




## Salinity
dates2=c(as.Date(End.Date-duration(30,"days")),End.Date)
bk.dbkeys.bot=data.frame(depth="bottom",
                         SITE=c(rep("VALI75",2),rep("FORTMYERSM",2),rep("CCORAL",2),rep("MARKH",2),rep("SANIB2",2)),
                         param=rep(c("WT","SPC"),5),
                         DBKEY=c("UL030","UL026","88288","88291","UO832","AJ012","88198","88202","WN375","WN377")
)
bk.dbkeys.bot=subset(bk.dbkeys.bot,SITE!="MARKH")
bk.dbkeys.bot=rbind(bk.dbkeys.bot,
                    data.frame(depth="bottom",
                               SITE=c(rep("MARKH",2)),
                               param=c("WT","SPC"),
                               DBKEY=c("WZ152","WZ156")
                    ));# Adds site MARKHA - temporary alterantive site for MARKH

bk.dbkeys.bot=subset(bk.dbkeys.bot,DBKEY!="UO832");# temperature data not reporting
bk.dbkeys.bot=rbind(bk.dbkeys.bot,
                    data.frame(depth="top",
                               SITE=c("CCORAL"),
                               param=c("WT"),
                               DBKEY=c("UO834")
                    ));# using top water temp as proxy to calculate salinity
bk.dbkeys.bot=subset(bk.dbkeys.bot,SITE!="VALI75")
sal.dat=data.frame()
for(i in 1:nrow(bk.dbkeys.bot)){
  tmp=DBHYDRO_breakpoint(dates2[1],dates2[2],bk.dbkeys.bot$DBKEY[i])
  if(nrow(tmp)==0) next
  tmp$DBKEY=bk.dbkeys.bot$DBKEY[i]
  sal.dat=rbind(sal.dat,tmp)
  print(i)
}
sal.dat=merge(sal.dat,bk.dbkeys.bot,"DBKEY")
sal.dat$Date.EST=date.fun(sal.dat$DATETIME)
sal.dat$Data.Value[sal.dat$Data.Value==-999]<-NA
sal.dat$Data.Value[sal.dat$param=="SPC"&sal.dat$Data.Value<300]<-NA
#subset(sal.dat,Data.Value<0)
da.screen=ddply(sal.dat,c("Date.EST","SITE"),summarise,N.val=N.obs(Data.Value[param=="SPC"]))
da.screen$screen=with(da.screen,ifelse(N.val<20,0,1))

sal.dat.xtab=dcast(sal.dat,SITE+Date.EST~param,value.var="Data.Value",mean,na.rm=T)
fill=data.frame(expand.grid(SITE=unique(bk.dbkeys.bot$SITE),Date.EST=date.fun(seq(dates2[1],dates2[2],"1 days"))))
sal.dat.xtab=merge(sal.dat.xtab,fill,c("SITE","Date.EST"),all.y=T)
sal.dat.xtab$Sal=with(sal.dat.xtab,SalinityCalc(SPC,WT))
Cal.Sal=subset(sal.dat.xtab,SITE%in%c("CCORAL","FORTMYERSM","MARKH","SANIB2"))
unique(Cal.Sal$SITE)

Cal.Sal=merge(Cal.Sal,da.screen,c("Date.EST","SITE"))
Cal.Sal=Cal.Sal[order(Cal.Sal$SITE,Cal.Sal$Date.EST),]
Cal.Sal$Sal=with(Cal.Sal,ifelse(screen==1,Sal,NA))
Cal.Sal$MovingAvg.7d=with(Cal.Sal,ave(Sal,SITE,FUN=function(x) roll_meanr(x,n=7)))



## SLE ---------------------------------------------------------------------

SLE.dbkeys=data.frame(STATION=c("GORDY","S49","S48","S80","S308","S308.DS"),
                       DBKEY=c("91295","91607","91606","DJ238","DJ239","06548"))
q.da=data.frame()
for(i in 1:nrow(SLE.dbkeys)){
  tmp=DBHYDRO_daily(dates[1],dates[2],SLE.dbkeys$DBKEY[i])
  tmp$DBKEY=as.character(SLE.dbkeys$DBKEY[i])
  q.da=rbind(q.da,tmp)
}
q.da=merge(q.da,SLE.dbkeys,"DBKEY")
q.da$DATE=date.fun(q.da$Date)

SLE.q.dat=dcast(q.da,DATE~STATION,value.var="Data.Value",mean,na.rm=T)
tmp=map.q[,c("Date","S308","S80")]
colnames(tmp)=c("Date","S308.map","S80.map")
SLE.q.dat=merge(SLE.q.dat,tmp,by.x="DATE",by.y="Date",all.x=T)
SLE.q.dat$S80=with(SLE.q.dat,ifelse(is.na(S80),S80.map,S80))
SLE.q.dat$S308=with(SLE.q.dat,ifelse(is.na(S308),S308.map,S308))
SLE.q.dat$C44Basin=with(SLE.q.dat,ifelse(S80<S308,0,S80-S308))

SLE.q.dat$NorthFork=rowSums(SLE.q.dat[,c("GORDY","S49","S48")],na.rm=T)

SLE.q.dat$SLE.tot=rowSums(SLE.q.dat[,c("GORDY","S49","S48","S80")],na.rm=T)

SLE.q.dat$Q.14=with(SLE.q.dat,roll_meanr(SLE.tot,n=14))

SLE.q.dat$WY=WY(SLE.q.dat$DATE)
SLE.q.dat$cum.S80=with(SLE.q.dat,ave(cfs.to.acftd(S80),WY,FUN = function(x)cumsum(ifelse(is.na(x),0,x))))
SLE.q.dat$cum.S308=with(SLE.q.dat,ave(cfs.to.acftd(S308),WY,FUN = function(x)cumsum(ifelse(is.na(x),0,x))))


max.q=max(subset(SLE.q.dat,DATE%in%seq(End.Date-ddays(30),End.Date+ddays(3),"1 days"))$SLE.tot,na.rm=T)
ylim.max=round(max.q+max.q*0.25,-3)

ylim.val=c(0,ylim.max);by.y=ylim.max/4;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val=date.fun(c(End.Date-ddays(30),End.Date+ddays(3)));xmaj=seq(xlim.val[1],xlim.val[2],by="7 days");xmin=seq(xlim.val[1],xlim.val[2],by="1 days")

layout(matrix(1:2,2,1,byrow=F),widths=c(2,1),heights=c(1,0.5))
par(family="serif",cex.axis=1.2,mar=c(2,2,1,2),oma=c(0.5,3,1,2));
plot(SLE.tot~DATE,SLE.q.dat,type="n",ylim=ylim.val,xlim=xlim.val,xaxs="i",yaxs="i",xaxt="n",yaxt="n",xlab=NA,ylab=NA)
xx=c(xlim.val[1],xlim.val[1],xlim.val[2],xlim.val[2])
yy3=c(1700,ylim.max,ylim.max,1700);polygon(x=xx,y=yy3,col=adjustcolor("red",0.5))
yy4=c(1400,1700,1700,1400);polygon(x=xx,y=yy4,col=adjustcolor("yellow",0.5))
yy5=c(150,1400,1400,150);polygon(x=xx,y=yy5,col=adjustcolor("green",0.5))
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(SLE.q.dat,pt_line(DATE,SLE.tot,2,"black",1,21,"grey",cex=1.25,pt.lwd=0.1))
with(SLE.q.dat,lines(DATE,Q.14,lwd=3,col=adjustcolor("dodgerblue3",0.5)))
axis_fun(1,xmaj,xmin,format(xmaj,"%m/%d/%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj,cex.axis=1)
axis_fun(4,ymaj,ymin,format(round(cfs.to.acftd(ymaj)/1000,1)),cex.axis=1)
box(lwd=lwd.val)
mtext("Date\n(MM/DD/YYYY)",side=1,line=2.75,cex=1.25)
mtext("Discharge (cfs)",side=2,line=3.5,cex=1.25)
mtext("Discharge (kAc-Ft D\u207B\u00B9)",side=4,line=2.5,cex=1.25)
mtext(side=3,line=-1.25,adj=0,col="white"," Does not include coastal run-off",cex=0.95,font=2)


## salinity
sle.bk.dbkeys.bot=data.frame(depth="bottom",
                         SITE=c(rep("HR1",2)),
                         param=rep(c("WT","SPC"),1),
                         DBKEY=c("IX674","IX679")
)
SLE.sal.dat=data.frame()
for(i in 1:nrow(sle.bk.dbkeys.bot)){
  tmp=DBHYDRO_breakpoint(dates2[1],dates2[2],sle.bk.dbkeys.bot$DBKEY[i])
  if(nrow(tmp)==0) next
  tmp$DBKEY=sle.bk.dbkeys.bot$DBKEY[i]
  SLE.sal.dat=rbind(SLE.sal.dat,tmp)
  print(i)
}
SLE.sal.dat=merge(SLE.sal.dat,sle.bk.dbkeys.bot,"DBKEY")
SLE.sal.dat$Date.EST=date.fun(SLE.sal.dat$DATETIME)

da.screen=ddply(SLE.sal.dat,c("Date.EST","SITE"),summarise,N.val=N.obs(Data.Value[param=="SPC"]))
da.screen$screen=with(da.screen,ifelse(N.val<20,0,1))

SLE.sal.dat.xtab=dcast(SLE.sal.dat,SITE+Date.EST~param,value.var="Data.Value",mean,na.rm=T)
fill=data.frame(expand.grid(SITE=unique(sle.bk.dbkeys.bot$SITE),Date.EST=date.fun(seq(dates2[1],dates2[2],"1 days"))))
SLE.sal.dat.xtab=merge(SLE.sal.dat.xtab,fill,c("SITE","Date.EST"),all.y=T)
SLE.sal.dat.xtab$WT=with(SLE.sal.dat.xtab,ifelse(WT<0,NA,WT))

SLE.sal.dat.xtab$Sal=with(SLE.sal.dat.xtab,SalinityCalc(SPC,WT))


ColumnRename=function(rawData){
  Conv.df=data.frame(conv.vals=c(paste("p",c("00010","00095","00480","00060"),sep="")),conv.defs=c("Wtemp","SpCond","Sal","Flow"))
  Cnames=names(rawData)
  dataColumns <- c(grep("X_", Cnames), grep("X\\d{2}", Cnames))
  
  for(i in dataColumns){
    chunks <- strsplit(Cnames[i], "_")[[1]]
    chunks=unlist(strsplit(chunks,".",fixed=T))
    loc=toupper(chunks[chunks%in%c("TOP","Top","BOTTOM","Bottom")==T])
    param=paste0("p",chunks[paste0("p",chunks) %in% Conv.df$conv.vals])
    param.def=as.character(subset(Conv.df,conv.vals==param)$conv.defs)
    flag=if(length(chunks[(chunks=="cd")])==0){NA}else{chunks[(chunks=="cd")]}
    Cnames[i]=if(is.na(flag)==F){paste(loc,param.def,flag,sep="_")}else{paste(loc,param.def,sep="_")}
  }
  Cnames <- gsub("X_", "", Cnames)
  names(rawData) <- Cnames
  return(rawData)
}

sites=c("02277100","02277110")
params=c("00010","00095","00480");#temp, spc,sal
stl.USGS.dat=NA
for(i in 1:length(sites)){
  tmp=readNWISdv(sites[i],params,format(Start.Date,"%Y-%m-%d"),format(End.Date,"%Y-%m-%d"))
  tmp=ColumnRename(tmp)
  stl.USGS.dat=rbind(stl.USGS.dat,tmp)
}
stl.USGS.dat=merge(stl.USGS.dat,data.frame(site_no=c("02277100","02277110"),SITE=c("STL_RIVER","STL_STPT")),by="site_no")
stl.USGS.dat$Date=date.fun(stl.USGS.dat$Date)

vars=c("SITE","Date","BOTTOM_SpCond","BOTTOM_Wtemp","BOTTOM_Sal")
usgs.dat=stl.USGS.dat[,vars]
colnames(usgs.dat)=c("SITE", "Date.EST","SPC","WT","Sal")

SLE.Sal=rbind(usgs.dat,SLE.sal.dat.xtab)
SLE.Sal$Calc.Sal=with(SLE.Sal,SalinityCalc(SPC,WT))

SLE.Sal$MovingAvg.7d=with(SLE.Sal,ave(Sal,SITE,FUN=function(x) roll_meanr(x,n=7)))


# LWL ---------------------------------------------------------------------
# S271 and S155A



# Florida Bay -------------------------------------------------------------



#### 
