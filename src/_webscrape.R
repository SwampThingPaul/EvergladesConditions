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
library(sp)
library(rgdal)
library(rgeos)

# library(tmap)
library(httr)
library(raster)
##
wgs84=CRS("+init=epsg:4326")
utm17=CRS("+init=epsg:26917")
# tmap_mode("view")
###
dates=seq(date.fun(date.fun(Sys.Date())-ddays(30)),date.fun(Sys.Date()),"1 days")

#### GIS data
shore=spTransform(readOGR("C:/Julian_LaCie/_GitHub/EvergladesConditions/report/GIS","FWC_Shoreline_simp"),wgs84)
# shore=readOGR("./GISData","FWC_Shoreline_simp") 
shore=spTransform(shore,wgs84)

lakeO=readOGR("C:/Julian_LaCie/_GitHub/EvergladesConditions/report/GIS","LakeOkeechobee_general")
# lakeO=readOGR("./GISData","LakeOkeechobee_general")
lakeO=spTransform(lakeO,utm17)

struct=readOGR("C:/Julian_LaCie/_GitHub/EvergladesConditions/report/GIS","SFWMDStructures")
# writeOGR(struct,"C:/Julian_LaCie/_GitHub/EvergladesConditions/report/GIS","SFWMDStructures",driver="ESRI Shapefile")



# Data from USACE Daily Map -----------------------------------------------
## Maps and Archived data
mapdata=readLines("https://w3.saj.usace.army.mil/h2o/reports/StatusDaily/archive/1217/StatusDaily.htm")
grep("S-155A",mapdata)
mapdata[262]
tmp=strsplit(mapdata[213],"<br>|>")[[1]]
sum(as.numeric(sapply(strsplit(tmp[grepl(" inflows",tmp)],":"),"[")[2,]))

map.q=data.frame()
map.stg=data.frame()
for(i in 1:(length(dates)-1)){
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
  
  # val=grep("/plots/s79h.pdf",mapdata)
  # S79=strsplit(strsplit(mapdata[val],"\\s+")[[1]][6],"</a>")[[1]][1]
  # 
  # val=grep("/plots/s78h.pdf",mapdata)
  # S78=strsplit(strsplit(mapdata[val],"\\s+")[[1]][6],"</a>")[[1]][1]
  
  val=grep("/plots/s77",mapdata)
  S77=strsplit(strsplit(mapdata[val],"\\s+")[[1]][6],"</a>")[[1]][1]
  
  val=grep("/plots/s308",mapdata)
  S308=strsplit(strsplit(mapdata[val],"\\s+")[[1]][6],"</a>")[[1]][1]
  
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
                  S308=as.numeric(S77),
                  WCA1=as.numeric(WCA1),
                  WCA2=as.numeric(WCA2),
                  WCA3=as.numeric(WCA3),
                  ENP=as.numeric(ENP),
                  S12s=as.numeric(S12s),
                  S333=as.numeric(S333),
                  S333N=as.numeric(S333N),
                  S356=as.numeric(S356),
                  S155A=as.numeric(S155A)
                  )
  map.q=rbind(map.q,rslt)
  rslt.stg=data.frame(Date=date.val,Stg=as.numeric(LakeStage))
  map.stg=rbind(map.stg,rslt.stg)
  print(i)
}

map.q$NthLake=rowSums(map.q[,c("FEC","Istok","S65E","S65EX1")],na.rm=T)
map.q$LOIN=rowSums(map.q[,c("OtherLOKInflow","FEC","Istok","S65E","S65EX1")],na.rm=T)
map.q$LOOUT=rowSums(map.q[,c("S77","S354","S351","S352","S271","S308")],na.rm=T)

map.q
map.q[,2:ncol(map.q)]=cfs.to.acftd(map.q[,2:ncol(map.q)])
map.q




# DBHydro -----------------------------------------------------------------
# https://w3.saj.usace.army.mil/h2o/lib/documents/Central_Flows_Ref.pdf
dates=date.fun(c("2023-02-20","2023-03-01"))
sites.STAs.WCA1=data.frame(
  SITE=c('S362',"S251","G310"),
  DBKEY=c(91517,90934,90973)
)

q.dat=DBHYDRO_daily(dates[1],dates[2],sites.STAs.WCA1$DBKEY)
q.dat=merge(q.dat,sites.STAs.WCA1,"DBKEY")

q.dat.xtab=dcast(q.dat,Date~SITE,value.var ="Data.Value",mean)
q.dat.xtab

sites.STAs.WCA2=data.frame(
  SITE=c('G335',"G436","S7"),
  DBKEY=c(91008,91209,91680)
)

q.dat=DBHYDRO_daily(dates[1],dates[2],sites.STAs.WCA2$DBKEY)
q.dat=merge(q.dat,sites.STAs.WCA2,"DBKEY")

q.dat.xtab=dcast(q.dat,Date~SITE,value.var ="Data.Value",mean)
q.dat.xtab


sites.STAs.WCA3a=data.frame(
  SITE=c('S8',"S150","G404","G409"),
  DBKEY=c(91688,91395,91190,91194)
)

q.dat=DBHYDRO_daily(dates[1],dates[2],sites.STAs.WCA3a$DBKEY)
q.dat=merge(q.dat,sites.STAs.WCA3a,"DBKEY")

q.dat.xtab=dcast(q.dat,Date~SITE,value.var ="Data.Value",mean)
q.dat.xtab$rG404=with(q.dat.xtab,ifelse(G404-G409<0,0,G404-G409))
q.dat.xtab$TFlow=rowSums(q.dat.xtab[,c("S8","S150","rG404")])



# LONIN -------------------------------------------------------------------
# load("C:/Julian_LaCie/_Github/LakeO_Sediment/Export/LOK_stgvol_ft_mod.Rdata")
# load("C:/Julian_LaCie/_Github/LakeO_Sediment/Export/LOK_stgarea_ft_mod.Rdata")

stg.vol.ft$model
stg.area.ft
stg1=predict(stg.vol.ft,data.frame(z=15.50),interval="confidence")
stg2=predict(stg.vol.ft,data.frame(z=15.46),interval="confidence")

library(AnalystHelper)

storage.cfs=stg2[1]*(1/cfs.to.acftd(1))-stg1[1]*(1/cfs.to.acftd(1))


map.stg$volume.cfs=predict(stg.vol.ft,data.frame(z=map.stg$Stg))# *(1/cfs.to.acftd(1))
map.stg$delta.store=c(NA,diff(map.stg$volume.cfs))
map.stg$area.acres=predict(stg.area.ft,data.frame(z=map.stg$Stg))

plot(volume.cfs~Date,map.stg)
plot(area.acres~Date,map.stg)
