## NOAA CI Remote Sensing Product download for analysis
## Created by: Paul Julian (pjulian@evergladesfoundation.org)
## Created on: 2024-07-10

library(AnalystHelper)
library(rvest)
library(httr)

## sets data.path depending on if on local machine or not
# if(grep('TEFP',Sys.info()["nodename"])==1){
#   data.path="C:/Julian_LaCie/_GitHub/EvergladesConditions/report/RSdata"
# }else{
#   data.path="./RSdata"
# }

# wd = "C:/Julian_LaCie/_GitHub/EvergladesConditions/report"
# data.path="C:/Julian_LaCie/_GitHub/EvergladesConditions/report/RSdata"
data.path="./report/RSdata"
## NOAA FTP image inventory ------------------------------------------------
link.val="https://app.coastalscience.noaa.gov/habs_explorer/index.php?path=ajZiOVoxaHZNdE5nNytEb3RZdU5iYjNnK3AvTWRrYmNWbXU0K0YvMlA1UlBtTWZlRFV3R1RicVRYb2pxeVJBUA==&uri=VWtuM1UzbVNVN0RsZzJMeTJvNlNpM29OalF0WTFQQjVZVnpuS3o5bnh1Ym0vYWhtWEh4ck1hREVUamE4SDZ0M2tsd1M1OWg3UDJ0djIrNEkvbXliRUJ3WjkrKzdIcUYrN1JsZ1I5NFlsaHBZbUJWV0pHZ3NFZUVnQW56aTFIbEw=&type=bllEUXA3TmhSK21RVDlqbFYxMmEwdz09"

rslt.table=rvest::read_html(link.val)

link_val= rslt.table|>
  html_elements(xpath = ".//section[@class='onecol habonecol']")|>
  html_nodes('a')|>
  html_attr("href")|>
  as.data.frame()
colnames(link_val)="link"

link_txt=rslt.table|>
  html_elements(xpath = ".//section[@class='onecol habonecol']")|>
  html_text(trim=T)|>
  as.data.frame()# |>(\(x) x[2:nrow(x),])(); # assumes first value is "Name"
colnames(link_txt) = "name"

link_txt=subset(link_txt,name!="Name")# link_txt[2:nrow(link_txt),]# assumes first value is "Name"

str.val=strsplit(link_txt$name,"\\.")
link_txt$date=date.fun(paste(as.numeric(substr(sapply(str.val,"[",2),1,4)),
                             as.numeric(substr(sapply(str.val,"[",3),1,2)),
                             as.numeric(substr(sapply(str.val,"[",3),3,4)),sep="-"))
link_txt$product=sapply(str.val,"[",9)
link_txt$AOI=sapply(str.val,"[",10)

noaa.image.inventory=cbind(link_txt,link_val)|>
  subset(product=="CIcyano")
noaa.image.inventory$fname=with(noaa.image.inventory,paste0(product,"_",AOI,"_",format(date,"%Y%m%d"),".tif"))

# noaa.image.inventory2=cbind(link_txt,link_val)|>
#   subset(product=="truecolor")|>
#   mutate(fname=paste0(product,"_",AOI,"_",format(date,"%Y%m%d"),".tif"))

## local file inventory
local.image.inventory = data.frame(fname=list.files(data.path))
local.image.inventory = subset(local.image.inventory,fname!="truecolor")

str.val=strsplit(local.image.inventory$fname,"_|\\.")
local.image.inventory$date=date.fun(paste(substr(sapply(str.val,"[",3),1,4),
                                          substr(sapply(str.val,"[",3),5,6),
                                          substr(sapply(str.val,"[",3),7,8),sep="-"))


max(noaa.image.inventory$date)
max(local.image.inventory$date)

# local.image.inventory2 = data.frame(fname=list.files(paste0(data.path,"/truecolor")))
# str.val=strsplit(local.image.inventory2$fname,"_|\\.")
# local.image.inventory2$date=date.fun(paste(substr(sapply(str.val,"[",3),1,4),
#                                           substr(sapply(str.val,"[",3),5,6),
#                                           substr(sapply(str.val,"[",3),7,8),sep="-"))
# 
# 
# max(noaa.image.inventory2$date)
# max(local.image.inventory2$date)

## update for missing data
noaa.image.inventory=subset(noaa.image.inventory,date>max(local.image.inventory$date)); # CICyano
# noaa.image.inventory2=subset(noaa.image.inventory2,date>max(local.image.inventory2$date)); # TrueColor
## data download  ------------------------------------------------

if(nrow(noaa.image.inventory)!=0){
  for(i in 1:nrow(noaa.image.inventory)){
    
    url.link=noaa.image.inventory$link[i]
    
    hd=httr::HEAD(url.link)
    if(hd$all_headers[[1]]$status!=200){next}else{
      try(download.file(url.link,paste(data.path, noaa.image.inventory$fname[i],sep="/"),mode="wb",method="wininet"))
    }
    cat(paste0("\nData Downloaded/Update ",Sys.Date(),". ",nrow(noaa.image.inventory)," files downloaded."),file="./report/NOAADownloadLog.txt",append=T)
  }
}else{
  cat(paste0("\nData Downloaded/Update ",Sys.Date(),". ",nrow(noaa.image.inventory)," files downloaded."),file="./report/NOAADownloadLog.txt",append=T)
}


# if(nrow(noaa.image.inventory2)!=0){
#   for(i in 1:nrow(noaa.image.inventory2)){
#     
#     url.link=noaa.image.inventory2$link[i]
#     
#     hd=httr::HEAD(url.link)
#     if(hd$all_headers[[1]]$status!=200){next}else{
#       try(download.file(url.link,paste(data.path,"truecolor", noaa.image.inventory2$fname[i],sep="/"),mode="wb",method="wininet"))
#     }
#   }
# }

