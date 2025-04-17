



### Scenario 1

# ```{r S1Calc,echo = F}
LOK_HMF_S1 <- forFUN_scenario(
  CRE.Q = cfs.to.acftd(2100)*rep(Q.fac,num.weeks),
  SLE.Q = cfs.to.acftd(1400)*rep(Q.fac,num.weeks),
  EAA.Q = cfs.to.acftd(0)*rep(Q.fac,num.weeks), 
  inflow.Q = cfs.to.acftd(Q.dat$Inflow.median), 
  ET =  ET.dat$med.ETPI_ft,
  STG = LOK.stg.da_today$STG29)
LOK_HMF_S1.UCI <- forFUN_scenario(
  CRE.Q = cfs.to.acftd(2100)*rep(Q.fac,num.weeks),
  SLE.Q = cfs.to.acftd(1400)*rep(Q.fac,num.weeks),
  EAA.Q = cfs.to.acftd(0)*rep(Q.fac,num.weeks), 
  inflow.Q = cfs.to.acftd(Q.dat$Inflow.Q90), 
  ET =  ET.dat$Q90.ETPI_ft,
  STG = LOK.stg.da_today$STG29)
LOK_HMF_S1.LCI <- forFUN_scenario(
  CRE.Q = cfs.to.acftd(2100)*rep(Q.fac,num.weeks),
  SLE.Q = cfs.to.acftd(1400)*rep(Q.fac,num.weeks),
  EAA.Q = cfs.to.acftd(0)*rep(Q.fac,num.weeks), 
  inflow.Q = cfs.to.acftd(Q.dat$Inflow.Q10), 
  ET =  ET.dat$Q10.ETPI_ft,
  STG = LOK.stg.da_today$STG29)
# ```

# ```{r S1,echo=FALSE,results='hide',fig.width=7,fig.height=5,fig.align='center',fig.cap="Lake Okeechobee Stage forecast under scenario #1."}
j <- 1
par(family="serif",mar=c(2,3,0.25,0.5),oma=c(2,1,1,0.5));
layout(matrix(1:2,1,2),widths=c(1,0.5))

xlim.val=date.fun(c("2024-11-20","2025-04-30"));by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],"2 months");xmin=seq(xlim.val[1],xlim.val[2],"1 months")
ylim.val=c(10,17.5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

plot(STG29~Date.EST,LOK.stg.da,ylim=ylim.val,xlim=xlim.val,type="n",ann=F,axes=F)
abline(h=ymaj,v=xmaj,lwd=0.5,col="grey",lty=3)
lines(ZONE.A~Date,LOSOM.sch.bk,lwd=1.5,col="black")
lines(ZONE.BC~Date,LOSOM.sch.bk,lwd=1.5,col="black")
lines(ZONE.D~Date,LOSOM.sch.bk,lwd=1.5,col="grey")
lines(STG29~Date.EST,LOK.stg.da,lwd=1,col="red",lty=1)
# shaded.range(LOK_HMF_S1.UCI$Date_forecast,LOK_HMF_S1.LCI$Stage,LOK_HMF_S1.UCI$Stage,s.cols[j],lty=0)
lines(Stage~Date_forecast,LOK_HMF_S1,lwd=2,col=s.cols[j],lty=1)
# lines(Stage~Date_forecast,LOK_HMF_S1.UCI,col=s.cols[j],lty=2,lwd=1)
# lines(Stage~Date_forecast,LOK_HMF_S1.LCI,col=s.cols[j],lty=2,lwd=1)
abline(h=12,col="hotpink",lwd=2,lty=2)
abline(h=11.5,col="lightgreen",lwd=2,lty=2)
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj)
box(lwd=1)
mtext(side=1,"Month",line=2,cex=1.25)
mtext(side=2,"Stage Elevation\n(Feet, NGVD29)",line=2,cex=1)

par(mar=c(2,0.5,0.25,0.25))
plot(0:1,0:1,type="n",ann=F,axes=F)

legend("center",
       legend = c("Current Stage",leg.txt[j]),
       pch=c(NA),lty=c(1),lwd=2,
       col=c("red",s.cols[j]),#,"hotpink","lightgreen"),
       pt.bg=c(NA),pt.cex=1,
       ncol=1,cex=0.7,bty="n",y.intersp=1.5,x.intersp=0.75,xjust=0.5,yjust=0.5)
mtext(side=1,line=-2,adj=0,"- Estuary discharges based on\n 2-week avg pulse discharges\n- Assumes no rainfall\n- Historic median ET",cex=0.75)
# ```

# ```{r,echo=F}
LOK_HMF_S1|>
  download_this(
    output_name = "S1_forecast",
    output_extension = ".xlsx",
    button_label = "Scenario Forecast Output",
    button_type = "primary",
    has_icon = TRUE,
    icon = "fa fa-file-excel"
  )
# ```

### Scenario 2

# ```{r S2Calc,echo = F}
LOK_HMF_S2 <- forFUN_scenario(
  CRE.Q = cfs.to.acftd(2100)*rep(Q.fac,num.weeks),
  SLE.Q = cfs.to.acftd(1400)*rep(Q.fac,num.weeks),
  EAA.Q = cfs.to.acftd(Q.dat$Outflow.median), 
  inflow.Q = cfs.to.acftd(Q.dat$Inflow.median), 
  ET =  ET.dat$med.ETPI_ft,
  STG = LOK.stg.da_today$STG29)
LOK_HMF_S2.UCI <- forFUN_scenario(
  CRE.Q = cfs.to.acftd(2100)*rep(Q.fac,num.weeks),
  SLE.Q = cfs.to.acftd(1400)*rep(Q.fac,num.weeks),
  EAA.Q = cfs.to.acftd(Q.dat$Outflow.Q90), 
  inflow.Q = cfs.to.acftd(Q.dat$Inflow.Q90), 
  ET =  ET.dat$Q90.ETPI_ft,
  STG = LOK.stg.da_today$STG29)
LOK_HMF_S2.LCI <- forFUN_scenario(
  CRE.Q = cfs.to.acftd(2100)*rep(Q.fac,num.weeks),
  SLE.Q = cfs.to.acftd(1400)*rep(Q.fac,num.weeks),
  EAA.Q = cfs.to.acftd(Q.dat$Outflow.Q10), 
  inflow.Q = cfs.to.acftd(Q.dat$Inflow.Q10), 
  ET =  ET.dat$Q10.ETPI_ft,
  STG = LOK.stg.da_today$STG29)
# ```

# ```{r S2,echo=FALSE,results='hide',fig.width=7,fig.height=5,fig.align='center',fig.cap="Lake Okeechobee Stage forecast under scenario #2."}
j <- 2
par(family="serif",mar=c(2,3,0.25,0.5),oma=c(2,1,1,0.5));
layout(matrix(1:2,1,2),widths=c(1,0.5))

xlim.val=date.fun(c("2024-11-20","2025-04-30"));by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],"2 months");xmin=seq(xlim.val[1],xlim.val[2],"1 months")
ylim.val=c(10,17.5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

plot(STG29~Date.EST,LOK.stg.da,ylim=ylim.val,xlim=xlim.val,type="n",ann=F,axes=F)
abline(h=ymaj,v=xmaj,lwd=0.5,col="grey",lty=3)
lines(ZONE.A~Date,LOSOM.sch.bk,lwd=1.5,col="black")
lines(ZONE.BC~Date,LOSOM.sch.bk,lwd=1.5,col="black")
lines(ZONE.D~Date,LOSOM.sch.bk,lwd=1.5,col="grey")
lines(STG29~Date.EST,LOK.stg.da,lwd=1,col="red",lty=1)
# shaded.range(LOK_HMF_S2.UCI$Date_forecast,LOK_HMF_S2.LCI$Stage, LOK_HMF_S2.UCI$Stage,s.cols[j],lty=0)
lines(Stage~Date_forecast,LOK_HMF_S2,lwd=2,col=s.cols[j],lty=1)
# lines(Stage~Date_forecast,LOK_HMF_S2.UCI,col=s.cols[j],lty=2,lwd=1)
# lines(Stage~Date_forecast,LOK_HMF_S2.LCI,col=s.cols[j],lty=2,lwd=1)
abline(h=12,col="hotpink",lwd=2,lty=2)
abline(h=11.5,col="lightgreen",lwd=2,lty=2)
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj)
box(lwd=1)
mtext(side=1,"Month",line=2,cex=1.25)
mtext(side=2,"Stage Elevation\n(Feet, NGVD29)",line=2,cex=1)

par(mar=c(2,0.5,0.25,0.25))
plot(0:1,0:1,type="n",ann=F,axes=F)

legend("center",
       legend = c("Current Stage",leg.txt[j]),
       pch=c(NA),lty=c(1),lwd=2,
       col=c("red",s.cols[j]),#,"hotpink","lightgreen"),
       pt.bg=c(NA),pt.cex=1,
       ncol=1,cex=0.7,bty="n",y.intersp=1.5,x.intersp=0.75,xjust=0.5,yjust=0.5)
mtext(side=1,line=-2,adj=0,"- Estuary discharges based on\n 2-week avg pulse discharges\n- Assumes no rainfall\n- Historic median ET",cex=0.75)
# ```

# ```{r,echo=F}
LOK_HMF_S2|>
  download_this(
    output_name = "S2_forecast",
    output_extension = ".xlsx",
    button_label = "Scenario Forecast Output",
    button_type = "primary",
    has_icon = TRUE,
    icon = "fa fa-file-excel"
  )
# ```

### Scenario 3

# ```{r S3Calc,echo = F}
EAA_add <- data.frame(Date.EST = seq(TODAY+(1*86400),EDate,"1 days"))|>
  mutate(AddQ = ifelse(Date.EST>date.fun("2025-02-01"),300,0))

LOK_HMF_S3 <- forFUN_scenario(
  CRE.Q = cfs.to.acftd(2100)*rep(Q.fac,num.weeks),
  SLE.Q = cfs.to.acftd(1400)*rep(Q.fac,num.weeks),
  EAA.Q = cfs.to.acftd(Q.dat$Outflow.median+EAA_add$AddQ), 
  inflow.Q = cfs.to.acftd(Q.dat$Inflow.median), 
  ET =  ET.dat$med.ETPI_ft,
  STG = LOK.stg.da_today$STG29)
LOK_HMF_S3.UCI <- forFUN_scenario(
  CRE.Q = cfs.to.acftd(2100)*rep(Q.fac,num.weeks),
  SLE.Q = cfs.to.acftd(1400)*rep(Q.fac,num.weeks),
  EAA.Q = cfs.to.acftd(Q.dat$Outflow.Q90+EAA_add$AddQ), 
  inflow.Q = cfs.to.acftd(Q.dat$Inflow.Q90), 
  ET =  ET.dat$Q90.ETPI_ft,
  STG = LOK.stg.da_today$STG29)
LOK_HMF_S3.LCI <- forFUN_scenario(
  CRE.Q = cfs.to.acftd(2100)*rep(Q.fac,num.weeks),
  SLE.Q = cfs.to.acftd(1400)*rep(Q.fac,num.weeks),
  EAA.Q = cfs.to.acftd(Q.dat$Outflow.Q10+EAA_add$AddQ), 
  inflow.Q = cfs.to.acftd(Q.dat$Inflow.Q10), 
  ET =  ET.dat$Q10.ETPI_ft,
  STG = LOK.stg.da_today$STG29)
# ```

# ```{r S3,echo=FALSE,results='hide',fig.width=7,fig.height=5,fig.align='center',fig.cap="Lake Okeechobee Stage forecast under scenario #3."}
j <- 3
par(family="serif",mar=c(2,3,0.25,0.5),oma=c(2,1,1,0.5));
layout(matrix(1:2,1,2),widths=c(1,0.5))

xlim.val=date.fun(c("2024-11-20","2025-04-30"));by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],"2 months");xmin=seq(xlim.val[1],xlim.val[2],"1 months")
ylim.val=c(10,17.5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

plot(STG29~Date.EST,LOK.stg.da,ylim=ylim.val,xlim=xlim.val,type="n",ann=F,axes=F)
abline(h=ymaj,v=xmaj,lwd=0.5,col="grey",lty=3)
lines(ZONE.A~Date,LOSOM.sch.bk,lwd=1.5,col="black")
lines(ZONE.BC~Date,LOSOM.sch.bk,lwd=1.5,col="black")
lines(ZONE.D~Date,LOSOM.sch.bk,lwd=1.5,col="grey")
lines(STG29~Date.EST,LOK.stg.da,lwd=1,col="red",lty=1)
# shaded.range(LOK_HMF_S3.UCI$Date_forecast,LOK_HMF_S3.LCI$Stage, LOK_HMF_S3.UCI$Stage,s.cols[j],lty=0)
lines(Stage~Date_forecast,LOK_HMF_S3,lwd=2,col=s.cols[j],lty=1)
# lines(Stage~Date_forecast,LOK_HMF_S3.UCI,col=s.cols[j],lty=2,lwd=1)
# lines(Stage~Date_forecast,LOK_HMF_S3.LCI,col=s.cols[j],lty=2,lwd=1)
abline(h=12,col="hotpink",lwd=2,lty=2)
abline(h=11.5,col="lightgreen",lwd=2,lty=2)
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj)
box(lwd=1)
mtext(side=1,"Month",line=2,cex=1.25)
mtext(side=2,"Stage Elevation\n(Feet, NGVD29)",line=2,cex=1)

par(mar=c(2,0.5,0.25,0.25))
plot(0:1,0:1,type="n",ann=F,axes=F)

legend("center",
       legend = c("Current Stage",leg.txt[j]),
       pch=c(NA),lty=c(1),lwd=2,
       col=c("red",s.cols[j]),#,"hotpink","lightgreen"),
       pt.bg=c(NA),pt.cex=1,
       ncol=1,cex=0.7,bty="n",y.intersp=1.5,x.intersp=0.75,xjust=0.5,yjust=0.5)
mtext(side=1,line=-2,adj=0,"- Estuary discharges based on\n 2-week avg pulse discharges\n- Assumes no rainfall\n- Historic median ET",cex=0.75)
# ```

# ```{r,echo=F}
LOK_HMF_S3|>
  download_this(
    output_name = "S3_forecast",
    output_extension = ".xlsx",
    button_label = "Scenario Forecast Output",
    button_type = "primary",
    has_icon = TRUE,
    icon = "fa fa-file-excel"
  )
# ```

### Scenario 4

# ```{r S4Calc,echo = F}
LOK_HMF_S4 <- forFUN_scenario(
  CRE.Q = cfs.to.acftd(2100)*rep(Q.fac,num.weeks),
  SLE.Q = cfs.to.acftd(700)*rep(Q.fac,num.weeks),
  EAA.Q = cfs.to.acftd(Q.dat$Outflow.median+EAA_add$AddQ), 
  inflow.Q = cfs.to.acftd(Q.dat$Inflow.median), 
  ET =  ET.dat$med.ETPI_ft,
  STG = LOK.stg.da_today$STG29)
LOK_HMF_S4.UCI <- forFUN_scenario(
  CRE.Q = cfs.to.acftd(2100)*rep(Q.fac,num.weeks),
  SLE.Q = cfs.to.acftd(700)*rep(Q.fac,num.weeks),
  EAA.Q = cfs.to.acftd(Q.dat$Outflow.Q90+EAA_add$AddQ), 
  inflow.Q = cfs.to.acftd(Q.dat$Inflow.Q90), 
  ET =  ET.dat$Q90.ETPI_ft,
  STG = LOK.stg.da_today$STG29)
LOK_HMF_S4.LCI <- forFUN_scenario(
  CRE.Q = cfs.to.acftd(2100)*rep(Q.fac,num.weeks),
  SLE.Q = cfs.to.acftd(700)*rep(Q.fac,num.weeks),
  EAA.Q = cfs.to.acftd(Q.dat$Outflow.Q10+EAA_add$AddQ), 
  inflow.Q = cfs.to.acftd(Q.dat$Inflow.Q10), 
  ET =  ET.dat$Q10.ETPI_ft,
  STG = LOK.stg.da_today$STG29)
# ```

# ```{r S4,echo=FALSE,results='hide',fig.width=7,fig.height=5,fig.align='center',fig.cap="Lake Okeechobee Stage forecast under scenario #4."}
j <- 4
par(family="serif",mar=c(2,3,0.25,0.5),oma=c(2,1,1,0.5));
layout(matrix(1:2,1,2),widths=c(1,0.5))

xlim.val=date.fun(c("2024-11-20","2025-04-30"));by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],"2 months");xmin=seq(xlim.val[1],xlim.val[2],"1 months")
ylim.val=c(10,17.5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

plot(STG29~Date.EST,LOK.stg.da,ylim=ylim.val,xlim=xlim.val,type="n",ann=F,axes=F)
abline(h=ymaj,v=xmaj,lwd=0.5,col="grey",lty=3)
lines(ZONE.A~Date,LOSOM.sch.bk,lwd=1.5,col="black")
lines(ZONE.BC~Date,LOSOM.sch.bk,lwd=1.5,col="black")
lines(ZONE.D~Date,LOSOM.sch.bk,lwd=1.5,col="grey")
lines(STG29~Date.EST,LOK.stg.da,lwd=1,col="red",lty=1)
# shaded.range(LOK_HMF_S4.UCI$Date_forecast,LOK_HMF_S4.LCI$Stage, LOK_HMF_S4.UCI$Stage,s.cols[j],lty=0)
lines(Stage~Date_forecast,LOK_HMF_S4,lwd=2,col=s.cols[j],lty=1)
# lines(Stage~Date_forecast,LOK_HMF_S4.UCI,col=s.cols[j],lty=2,lwd=1)
# lines(Stage~Date_forecast,LOK_HMF_S4.LCI,col=s.cols[j],lty=2,lwd=1)
abline(h=12,col="hotpink",lwd=2,lty=2)
abline(h=11.5,col="lightgreen",lwd=2,lty=2)
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj)
box(lwd=1)
mtext(side=1,"Month",line=2,cex=1.25)
mtext(side=2,"Stage Elevation\n(Feet, NGVD29)",line=2,cex=1)

par(mar=c(2,0.5,0.25,0.25))
plot(0:1,0:1,type="n",ann=F,axes=F)

legend("center",
       legend = c("Current Stage",leg.txt[j]),
       pch=c(NA),lty=c(1),lwd=2,
       col=c("red",s.cols[j]),#,"hotpink","lightgreen"),
       pt.bg=c(NA),pt.cex=1,
       ncol=1,cex=0.7,bty="n",y.intersp=1.5,x.intersp=0.75,xjust=0.5,yjust=0.5)
mtext(side=1,line=-2,adj=0,"- Estuary discharges based on\n 2-week avg pulse discharges\n- Assumes no rainfall\n- Historic median ET",cex=0.75)
# ```

# ```{r,echo=F}
LOK_HMF_S4|>
  download_this(
    output_name = "S4_forecast",
    output_extension = ".xlsx",
    button_label = "Scenario Forecast Output",
    button_type = "primary",
    has_icon = TRUE,
    icon = "fa fa-file-excel"
  )
# ```

### Scenario 5

# ```{r S5Calc,echo = F}
LOK_HMF_S5 <- forFUN_scenario(
  CRE.Q = cfs.to.acftd(2100)*rep(Q.fac,num.weeks),
  SLE.Q = cfs.to.acftd(0)*rep(Q.fac,num.weeks),
  EAA.Q = cfs.to.acftd(Q.dat$Outflow.median+EAA_add$AddQ), 
  inflow.Q = cfs.to.acftd(Q.dat$Inflow.median), 
  ET =  ET.dat$med.ETPI_ft,
  STG = LOK.stg.da_today$STG29)
LOK_HMF_S5.UCI <- forFUN_scenario(
  CRE.Q = cfs.to.acftd(2100)*rep(Q.fac,num.weeks),
  SLE.Q = cfs.to.acftd(0)*rep(Q.fac,num.weeks),
  EAA.Q = cfs.to.acftd(Q.dat$Outflow.Q90+EAA_add$AddQ), 
  inflow.Q = cfs.to.acftd(Q.dat$Inflow.Q90), 
  ET =  ET.dat$Q90.ETPI_ft,
  STG = LOK.stg.da_today$STG29)
LOK_HMF_S5.LCI <- forFUN_scenario(
  CRE.Q = cfs.to.acftd(2100)*rep(Q.fac,num.weeks),
  SLE.Q = cfs.to.acftd(0)*rep(Q.fac,num.weeks),
  EAA.Q = cfs.to.acftd(Q.dat$Outflow.Q10+EAA_add$AddQ), 
  inflow.Q = cfs.to.acftd(Q.dat$Inflow.Q10), 
  ET =  ET.dat$Q10.ETPI_ft,
  STG = LOK.stg.da_today$STG29)
# ```

# ```{r S5,echo=FALSE,results='hide',fig.width=7,fig.height=5,fig.align='center',fig.cap="Lake Okeechobee Stage forecast under scenario #5."}
j <- 5
par(family="serif",mar=c(2,3,0.25,0.5),oma=c(2,1,1,0.5));
layout(matrix(1:2,1,2),widths=c(1,0.5))

xlim.val=date.fun(c("2024-11-20","2025-04-30"));by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],"2 months");xmin=seq(xlim.val[1],xlim.val[2],"1 months")
ylim.val=c(10,17.5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

plot(STG29~Date.EST,LOK.stg.da,ylim=ylim.val,xlim=xlim.val,type="n",ann=F,axes=F)
abline(h=ymaj,v=xmaj,lwd=0.5,col="grey",lty=3)
lines(ZONE.A~Date,LOSOM.sch.bk,lwd=1.5,col="black")
lines(ZONE.BC~Date,LOSOM.sch.bk,lwd=1.5,col="black")
lines(ZONE.D~Date,LOSOM.sch.bk,lwd=1.5,col="grey")
lines(STG29~Date.EST,LOK.stg.da,lwd=1,col="red",lty=1)
# shaded.range(LOK_HMF_S5.UCI$Date_forecast,LOK_HMF_S5.LCI$Stage, LOK_HMF_S5.UCI$Stage,s.cols[j],lty=0)
lines(Stage~Date_forecast,LOK_HMF_S5,lwd=2,col=s.cols[j],lty=1)
# lines(Stage~Date_forecast,LOK_HMF_S5.UCI,col=s.cols[j],lty=2,lwd=1)
# lines(Stage~Date_forecast,LOK_HMF_S5.LCI,col=s.cols[j],lty=2,lwd=1)
abline(h=12,col="hotpink",lwd=2,lty=2)
abline(h=11.5,col="lightgreen",lwd=2,lty=2)
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj)
box(lwd=1)
mtext(side=1,"Month",line=2,cex=1.25)
mtext(side=2,"Stage Elevation\n(Feet, NGVD29)",line=2,cex=1)

par(mar=c(2,0.5,0.25,0.25))
plot(0:1,0:1,type="n",ann=F,axes=F)

legend("center",
       legend = c("Current Stage",leg.txt[j]),
       pch=c(NA),lty=c(1),lwd=2,
       col=c("red",s.cols[j]),#,"hotpink","lightgreen"),
       pt.bg=c(NA),pt.cex=1,
       ncol=1,cex=0.7,bty="n",y.intersp=1.5,x.intersp=0.75,xjust=0.5,yjust=0.5)
mtext(side=1,line=-2,adj=0,"- Estuary discharges based on\n 2-week avg pulse discharges\n- Assumes no rainfall\n- Historic median ET",cex=0.75)
# ```

# ```{r,echo=F}
LOK_HMF_S5|>
  download_this(
    output_name = "S5_forecast",
    output_extension = ".xlsx",
    button_label = "Scenario Forecast Output",
    button_type = "primary",
    has_icon = TRUE,
    icon = "fa fa-file-excel"
  )
# ```


### Scenario 6

# ```{r S6Calc,echo = F}
LOK_HMF_S6 <- forFUN_scenario(
  CRE.Q = cfs.to.acftd(2100)*rep(Q.fac,num.weeks),
  SLE.Q = cfs.to.acftd(150)*rep(Q.fac,num.weeks),
  EAA.Q = cfs.to.acftd(Q.dat$Outflow.median+EAA_add$AddQ), 
  inflow.Q = cfs.to.acftd(Q.dat$Inflow.median), 
  ET =  ET.dat$med.ETPI_ft,
  STG = LOK.stg.da_today$STG29)
LOK_HMF_S6.UCI <- forFUN_scenario(
  CRE.Q = cfs.to.acftd(2100)*rep(Q.fac,num.weeks),
  SLE.Q = cfs.to.acftd(150)*rep(Q.fac,num.weeks),
  EAA.Q = cfs.to.acftd(Q.dat$Outflow.Q90+EAA_add$AddQ), 
  inflow.Q = cfs.to.acftd(Q.dat$Inflow.Q90), 
  ET =  ET.dat$Q90.ETPI_ft,
  STG = LOK.stg.da_today$STG29)
LOK_HMF_S6.LCI <- forFUN_scenario(
  CRE.Q = cfs.to.acftd(2100)*rep(Q.fac,num.weeks),
  SLE.Q = cfs.to.acftd(150)*rep(Q.fac,num.weeks),
  EAA.Q = cfs.to.acftd(Q.dat$Outflow.Q10+EAA_add$AddQ), 
  inflow.Q = cfs.to.acftd(Q.dat$Inflow.Q10), 
  ET =  ET.dat$Q10.ETPI_ft,
  STG = LOK.stg.da_today$STG29)
# # ```

# # ```{r S6,echo=FALSE,results='hide',fig.width=7,fig.height=5,fig.align='center',fig.cap="Lake Okeechobee Stage forecast under scenario #4."}
j <- 6
par(family="serif",mar=c(2,3,0.25,0.5),oma=c(2,1,1,0.5));
layout(matrix(1:2,1,2),widths=c(1,0.5))

xlim.val=date.fun(c("2024-11-20","2025-04-30"));by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],"2 months");xmin=seq(xlim.val[1],xlim.val[2],"1 months")
ylim.val=c(10,17.5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

plot(STG29~Date.EST,LOK.stg.da,ylim=ylim.val,xlim=xlim.val,type="n",ann=F,axes=F)
abline(h=ymaj,v=xmaj,lwd=0.5,col="grey",lty=3)
lines(ZONE.A~Date,LOSOM.sch.bk,lwd=1.5,col="black")
lines(ZONE.BC~Date,LOSOM.sch.bk,lwd=1.5,col="black")
lines(ZONE.D~Date,LOSOM.sch.bk,lwd=1.5,col="grey")
lines(STG29~Date.EST,LOK.stg.da,lwd=1,col="red",lty=1)
# shaded.range(LOK_HMF_S6.UCI$Date_forecast,LOK_HMF_S6.LCI$Stage, LOK_HMF_S6.UCI$Stage,s.cols[j],lty=0)
lines(Stage~Date_forecast,LOK_HMF_S6,lwd=2,col=s.cols[j],lty=1)
# lines(Stage~Date_forecast,LOK_HMF_S6.UCI,col=s.cols[j],lty=2,lwd=1)
# lines(Stage~Date_forecast,LOK_HMF_S6.LCI,col=s.cols[j],lty=2,lwd=1)
abline(h=12,col="hotpink",lwd=2,lty=2)
abline(h=11.5,col="lightgreen",lwd=2,lty=2)
axis_fun(1,xmaj,xmin,format(xmaj,"%m-%Y"),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj)
box(lwd=1)
mtext(side=1,"Month",line=2,cex=1.25)
mtext(side=2,"Stage Elevation\n(Feet, NGVD29)",line=2,cex=1)

par(mar=c(2,0.5,0.25,0.25))
plot(0:1,0:1,type="n",ann=F,axes=F)

legend("center",
       legend = c("Current Stage",leg.txt[j]),
       pch=c(NA),lty=c(1),lwd=2,
       col=c("red",s.cols[j]),#,"hotpink","lightgreen"),
       pt.bg=c(NA),pt.cex=1,
       ncol=1,cex=0.7,bty="n",y.intersp=1.5,x.intersp=0.75,xjust=0.5,yjust=0.5)
mtext(side=1,line=-2,adj=0,"- Estuary discharges based on\n 2-week avg pulse discharges\n- Assumes no rainfall\n- Historic median ET",cex=0.75)
# ```

# ```{r,echo=F}
LOK_HMF_S6|>
  download_this(
    output_name = "S6_forecast",
    output_extension = ".xlsx",
    button_label = "Scenario Forecast Output",
    button_type = "primary",
    has_icon = TRUE,
    icon = "fa fa-file-excel"
  )
# ```
