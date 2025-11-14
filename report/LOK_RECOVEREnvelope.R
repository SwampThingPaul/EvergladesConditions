
library(zoo)

## All data is in Ft, NGVD29. To convert to Ft, NAVD88 subtract by 1.25
## Lake Okeecobee "Normal" ecological Envelope Breakpoint data 
lok.norm.env <- rbind(
  data.frame(
    month = c(1, 1, 2, 3, 5, 6, 7, 8, 8, 9, 10, 11, 11, 12),
    day = c(1, 15, 15, 15, 1, 1, 1, 1, 15, 15, 15, 1, 15, 31),
    variable = "lower",
    value = c(14.5, 14.5, 14.0, 13.5, 12.0, 11.5, 11.5, 12.0, 12.0, 12.5,
              13.5, 14.0, 14.5, 14.5)
  ),
  data.frame(
    month = c(1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 9, 10, 12),
    day = c(1, 15, 15, 15, 1, 1, 15, 15, 15, 1, 15, 15, 31),
    variable = "upper",
    value = c(15.5, 15.5, 15.0, 14.5, 14.5, 14.0, 12.5, 13.0, 13.5, 14.0,
              15.0, 15.5, 15.5)
  )
)

## makes a "wide version" of the data (using base R)
lok.norm.env_wide <- reshape(
  lok.norm.env,
  timevar = "variable",
  idvar = c("month", "day"),
  direction = "wide"
)
names(lok.norm.env_wide) <- sub("value\\.", "", names(lok.norm.env_wide))

lok.norm.env_wide$lower_NAVD88 <- lok.norm.env_wide$lower - 1.25; # example of conversion of NGVD29 to NAVD88

## Lake Okeechobee Recovery Envelope Breakpoint data 
lok.rec.env <- rbind(
  data.frame(
    month = c(1, 1, 2, 3, 5, 7, 8, 8, 9, 10, 11, 11, 12),
    day = c(1, 15, 15, 15, 15, 1, 1, 15, 15, 15, 1, 15, 31),
    variable = "lower",
    value = c(13.5, 13.5, 13.0, 12.5, 11.0, 11.0, 11.5, 11.5, 12.0, 
              13.0, 13.5, 14.0, 14.0)
  ),
  data.frame(
    month = c(1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12)
    day = c(1, 15, 15, 15, 1, 1, 15, 15, 15, 15, 15, 31)
    variable = "upper",
    value = c(14.5, 14.5, 14, 13.5, 13.5, 13.0, 12.0, 12.5, 13.0, 
              14.5, 15.0, 15.0)
  )
)

## makes a "wide version" of the data (using base R)
lok.rec.env_wide <- reshape(
  lok.rec.env,
  timevar = "variable",
  idvar = c("month", "day"),
  direction = "wide"
)
names(lok.rec.env_wide) <- sub("value\\.", "", names(lok.rec.env_wide))


## Make a time-series for each line for plotting/analysis
date_seq <- data.frame(
  date = seq(as.Date("2025-01-01"),as.Date("2025-12-31"),"1 days")
)
date_seq$month <- as.numeric(format(date_seq$date,"%m"))
date_seq$day <- as.numeric(format(date_seq$date,"%d"))

lok.norm.env_2025 <- merge(date_seq,lok.norm.env_wide,c("month","day"),all.x=T)
lok.norm.env_2025$lower <- na.approx(lok.norm.env_2025$lower)# interpolates between breakpoints
lok.norm.env_2025$upper <- na.approx(lok.norm.env_2025$upper)

lok.rec.env_2025 <- merge(date_seq,lok.rec.env_wide,c("month","day"),all.x=T)
lok.rec.env_2025$lower <- na.approx(lok.rec.env_2025$lower)
lok.rec.env_2025$upper <- na.approx(lok.rec.env_2025$upper)


plot(lower~date,lok.norm.env_2025,ylim=c(10,19),type="l",col="blue",lwd=2,
     ylab = "Stage (Ft, NGVD29)", xlab = "Date (2025)")
lines(upper~date,lok.norm.env_2025,col="blue",lwd=2)

lines(lower~date,lok.rec.env_2025,col="red",lwd=2,lty=2)
lines(upper~date,lok.rec.env_2025,col="red",lwd=2,lty=2)
