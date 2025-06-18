## System Report Data Download
## Created by: Paul Julian (pjulian@evergladesfoundation.org)
## Created on: 2025-03-20

## Analysis Outline/notes

## BAD 
## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
## Clears Everything...start fresh.
# rm(list=ls(all=T));cat("\014");dev.off()


library(AnalystHelper)
library(plyr)
library(reshape2)
library(lubridate)

# Database 
library(DBI)
library(RSQLite)


# data download -----------------------------------------------------------

dates <- seq(date.fun(date.fun(Sys.Date())-ddays(13)),date.fun(Sys.Date()),"1 days")

CurWY <- WY(date.fun(Sys.time()))

Start.Date <- date.fun(paste(CurWY-4,05,01,sep="-"))
End.Date <- date.fun(Sys.time())

YEST <- date.fun(End.Date-ddays(1))


# USACE Data --------------------------------------------------------------
dates <- seq(date.fun(paste(format(Sys.Date(),"%Y"),"01","01",sep="-")),date.fun(Sys.Date()),"1 days")

## Maps and Archived data
# mapdata=readLines("https://w3.saj.usace.army.mil/h2o/reports/StatusDaily/archive/1217/StatusDaily.htm")


## Central and South Sytem ------------------------------------------------
results_list <- vector("list", length(dates))

extract_val <- function(mapdata, pattern, split1, idx1, split2 = NULL, idx2 = NULL, as_num = TRUE) {
  line <- grep(pattern, mapdata, value = TRUE)
  if (length(line) == 0) return(NA)
  part <- strsplit(line, split1)[[1]]
  if (length(part) < idx1) return(NA)
  value <- part[idx1]
  if (!is.null(split2)) {
    part2 <- strsplit(value, split2)[[1]]
    if (length(part2) < idx2) return(NA)
    value <- part2[idx2]
  }
  if (as_num) return(as.numeric(value))
  value
}

for (i in seq_along(dates)) {
  date_i <- dates[i]
  url <- paste0("https://w3.saj.usace.army.mil/h2o/reports/StatusDaily/archive/", format(date_i, "%m%d"), "/StatusDaily.htm")
  
  mapdata <- tryCatch(readLines(url, warn = FALSE), error = function(e) return(NULL))
  if (is.null(mapdata)) {
    results_list[[i]] <- NA
    next
  }
  
  rslt <- data.frame(
    Date = date_i - ddays(1),
    OtherLOKInflow = {
      line <- grep("Other inflows", mapdata, value = TRUE)
      tmp <- strsplit(line, "<br>|>")[[1]]
      sum(as.numeric(sapply(strsplit(tmp[grepl(" inflows", tmp)], ":"), function(x) x[2])), na.rm = TRUE)
    },
    FEC = extract_val(mapdata, "Fisheating Creek", "\\s+", 7, "</div>", 1),
    Istok = extract_val(mapdata, "Istokpoga</a>", "\\s+", 6, "</div>", 1),
    S65E = extract_val(mapdata, "S-65E</a>", "\\s+", 6, "<br>", 1),
    S65EX1 = extract_val(mapdata, "S-65EX1</a>", "\\s+", 6, "<br>", 1),
    S354 = extract_val(mapdata, "S354", "\\s+", 8, "</div>", 1),
    S351 = extract_val(mapdata, "S351", "\\s+", 8, "</div>", 1),
    S352 = extract_val(mapdata, "S352", "\\s+", 8, "</div>", 1),
    S271 = extract_val(mapdata, "S-271", "\\s+", 6, "</div>", 1),
    S77 = extract_val(mapdata, "/plots/s77", "\\s+", 6, "</a>", 1),
    S79 = extract_val(mapdata, "/plots/s79h.pdf", "\\s+", 6, "</a>", 1),
    S80 = extract_val(mapdata, "/plots/s80", "\\s+", 6, "</a>", 1),
    S308 = extract_val(mapdata, "/plots/s308", "\\s+", 6, "</a>", 1),
    WCA1 = extract_val(mapdata, "CA1IN", "\\s+", 13, "</div>", 1),
    WCA2 = extract_val(mapdata, "CA2IN", "\\s+", 13, "</div>", 1),
    WCA3 = extract_val(mapdata, "CA3IN", "\\s+", 13, "</div>", 1),
    S10s = extract_val(mapdata, "S10", "\\s+", 8, "</div>", 1),
    S11s = extract_val(mapdata, "S11", "\\s+", 8, "</div>", 1),
    S12s = extract_val(mapdata, "S12", "\\s+", 8, "</div>", 1),
    S333 = extract_val(mapdata, "S333", "<br>|\\s+", 12),
    S333N = extract_val(mapdata, "S333", "<br>|\\s+", 14, "</div>", 1),
    S356 = extract_val(mapdata, "S356", "\\s+", 8, "</div>", 1),
    S155A = extract_val(mapdata, "S155A", "\\s+", 8, "</", 1),
    LOK.stage = extract_val(mapdata, "../plots/ok8hhp.pdf", "\\s+", 6),
    ENP = NA_real_  # Placeholder for now
  )
  
  # ENP needs special handling because it's computed
  rslt$ENP <- with(rslt, S12s + (S333 + S333N) - S356)
  
  results_list[[i]] <- rslt
  print(i)
}

# Drop NULL/NA rows and bind into a single data frame
map.q <- do.call(rbind, results_list[!sapply(results_list, is.na)])


## South Dade conveyance page ----------------------------------------------
results_list <- vector("list", length(dates))

for (i in seq_along(dates)) {
  date_i <- dates[i]
  map.url <- paste0("https://w3.saj.usace.army.mil/h2o/reports/SDCSDaily/archive/",
                    format(date_i, "%m%d"), "/SDCSDaily.htm")
  
  mapdata <- tryCatch(readLines(map.url, warn = FALSE), error = function(e) return(NULL))
  
  if (!is.null(mapdata)) {
    get_val <- function(pattern, split_regex, idx) {
      line <- grep(pattern, mapdata, value = TRUE)
      if (length(line) == 0) return(NA_real_)
      parts <- strsplit(line, split_regex)[[1]]
      if (length(parts) < idx) return(NA_real_)
      as.numeric(parts[idx])
    }
    
    S355A <- get_val("S355A", ">S-355A:|</div>", 2)
    S355B <- get_val("S355B", ">S-355B:|</div>", 2)
    L29Canal <- get_val("L-29", "Canal:|ft", 3)
    S197 <- get_val("S197", ">S-197:|</div>", 2)
    
    results_list[[i]] <- data.frame(
      Date = date_i - ddays(1),
      S355A = S355A,
      S355B = S355B,
      L29Canal.map = L29Canal,
      S197 = S197
    )
  } else {
    results_list[[i]] <- data.frame(
      Date = date_i - ddays(1),
      S355A = NA_real_,
      S355B = NA_real_,
      L29Canal.map = NA_real_
    )
  }
  
  print(i)
}
# Combine all results efficiently
map.SDCS <- do.call(rbind, results_list)

