
library(AnalystHelper)
library(lubridate)


data.path <- "./report"

# Data from USACE Daily Map -----------------------------------------------
dates <- seq(date.fun(Sys.Date()-ddays(20)),date.fun(Sys.Date()),"1 days")
# dates=seq(date.fun(paste(format(Sys.Date(),"%Y"),"01","01",sep="-")),date.fun(Sys.Date()),"1 days")

## Maps and Archived data
# mapdata=readLines("https://w3.saj.usace.army.mil/h2o/reports/StatusDaily/archive/1217/StatusDaily.htm")

## Centeral and South Ssytem
results_list <- vector("list", length(dates))

extract_val <- function(mapdata, pattern, split1, idx1, split2 = NULL, idx2 = NULL, as_num = TRUE) {
  line <- grep(pattern, mapdata, value = TRUE)
  if (length(line) == 0) return(NA_real_)
  part1 <- strsplit(line, split1)[[1]]
  if (length(part1) < idx1) return(NA_real_)
  val <- part1[idx1]
  if (!is.null(split2)) {
    part2 <- strsplit(val, split2)[[1]]
    if (length(part2) < idx2) return(NA_real_)
    val <- part2[idx2]
  }
  if (as_num) return(as.numeric(val))
  return(val)
}

get_status_daily_data <- function(date_i) {
  url <- paste0("https://w3.saj.usace.army.mil/h2o/reports/StatusDaily/archive/",
                format(date_i, "%m%d"), "/StatusDaily.htm")
  
  mapdata <- tryCatch(readLines(url, warn = FALSE), error = function(e) return(NULL))
  if (is.null(mapdata)) return(NULL)
  
  other_inflow <- {
    line <- grep("Other inflows", mapdata, value = TRUE)
    tmp <- strsplit(line, "<br>|>")[[1]]
    sum(as.numeric(sapply(strsplit(tmp[grepl(" inflows", tmp)], ":"), function(x) x[2])), na.rm = TRUE)
  }
  
  rslt <- data.frame(
    Date = date_i - 1,
    OtherLOKInflow = other_inflow,
    FEC   = extract_val(mapdata, "Fisheating Creek", "\\s+", 7, "</div>", 1),
    Istok = extract_val(mapdata, "Istokpoga</a>", "\\s+", 6, "</div>", 1),
    S65E  = extract_val(mapdata, "S-65E</a>", "\\s+", 6, "<br>", 1),
    S65EX1= extract_val(mapdata, "S-65EX1</a>", "\\s+", 6, "<br>", 1),
    S354  = extract_val(mapdata, "S354", "\\s+", 8, "</div>", 1),
    S351  = extract_val(mapdata, "S351", "\\s+", 8, "</div>", 1),
    S352  = extract_val(mapdata, "S352", "\\s+", 8, "</div>", 1),
    S271  = extract_val(mapdata, "S-271", "\\s+", 6, "</div>", 1),
    S77   = extract_val(mapdata, "/plots/s77", "\\s+", 6, "</a>", 1),
    S79   = extract_val(mapdata, "/plots/s79h.pdf", "\\s+", 6, "</a>", 1),
    S80   = extract_val(mapdata, "/plots/s80", "\\s+", 6, "</a>", 1),
    S308  = extract_val(mapdata, "/plots/s308", "\\s+", 6, "</a>", 1),
    WCA1  = extract_val(mapdata, "CA1IN", "\\s+", 13, "</div>", 1),
    WCA2  = extract_val(mapdata, "CA2IN", "\\s+", 13, "</div>", 1),
    WCA3  = extract_val(mapdata, "CA3IN", "\\s+", 13, "</div>", 1),
    S10s  = extract_val(mapdata, "S10", "\\s+", 8, "</div>", 1),
    S11s  = extract_val(mapdata, "S11", "\\s+", 8, "</div>", 1),
    S12s  = extract_val(mapdata, "S12", "\\s+", 8, "</div>", 1),
    S333  = extract_val(mapdata, "S333", "<br>|\\s+", 12),
    S333N = extract_val(mapdata, "S333", "<br>|\\s+", 14, "</div>", 1),
    S356  = extract_val(mapdata, "S356", "\\s+", 8, "</div>", 1),
    S155A = extract_val(mapdata, "S155A", "\\s+", 8, "</", 1),
    LOK.stage = extract_val(mapdata, "../plots/ok8hhp.pdf", "\\s+", 6),
    stringsAsFactors = FALSE
  )
  
  rslt$ENP <- with(rslt, S12s + (S333 + S333N) - S356)
  return(rslt)
}

# Apply over all dates
results_list <- lapply(dates, get_status_daily_data)

# Filter out NULLs (failed requests) and bind into one data.frame
results_list <- results_list[!sapply(results_list, is.null)]
map.q <- do.call(rbind, results_list)

## South Dade conveyance page
get_sdcs_val <- function(mapdata, pattern, split_regex, idx) {
  line <- grep(pattern, mapdata, value = TRUE)
  if (length(line) == 0) return(NA_real_)
  parts <- strsplit(line, split_regex)[[1]]
  if (length(parts) < idx) return(NA_real_)
  return(as.numeric(parts[idx]))
}

# Define function to process a single date
get_sdcs_data <- function(date_i) {
  url <- paste0("https://w3.saj.usace.army.mil/h2o/reports/SDCSDaily/archive/",
                format(date_i, "%m%d"), "/SDCSDaily.htm")
  
  mapdata <- tryCatch(readLines(url, warn = FALSE), error = function(e) return(NULL))
  if (is.null(mapdata)) return(NULL)
  
  S355A <- get_sdcs_val(mapdata, "S355A", ">S-355A:|</div>", 2)
  S355B <- get_sdcs_val(mapdata, "S355B", ">S-355B:|</div>", 2)
  L29Canal <- get_sdcs_val(mapdata, "L-29", "Canal:|ft", 3)
  
  return(data.frame(
    Date = date_i - 1,
    S355A = S355A,
    S355B = S355B,
    L29Canal.map = L29Canal,
    stringsAsFactors = FALSE
  ))
}

# Apply function to all dates
sdcs_results_list <- lapply(dates, get_sdcs_data)

# Filter out NULLs
sdcs_results_list <- sdcs_results_list[!sapply(sdcs_results_list, is.null)]

# Bind rows into single data frame
map.SDCS <- do.call(rbind, sdcs_results_list)

# Replace NA values with 0
map.SDCS[, !(names(map.SDCS) %in% "Date")] <- lapply(
  map.SDCS[, !(names(map.SDCS) %in% "Date")],
  function(x) ifelse(is.na(x), 0, x)
)

# map.q[,!(names(map.q) %in% "Date")] <- lapply(map.q[,!(names(map.q) %in% "Date")], FUN=function(x) ifelse(is.na(x)==T,0,x))
map.q[, !(names(map.q) %in% "Date")] <- lapply(
  map.q[, !(names(map.q) %in% "Date")],
  function(x) ifelse(is.na(x), 0, x)
)


map.q <- cbind(map.q,map.SDCS[,!(names(map.SDCS) %in% "Date")])
map.q$Date <- date.fun(map.q$Date)

write.csv(map.q,paste0(data.path,"/USACE_20d.csv"),row.names = F)
