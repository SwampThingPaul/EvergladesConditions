
libs_val <- c("remotes","markdown", "rmarkdown", "knitr", "reshape2", "plyr", "zoo", 
              "lubridate", "RcppRoll", "dataRetrieval", "downloadthis", "rvest", 
              "httr", "mapmisc", "future", "future.apply", "flextable", "gdtools",
              "github::SwampThingPaul/AnalystHelper",
              "github::SwampThingPaul/EVERSpatDat")
pak::lockfile_create(libs_val, dependencies = TRUE, lockfile = ".github/pkg.lock")
