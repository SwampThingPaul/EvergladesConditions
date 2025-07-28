library(rvest); 

url <- "https://w3.saj.usace.army.mil/h2o/reports/StatusDaily/archive/0728/StatusDaily.htm"

page <- read_html(url)
txt_nodes <- html_nodes(page, "pre")

# Extract all data  -------------------------------------------------------
if (length(txt_nodes) == 0) {
  # Fallback if no <pre> exists
  txt_nodes <- html_nodes(page, "body")
}

txt <- html_text(txt_nodes)

# Confirm result
cat(substr(txt, 1, 500))  # Print a snippet of the result


# Splite lines and extract fields -----------------------------------------
lines <- unlist(strsplit(txt, "\\r?\\n|\\r"))  # Handles all common line break styles

# Clean up whitespace
lines <- trimws(lines)
lines <- lines[lines != ""]  # Remove empty lines




# Extract values using grep and sub ---------------------------------------
lok_stg <- lines[grep("Lake Okeechobee stage:",lines)+1]|>
  strsplit("\\ ")

lok_stg <- sapply(lok_stg,"[",1)|>
  as.numeric()


## Extract all S-structures ------------------------------------------------
s_lines <- grep("^S-[0-9]+\\s*:", lines, value = TRUE)

# Extract structure name and flow using regular expressions
s_names <- sub("^([A-Z0-9-]+):.*", "\\1", s_lines)
s_flows <- as.numeric(sub("^[A-Z0-9-]+:\\s*([0-9.]+).*", "\\1", s_lines))
s_df <- data.frame(structure = s_names, flow_cfs = s_flows, stringsAsFactors = FALSE)

##
structure_flows <- list()
for (i in seq_along(lines)) {
  line <- lines[i]
  
  # Case 1: Value is on the same line (e.g., "S-271:   34")
  if (grepl("^S-[0-9]+:\\s*[0-9.]+", line)) {
    struct_name <- sub(":.*", "", line)
    struct_val <- as.numeric(sub(".*:\\s*", "", line))
    structure_flows[[struct_name]] <- struct_val
    
    # Case 2: Value is on next line (e.g., "S-77:" on one line, number on next)
  } else if (grepl("^S-[0-9]+\\s*:?\\s*$", line)) {
    struct_name <- gsub("[:\\s]", "", line)  # Clean "S-77:" or "S-77"
    
    # Look ahead to find numeric value
    lookahead <- lines[(i+1):min(i+3, length(lines))]
    value_line <- lookahead[grepl("^[0-9]+(\\.[0-9]+)?$", lookahead)][1]
    
    if (!is.na(value_line)) {
      structure_flows[[struct_name]] <- as.numeric(value_line)
    }
  }
}

s_df <- data.frame(
  structure = names(structure_flows),
  flow_cfs = as.numeric(unlist(structure_flows)),
  stringsAsFactors = FALSE
)


extract_name_value_pairs <- function(lines) {
  all_matches <- list()
  
  # Pattern: match name and value — e.g., "S-65E:   1143" or "Istokpoga: 305"
  pattern <- "([A-Za-z0-9\\- ]+):\\s*([0-9]+(?:\\.[0-9]+)?)"
  
  for (line in lines) {
    matches <- gregexpr(pattern, line, perl = TRUE)
    matched_text <- regmatches(line, matches)[[1]]
    
    for (m in matched_text) {
      name <- sub(":.*", "", m)
      value <- sub(".*:\\s*", "", m)
      all_matches[[trimws(name)]] <- as.numeric(value)
    }
  }
  
  # Convert to data.frame
  data.frame(
    location = names(all_matches),
    value = as.numeric(unlist(all_matches)),
    stringsAsFactors = FALSE
  )
}

lines <- trimws(lines[lines != ""])  # Clean whitespace
df <- extract_name_value_pairs(lines)
df <- rbind(df,
            data.frame(location = "LOK",value = lok_stg,
                       stringsAsFactors = FALSE))
print(df)
