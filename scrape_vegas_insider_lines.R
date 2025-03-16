library(httr)

# Define the URL
url <- "https://www.vegasinsider.com/college-basketball/odds/las-vegas/#"

# Get the webpage content
response <- GET(url)

current_date <- Sys.Date()

# Format the date as yyyy_mm_dd
formatted_date <- format(current_date, "%Y_%m_%d")

# Save the HTML content to a file
html_content <- content(response, as = "text", encoding = "UTF-8")
writeLines(html_content, paste0(
  "C:/Users/ckoho/Documents/Inputs/NCAA/line/VegasInsider/VI_", formatted_date,
                                "_NCAAB.html"))
