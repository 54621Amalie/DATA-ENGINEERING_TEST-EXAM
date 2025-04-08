
pacman::p_load(httr, jsonlite, tidyverse, rlist, rjstat, rjson, Rcrawler)

base_url <- "https://restcountries.com/v3.1/all"
#hent url
api_call <- httr::GET(base_url)

http_type(api_call)
# application/json 
api_call$status_cod
#200
api_call$content
api_char <- base::rawToChar(api_call$content)

api_JSON <- jsonlite::fromJSON(api_char, flatten = TRUE)

countries <- as.data.frame(api_JSON)

head(countries)
cat("Antal lande:", nrow(countries), "\n")
cat("Gennemsnitlig befolkning:", mean(countries$population, na.rm = TRUE), "\n")
cat("Antal regioner:", length(unique(countries$region)), "\n")
