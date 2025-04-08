
pacman::p_load(httr, jsonlite, tidyverse, rlist, rjstat, rjson, Rcrawler, ggplot2, plumber, maps, sf, rnaturalearth, rnaturalearthdata, RColorBrewer)

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
cat("Gennemsnitlig befolkning:", mean(countries$population, na.rm = TRUE), "\n")

names(countries)
view(countries)

# How many countries speak Spanish English and French? --------------------
# Find alle sprog
all_languages <- countries$languages
cat("Antal lande:", nrow(countries), "\n")
#250 lande i alt
#languages findes ikke i API'en


# Which region has the most countries? ------------------------------------
cat("Antal regioner:", length(unique(countries$region)), "\n")
#6 forskellige

region_count <- countries %>%
  filter(!is.na(region)) %>%
  count(region, sort = TRUE)

# Print alle regioner og antallet af lande
cat("Region med flest lande:", region_count$region[1], "med", region_count$n[1], "lande\n")
#Afika med 59 lande

#hvad med resten?
cat("Antal lande per region:\n")
for (i in 1:nrow(region_count)) {
  cat(region_count$region[i], ":", region_count$n[i], "lande\n")
}
#Africa : 59 lande
#Americas : 56 lande
#Europe : 53 lande
#Asia : 50 lande
#Oceania : 27 lande
#Antarctic : 5 lande


# Top ten most and least populated countries? -----------------------------

top_10_most <- countries %>%
  select(name.common, population) %>%
  arrange(desc(population)) %>%
  slice_head(n = 10)

top_10_least <- countries %>%
  select(name.common, population) %>%
  arrange(population) %>%
  slice_head(n = 10)

cat("Top 10 mest befolkede lande:\n")
print(top_10_most)
#Kina er nr 1

cat("\nTop 10 mindst befolkede lande:\n")
print(top_10_least)
#Bouvet Island og Heard Island and McDonald Islands har kun pengviner og 0 indbyggere


# Hvilken tidszone har flest lande? ---------------------------------------
timezone_count <- countries %>%
  filter(!is.na(timezones)) %>%
  unnest(timezones) %>%
  count(timezones, sort = TRUE)

# Print den tidszone med flest lande
cat("Tidszone med flest lande:", timezone_count$timezones[1], "med", timezone_count$n[1], "lande\n")
#UTC+01:00 med 50 lande

# alle oversigt
cat("Antal lande per tidszone:\n")
for (i in 1:nrow(timezone_count)) {
  cat(timezone_count$timezones[i], ":", timezone_count$n[i], "lande\n")
}

# hvor mange tidszoner har kun 1 land?
single_country_timezones <- timezone_count %>%
  filter(n == 1)

cat("Antal tidszoner med kun ét land:", nrow(single_country_timezones), "\n")
#9 tidszoner har kun 1 land




# Make a plot visualizing point 2 ---------------------

region_count <- countries %>%
  filter(!is.na(region)) %>%
  count(region, sort = TRUE)

# Lav et plot
ggplot(region_count, aes(x = reorder(region, n), y = n, fill = region)) +
  geom_bar(stat = "identity") +
  labs(title = "Antal Lande Per Region", x = "Region", y = "Antal Lande")

# Gem plot som PNG-fil
ggsave("region.png", width = 10, height = 6)

pr <- plumber$new()

pr$handle("GET", "/plot", function(req, res) {
  res$file("top_populous_countries.png")
})

# Kør serveren
pr$run(port = 8000)


# Make a plot visualizing your point 4 ------------------------------------

ggplot(timezone_count, aes(x = reorder(timezones, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Bar chart
  coord_flip() +  # Gør det til et horizontal barplot
  labs(title = "Antal lande per tidszone",
       x = "Tidszone",
       y = "Antal Lande") +
  theme_minimal()  # Ren og enkel theme