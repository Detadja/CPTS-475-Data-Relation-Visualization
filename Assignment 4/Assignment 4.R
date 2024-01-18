library(tidyverse)
flights = read.csv("flights.csv", sep = ",", header = TRUE)
airlines = read.csv("airlines.csv", sep = ",", header = TRUE)
airports = read.csv("airports.csv", sep = ",", header = TRUE)
planes = read.csv("planes.csv", sep = ",", header = TRUE)
weather = read.csv("weather.csv", sep = ",", header = TRUE)

#-------------------------------------------------------------------------------
#Left Join
fL_Join = flights %>%
  left_join(airlines, by = "carrier") %>%
  mutate(carriername = name) %>%
  select(-name)

fL_Join2 = flights %>%
  left_join(weather)

fL_Join3 = airports %>%
  left_join(flights, c("faa" = "dest"))

fL_Join4 = flights %>%
  left_join(airports, c("dest" = "faa"))

#Semi Join
aS_Join = airports %>% 
  semi_join(flights, c("faa" = "dest"))

#Anti Join
aA_Join = airports %>% 
  anti_join(flights, c("faa" = "dest"))

#Inner Join
ai_join = airports %>% 
  inner_join(flights, c("faa" = "dest"))
#-------------------------------------------------------------------------------

#1.)
#a.)
filtered_data = flights %>%
  left_join(weather) %>%
  filter(dest == "TPA", dep_time >= 1200, arr_time <= 1800, month == 11, day == 1, year == 2013) %>%
  select(tailnum, year, month, day, hour, origin, humid)
filtered_data
count(filtered_data)


#c .)
flight_count = airports %>% 
  inner_join(flights, c("faa" = "dest")) %>%
  mutate(dest = faa) %>%
  select(-faa) %>%
  select(origin, dest, lat, lon)

count(flight_count)  

#d.)
unique_count = flights %>%
  group_by(dest) %>%
  summarise(carrier) %>%
  unique()

nrow(unique_count)

#e.)
library(ggplot2)
library(ggmap)
library(maptools)
library(mapproj)

outgoing_flights = flights %>%
  left_join(airports, c("origin" = "faa")) %>%
  select(origin, lon, lat)

count = c(length(which(outgoing_flights$origin == "EWR")), length(which(outgoing_flights$origin == "LGA")), length(which(outgoing_flights$origin == "JFK")))
column = c("EWR", "LGA", "JFK")
size = data.frame(column, count)

outgoing_flights = outgoing_flights %>%
  left_join(size, c("origin" = "column"))

ggplot(outgoing_flights, aes(x = lon, y = lat, size = count)) +
  geom_point(alpha= 0.2) +
  scale_size_continuous(range = c(1, 3), name = "Number of Flights") + 
  borders("state") +
  geom_point() +
  coord_map(xlim = c(-74.2, -73.75), ylim = c(40.6, 40.8)) +  
  labs(x = "Longitude", y = "Latitude", title = "Magnitude of Outgoing Flights by Origin")


#2.)
library(usmap)
us_predidents = read.csv("us-presidents.csv", sep = ",", header = TRUE)
str(us_predidents)

pres_year1 = us_predidents %>%
  subset(select = -office)
pres_year1 = pres_year1[pres_year1$year == 1976, ]

pres_year2 = us_predidents %>%
  subset(select = -office)
pres_year2 = pres_year2[pres_year2$year == 2020, ]

plot_usmap(data = pres_year1, values = "totalvotes") +
  scale_fill_continuous(low = "white", high = "blue", name = "Number of Votes", label = scales::comma) + 
  labs(title = "1976 Presidential Vote Distribution", subtitle = "Number of presidential votes by state in 1976") +
  theme(legend.position = "right")

plot_usmap(data = pres_year2, values = "totalvotes") +
  scale_fill_continuous(low = "white", high = "purple", name = "Number of Votes", label = scales::comma) + 
  labs(title = "2020 Presidential Vote Distribution", subtitle = "Number of presidential votes by state in 2020") +
  theme(legend.position = "right")


#3.)
library(wordcloud)
library(tm)

text = readLines(file.choose())
document = Corpus(VectorSource(text))

space = content_transformer(function (x, pattern) gsub(pattern, " ", x))
document = tm_map(document, space, "/")
document = tm_map(document, space, "@")
document = tm_map(document, space, "\\|")
document = tm_map(document, space, "“")
document = tm_map(document, space, "”")
document = tm_map(document, space, "’")
document = tm_map(document, space, "…")

document = tm_map(document, content_transformer(tolower))
document = tm_map(document, removeNumbers)
document = tm_map(document, removeWords, stopwords("english"))
document = tm_map(document, removePunctuation)
document = tm_map(document, stripWhitespace)

tdoc_matrix = TermDocumentMatrix(document)
doc_matrix = as.matrix(tdoc_matrix)
frequent = sort(rowSums(doc_matrix), decreasing = TRUE)
text_data = data.frame(word = names(frequent), freq = frequent)

str(text_data)

wordcloud(words = text_data$word, freq = text_data$freq, min.freq = 1, max.words = 300, random.order = FALSE, 
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"))






