library(RCurl)
library(XML)
library(rvest)
library(knitr)
library(dplyr)
library(stringr)

#parse the URL

theURL <- "https://www.beeradvocate.com/place/list/?start=0&&c_id=US&s_id=CA&brewery=Y&sort=name"
pageURL <- vector(mode="character", length=0)
brewery <- vector(mode="character", length=0)
final <- vector(mode="character", length=0)
final_add <- vector(mode="character", length=0)
brewery_add <- vector(mode="character", length=0)


scrape_brewery_links <- function(page_data){
  brewery <- page_data %>%
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "hr_bottom_light", " " ))]//b | //a//b') %>%
    html_text()
  brewery <- brewery[-1]
  
  return(brewery)
}

scrape_brewery_address <- function(page_data){
  brewery <- page_data %>%
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "hr_bottom_dark", " " )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]') %>%
    html_text()
  #brewery <- brewery[-1]
  #output <- matrix(unlist(brewery), ncol = 5, byrow = TRUE)
  return(brewery)
}

#get the total number of pages

pages <- read_html(theURL) %>%
  html_nodes(xpath='//span//b[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]') %>%
  html_text()

# regex to convert it into numbers
results <- str_extract(pages,"of \\d+")
results <- as.numeric(str_extract(results,"\\d+"))

# need this conversion for the website and how they name the page numbers.
page_no = results/20;
page_no = ceiling(page_no) #number of pages
page_no_url = page_no*20 #for URL string

# Loop through all the pages :
loop_URL <- "https://www.beeradvocate.com/place/list/?start="
for (i in c(1:page_no)){
  Sys.sleep(1)
  page_no_url = (i-1)*20
  pageURL[i] <- paste(loop_URL,page_no_url,"&c_id=US&s_id=CA&brewery=Y&sort=name",sep = "")
  page_data <- read_html(pageURL[i])
  brewery = scrape_brewery_links(page_data)
  final = append(final,brewery)
  remove(brewery)
  brewery_add = scrape_brewery_address(page_data)
  final_add = append(final_add,brewery_add)
  remove(brewery_add)
}

brewery_mat <- matrix(unlist(final), ncol = 5, byrow = TRUE)
brewery_final <- cbind(brewery_mat,final_add)
View(brewery_final)
beer_df <- as.data.frame(brewery_final)
head(beer_df)

# change the column names
colnames(beer_df) <- c("brewery_name", "brewery_rating", "number_of_reviews", "beer_avg", "number_of_beers", "address")

# get our missing values as NA
beer_df[beer_df == "-"] = NA

# clean up some of the columns and remove extra data
beer_df$address <- as.character(beer_df$address) # change into character data type

library(stringr)
beer_df$phone_number <- str_extract_all(beer_df$address, "\\(?\\d{3}\\)?[.-]? *\\d{3}[.-]? *[.-]?\\d{4}")
beer_df$phone_number[beer_df$phone_number == "character(0)"] = NA

# remove the phone number tag from the address field for geocoding
beer_df$address <- gsub("\\(?\\d{3}\\)?[.-]? *\\d{3}[.-]? *[.-]?\\d{4}", "", beer_df$address)
beer_df$address <- gsub("United States", " United States", beer_df$address)
beer_df$address <- gsub('([[:lower:]])([[:upper:]])', '\\1 \\2',beer_df$address)
beer_df$address <- gsub('([[:digit:]])([[:upper:]])', '\\1 \\2',beer_df$address)

# change the factor for average beer rating into a numeric
# http://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-an-integer-numeric-without-a-loss-of-information

beer_df$beer_avg <- as.numeric(levels(beer_df$beer_avg))[beer_df$beer_avg]
beer_df$number_of_reviews <- as.numeric(beer_df$number_of_reviews)
View(beer_df)

beer_df = subset(beer_df, select = -c(11))

library(htmlwidgets)
library(leaflet)
library(ggmap)
library(dplyr)
library(RColorBrewer)

# This function geocodes a location (find latitude and longitude) using the Google Maps API
geo <- geocode(location = beer_df$address, output="latlon", source="google")
?geocode

# add those coordinates to our dataset
beer_df$lon <- geo$lon
beer_df$lat <- geo$lat

top_brewery <-     beer_df %>%
  filter(!is.na(number_of_reviews)) %>%
  filter(!is.na(beer_avg)) %>%
  filter(number_of_reviews >= 100 | beer_avg >= 4) %>%
  arrange(desc(beer_avg),desc(number_of_reviews)) 
        
View(top_brewery)

m <- leaflet(top_brewery) %>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png', 
                              attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') 
m %>% setView(-119.417931, 36.778259, zoom = 5)
m %>% addCircleMarkers(lng = top_brewery$lon[!is.na(top_brewery$lon)], 
                 lat = top_brewery$lat[!is.na(top_brewery$lat)], radius=5, 
                 stroke = TRUE, fillOpacity = 1,
                 popup = ~as.character(pop_up_content), 
                 color = ~pal(beer_avg)) %>% 
  addLegend("topright", pal = pal, values = top_brewery$beer_avg,
                title = "Average Beer Ratings at Brewery",
                opacity = 1)






# create the pop-up info
top_brewery$pop_up_content <- paste(sep = "<br/>", 
                                      "<b><a href='", top_brewery$brewery_name, "'>", top_brewery$brewery_name, "</a></b>", top_brewery$address,
                                      top_brewery$phone_number, paste("Average beer rating: ", top_brewery$beer_avg,"/5.0"))

# add colors corresponding to the average beer ratings  
pal <- colorNumeric(palette = "YlOrRd", top_brewery$beer_avg, n = 5) 

m <- leaflet(top_brewery) %>% 
  addTiles() %>%
  setView(lng =-119.417931, lat=36.778259, zoom = 6) %>% 
  addCircleMarkers(lng = top_brewery$lon[!is.na(top_brewery$lon)], 
                   lat = top_brewery$lat[!is.na(top_brewery$lat)], popup = ~as.character(pop_up_content), color = ~pal(brewery_rating)) %>%
  addLegend("topright", pal = pal, values = top_brewery$brewery_rating,
            title = "Average Beer Ratings at Brewery",
            opacity = 1
  )
m

?colorNumeric

# Show first 20 rows from the `quakes` dataset
leaflet(data = top_brewery) %>% 
  addTiles() %>%
  setView(lng =-119.417931, lat=36.778259, zoom = 6) %>%
  addMarkers(lng = top_brewery$lon[!is.na(top_brewery$lon)], 
             lat = top_brewery$lat[!is.na(top_brewery$lat)], popup = ~as.character(pop_up_content))

is.numeric(top_brewery$lat)

library(ggmap)
california <- get_map(location = 'california', zoom = 6)

ggmap(california)

beer_location <- top_brewery %>%
  select(address,lon,lat, beer_avg) %>%
  filter(!is.na(lat) & !is.na(lon))

View(beer_location)

gsub('([[:lower:]])([[:upper:]])', '\\1 \\2', places)
beer_location$address <- gsub('([[:digit:]])([[:upper:]])', '\\1 \\2',beer_location$address)
write.csv(beer_location, file = "beer_locations.csv")

ggmap(california) + geom_tile(data = beer_location, aes(x = lon, y = lat, alpha = beer_avg),
                           fill = 'red') + theme(axis.title.y = element_blank(), axis.title.x = element_blank())


