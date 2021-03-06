library(RCurl)
library(XML)
library(rvest)
library(knitr)
library(dplyr)
library(stringr)
library(devtools)
install_github('ramnathv/slidify')
install_github('ramnathv/slidifyLibraries')

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
beer_df$address <- gsub('(\\.)([[:upper:]])', '\\1 \\2',beer_df$address)
beer_df$address <- gsub('([[:upper:]])([[:upper:]])', '\\1 \\2',beer_df$address)

# change the factor for average beer rating into a numeric
# http://stackoverflow.com/questions/3418128/how-to-convert-a-factor-to-an-integer-numeric-without-a-loss-of-information

beer_df$beer_avg <- as.numeric(levels(beer_df$beer_avg))[beer_df$beer_avg]
beer_df$brewery_rating <- as.numeric(levels(beer_df$brewery_rating))[beer_df$brewery_rating]
beer_df$number_of_reviews <- as.numeric(beer_df$number_of_reviews)
View(beer_df)


top_beer <-     beer_df %>%
  filter(!is.na(number_of_reviews)) %>%
  filter(!is.na(beer_avg)) %>%
  filter(number_of_reviews >= 100 | beer_avg >= 4) %>%
  arrange(desc(beer_avg),desc(number_of_reviews))

top_brewery <-     beer_df %>%
  filter(!is.na(number_of_reviews)) %>%
  filter(!is.na(brewery_rating)) %>%
  filter(number_of_reviews >= 50 & brewery_rating >= 4) %>%
  arrange(desc(brewery_rating),desc(number_of_reviews))

View(top_beer)
View(top_brewery)

library(htmlwidgets)
library(leaflet)
library(ggmap)
library(dplyr)
library(RColorBrewer)

# This function geocodes a location (find latitude and longitude) using the Google Maps API
geo <- geocode(location = top_beer$address, output="latlon", source="google")
geo2 <- geocode(location = top_brewery$address, output="latlon", source="google")

# add those coordinates to our dataset
top_beer$lon <- geo$lon
top_beer$lat <- geo$lat

# add those coordinates to our dataset
top_brewery$lon <- geo2$lon
top_brewery$lat <- geo2$lat


# create the pop-up info
top_beer$pop_up_content <- paste(sep = "<br/>", 
                                    "<b><a href='", top_beer$brewery_name, "'>", top_beer$brewery_name, "</a></b>", top_beer$address,
                                    top_beer$phone_number, paste("Average beer rating: ", top_beer$beer_avg,"/5.0"))

# add colors corresponding to the average beer ratings  
pal <- colorNumeric(palette = "YlOrRd", top_beer$beer_avg, n = 5)


leaflet(data = top_beer) %>% 
  addTiles() %>%
  setView(lng =-119.417931, lat=36.778259, zoom = 6) %>%
  addMarkers(lng = top_beer$lon[!is.na(top_beer$lon)], 
             lat = top_beer$lat[!is.na(top_beer$lat)], popup = ~as.character(pop_up_content))

leaflet(top_beer) %>% 
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png', 
                              attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>%
  setView(-119.417931, 36.778259, zoom = 5) %>% 
  addCircleMarkers(lng = top_beer$lon[!is.na(top_beer$lon)], 
                 lat = top_beer$lat[!is.na(top_beer$lat)], radius=5, 
                 stroke = TRUE, fillOpacity = 1,
                 popup = ~as.character(pop_up_content), 
                 color = ~pal(beer_avg)) %>% 
  addLegend("topright", pal = pal, values = top_beer$beer_avg,
                title = "Average Beer Ratings at Brewery",
                opacity = 1)


top_brewery$pop_up_content <- paste(sep = "<br/>", 
                                 "<b><a href='", top_beer$brewery_name, "'>", top_beer$brewery_name, "</a></b>", top_beer$address,
                                 top_beer$phone_number, paste("Average brewery rating: ", top_brewery$brewery_rating,"/5.0"))

pal <- colorNumeric(palette = "YlOrRd", top_brewery$brewery_rating)

leaflet(data = top_brewery) %>% 
  addTiles() %>%
  setView(lng =-119.417931, lat=36.778259, zoom = 6) %>% 
  addCircleMarkers(lng = top_brewery$lon[!is.na(top_brewery$lon)], 
                   lat = top_brewery$lat[!is.na(top_brewery$lat)], popup = ~as.character(pop_up_content), color = ~pal(top_beer$brewery_rating)) %>%
  addLegend("topright", pal = pal, values = top_brewery$brewery_rating,
            title = "Brewery Ratings",
            opacity = 1)





