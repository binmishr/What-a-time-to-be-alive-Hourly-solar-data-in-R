# What-a-time-to-be-alive-Hourly-solar-data-in-R

Hourly Data from the NASA POWER API
< /a> I absolutely love Adam Sparks’ nasapower package; it’s no exaggeration to say that this is definitely my favourite CRAN package (maybe nicethings is a close second, but it’s not even on CRAN last I checked, so). As a wrapper for the NASA POWER API, nasapower is just brilliant. The problem is though that the API at this stage has a serious limitation on the time resolution of the data it allows you to download–at the moment, the finest temporal resolution is daily, so you can seriously miss out on some important details within the data. NASA have made clear that they will begin allowing hourly data downloads by July, and nasapower will be changing to meet the times. What if you need hourly data now, however, and are willing to take a risk with the beta version of the NASA POWER API? The Beta version of the NASA POWER API does indeed allow downloads hourly data. Although not perfect, hourly data is a big improvement. This means though that you would forget about the easy to use wrappers and instead focus on getting/scanning any dangers. In general, I found it difficult to get comprehensive guides to using APIs in R; there seemed not to be a clear and reproducible fix which always worked with all APIs. In the end, I found the answer lying on Tony Haber’s blog. The answer is adapated for our needs.

# Converting an API into a useable format 
# We will need to use the following packages
library(httr)
# for the GET request 
library(jsonlite)
# for the fromJSON -- note that there are always competitors for this 
library(tibble)
# for enframe

#The first request we download is for GHI, or "ALLSKY" insolation
#The second is for temperatures

#The query string for the API request is based a start and end dates of 1 Aug 2009 to 1 Sep 2009, in a YYYYMMDD format
#We give the longitude and latitude of Qatar as 51.25 and 25.2 (these are approximate)
#and then use parameters as "ALLSKY_SFC_SW_DWN" or "T2M" as appropriate

#set the URL to download hourly data 
response_qatar_aug_sep_2009 = GET("https://power.larc.nasa.gov/beta/api/temporal/hourly/point?start=20090801&end=20090901&latitude=25.2&longitude=51.25&community=re&parameters=ALLSKY_SFC_SW_DWN&format=json&user=dataqueryabed&header=false&time-standard=lst")
response_qatar_aug_sep_2009_temperatures = GET("https://power.larc.nasa.gov/beta/api/temporal/hourly/point?start=20090801&end=20090901&latitude=25.2&longitude=51.25&community=re&parameters=T2M&format=json&user=dataqueryabed&header=false&time-standard=lst")

#Let's ignore the metadata and take only the "content"
content(response_qatar_aug_sep_2009, as="text")
content(response_qatar_aug_sep_2009_temperatures, as = "text")

#We want this not to be in JSON, but a "list" type of data
# unframed_response = (fromJSON(rawToChar(response_qatar_aug_sep_2009$content)))
unframed_response = (fromJSON(rawToChar(response_qatar_aug_sep_2009$content)))
unframed_response_temperatures = (fromJSON(rawToChar(response_qatar_aug_sep_2009_temperatures$content)))

#Following Tony El Habr's steps 
#framed_response = enframe(unlist(ugly_response))
framed_response = enframe(unlist(unframed_response))
framed_response_temperatures = enframe(unlist(unframed_response_temperatures))
#We now have hourly data from the NASA POWER API 
#We can remove the metadata from both dataframes 

# Look at the structure of these data frames however. The data we actually want 
# has a row name in the $name column of something like "properties.parameter..." etc
# Let's remove the metadata rows, and let's do it with a function to make life more modular

remove_metadata_from_framed <- function(data_input_framed)
{
  rows_to_remove = vector()
  for(i in 1:nrow(data_input_framed))
  {
    #Make sure you have stringr installed and called 
    if(! str_starts(data_input_framed$name[i], "properties"))
    {
      rows_to_remove = append(rows_to_remove, i)
    }
  }
  framed_no_metadata = data_input_framed[-c(rows_to_remove),]
  return(framed_no_metadata)
}

#This simple function deletes rows where the row name does not 
# begin with "properties"

qatar_august_GHI = remove_metadata_from_framed(framed_response)
qatar_august_T2M = remove_metadata_from_framed(framed_response_temperatures)

#A word to the wise: the units for GHI are now Wh/m^2 instead of kWh/m^2
#Just as a kind of basic check, we can confirm too that the solar insolation in Qatar
#is a likely predictor for the temperatures there 

> summary(lm(qatar_august_T2M$value ~ qatar_august_GHI$value))$r.squared
[1] 0.8927242
