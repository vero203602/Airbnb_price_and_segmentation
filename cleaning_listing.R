#####################################
###### Calendar:occupancy rate ######
#####################################

# upload libraries
library(lubridate)
library(dplyr)

# set working directory
setwd('')

jan2019 <- read.csv('calendar.csv')
feb2019 <- read.csv('calendar-2.csv')
mar2019 <- read.csv('calendar-3.csv')
apr2019 <- read.csv('calendar-4.csv')
may2019 <- read.csv('calendar-6.csv')
jun2019 <- read.csv('calendar-5.csv')
jul2019 <- read.csv('calendar-7.csv')
aug2019 <- read.csv('calendar-9.csv')
sep2019 <- read.csv('calendar-12.csv')
oct2019 <- read.csv('calendar-8.csv')
nov2019 <- read.csv('calendar-10.csv')
dec2019 <- read.csv('calendar-11.csv')

##### Create sets of occupancy rate per listing per month
# local
Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "en_GB.UTF-8")
# "it_IT.UTF-8"
# to go back Sys.setlocale("LC_TIME","it_IT.UTF-8")

### JANUARY ###
tonumber <- function(x) {     #function to change the price string into a number
  x <- substr(x, 2, nchar(x))
  x <- sub(',', '', x)
  x <- as.numeric(x)
}

occrate <- function(monthdf, strinmonth) { # function to compute occ.rate per month
  monthdf$date      <- as.Date(monthdf$date) # change into date format
  monthdf$month     <- month.abb[month(monthdf$date)] # short name for month
  monthdf$month     <- factor(monthdf$month,levels=month.abb) # month as factor - to order them
  monthdf$day       <- wday(monthdf$date, label=TRUE) #weekdays
  monthdf$year      <- year(monthdf$date) # get the year
  monthdf$binbooked <- ifelse(monthdf$available == 'f', 1, 0) # dummy variablee for booking 
  names(monthdf)[4] <- 'price_purchase'
  
  monthdf$price_purchase <- tonumber(monthdf$price_purchase)
  monthdf$adjusted_price <- tonumber(monthdf$adjusted_price)
  
  n <- monthdf %>% #count of the available nights per month
    group_by(listing_id, month, year) %>%
    tally() 
  
  n2 <- monthdf %>% # count of the booked nights per month
    filter(available == 'f') %>%
    group_by(listing_id, month, year) %>%
    tally()
  
  rate           <- merge(x = n, 
                          y = n2, 
                          by = c('listing_id', 'month', 'year'), 
                          all.x = TRUE)
  colnames(rate) <- c('listing_id', 'month', 'year', 'available','booked')
  rate$booked[is.na(rate$booked)] <- 0
  rate$monthrate <- rate$booked/rate$available
  # ratio between booked nights and total night: occupancy rate
  
  month_rate <- aggregate(rate[, 6], list(month = rate$month, 
                                          year  = rate$year), 
                          mean)
  names(month_rate)[3] <- 'occup_month_rate'
  month_rate_listing   <- aggregate(rate[, 6], list(list_id = rate$listing_id, 
                                                    month   = rate$month, 
                                                    year    = rate$year), 
                                    mean)
  rate_year <- mean(month_rate$occup_month_rate)
  
  # I filter this data on month == month & year == 2019 and merge with all the following 
  # so I will add this to the data set for price prediction
  month_to_merge <- month_rate_listing %>%
    filter(month == strinmonth & year == 2019)
  names(month_to_merge)[4] <- 'occupancyrate_list'
  month_to_merge <- transform(month_to_merge, diff_from_avg = 
                                month_to_merge$occupancyrate_list - month_rate$occup_month_rate[1])
  month_to_merge <- month_to_merge %>%
    dplyr::mutate(occupancyrate_list = occupancyrate_list*100,
                  diff_from_avg = diff_from_avg*100)
  
  return(month_to_merge)
}

strinmonth   <- 'Jan'
jan_to_merge <- occrate(jan2019, strinmonth)
rm(jan2019)
strinmonth   <- 'Feb'
feb_to_merge <- occrate(feb2019, strinmonth)
rm(feb2019)
strinmonth   <- 'Mar'
mar_to_merge <- occrate(mar2019, strinmonth)
rm(mar2019)
strinmonth   <- 'Apr'
apr_to_merge <- occrate(apr2019, strinmonth)
rm(apr2019)
strinmonth   <- 'May'
may_to_merge <- occrate(may2019, strinmonth)
rm(may2019)
strinmonth   <- 'Jun'
jun_to_merge <- occrate(jun2019, strinmonth)
rm(jun2019)
strinmonth   <- 'Jul'
jul_to_merge <- occrate(jul2019, strinmonth)
rm(jul2019)
strinmonth   <- 'Aug'
aug_to_merge <- occrate(aug2019, strinmonth)
rm(aug2019)
strinmonth   <- 'Sep'
sep_to_merge <- occrate(sep2019, strinmonth)
rm(sep2019)
strinmonth   <- 'Oct'
oct_to_merge <- occrate(oct2019, strinmonth)
rm(oct2019)
strinmonth   <- 'Nov'
nov_to_merge <- occrate(nov2019, strinmonth)
rm(nov2019)
strinmonth   <- 'Dec'
dec_to_merge <- occrate(dec2019, strinmonth)
rm(dec2019)

#######################################
###### LISTING: complete dataset ######
#######################################

library(stringr)
library(naniar)
library(visdat)
library(ggplot2)
library(lubridate)
library(dplyr)

# As literature opens up many different possibilities of analysis, the following 
# script aims to deepen the understanding of the case of Airbnb in Milan, after 
# the first step of EDA - exploratory data analysis.
# In particular, what I want to do in the next section is to create new variables 
# that are proven to be interesting in literature and recoded - when needed - 
# the existing ones.

setwd('/Users/veronicacipriani/Desktop/tesi/dati/listing_2019/listing_complete/')
list_room   <- read.csv('list_room.csv',   sep=',')[, -c(1)] # da jupyter
listjan2019 <- read.csv('listings.csv',    sep=',') 
listfeb2019 <- read.csv('listings-2.csv',  sep=',') 
listmar2019 <- read.csv('listings-3.csv',  sep=',') 
listapr2019 <- read.csv('listings-4.csv',  sep=',') 
listmay2019 <- read.csv('listings-5.csv',  sep=',') 
listjun2019 <- read.csv('listings-6.csv',  sep=',') 
listjul2019 <- read.csv('listings-7.csv',  sep=',') 
listaug2019 <- read.csv('listings-8.csv',  sep=',') 
listsep2019 <- read.csv('listings-9.csv',  sep=',') 
listoct2019 <- read.csv('listings-10.csv', sep=',') 
listnov2019 <- read.csv('listings-11.csv', sep=',') 
listdec2019 <- read.csv('listings-12.csv', sep=',') 

# initial check for missin values
 

missingval <- function(mesedf){
  #vis_dat(mesedf, warn_large_data = FALSE) 
  #gg_miss_var(mesedf) + theme_bw()
  miss_variables <- gg_miss_var(mesedf)$data$n_miss
  col_names      <- gg_miss_var(mesedf)$data$variable
  
  do.call(rbind, Map(data.frame, names = col_names, miss_var = miss_variables))%>%
    filter(miss_var > 0) %>%
    arrange(desc(miss_var))
}

# function to perform data cleaning on each month
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

tonumber <- function(x, start, end) {   
  x <- substr(x, start, end)
  x <- sub(',', '', x)
  x <- as.numeric(x)
}
cleanlisting <- function(dfmonth, occ_rate){
  
  dfmonth <- dfmonth[, -c(2,3,9,16,17,18,19,21,30,31,32,33,38,41,42,43,44,45,46,47,
                          48,49,50,70,71,72,73,74,75,76,82,84,94,95,96,103,104,105)]
  dfmonth$bathrooms <- round(dfmonth$bathrooms)
  # deal with NA in bathrooms, bedrooms, beds, host_total_listings_count
  dfmonth$bathrooms[is.na(dfmonth$bathrooms)] <- Mode(dfmonth[dfmonth$bedrooms == (dfmonth$bedrooms),]$bathrooms)
  dfmonth$bedrooms[is.na(dfmonth$bedrooms)] <- Mode(dfmonth[dfmonth$bedrooms == (dfmonth$bathrooms),]$bedrooms)
  dfmonth$beds[is.na(dfmonth$beds)] <- Mode(dfmonth[dfmonth$bedrooms == (dfmonth$bedrooms),]$beds)
  dfmonth$host_total_listings_count[is.na(dfmonth$host_total_listings_count)] <- round(mean(dfmonth$host_total_listings_count, 
                                                                                        na.rm = TRUE))
  dfmonth[, c(37, 55:61)][is.na(dfmonth[, c(37, 55:61)])] <- 0 
  dfmonth$reviews_per_month[is.na(dfmonth$reviews_per_month)] <- 0
  for (i in 1:nrow(dfmonth)){ # if there are no review I save the "day since last"
                              # as "days host since" - approximation
    if (dfmonth$reviews_per_month[i] == 0.00) {
      dfmonth$days_first_review[i] <- dfmonth$days_host_since[i]
      dfmonth$days_last_review[i]  <- dfmonth$days_host_since[i]
    }
  }
  # some NAs are coded as "" or "N/A"
  dfmonth <- dfmonth[!(dfmonth$neighbourhood == ""), ] #dropped as R cannot go back to location
  
  dfmonth <- dfmonth %>%  # change strings "" "N/A" to NA and deal with these
    mutate_at(vars(host_response_time, host_response_rate, host_acceptance_rate),
              na_if, 'N/A') %>%
    mutate_at(vars(host_response_time, host_response_rate, host_acceptance_rate),
              na_if, '')
  dfmonth$host_response_time[is.na(dfmonth$host_response_time)] <- 'no info'
  dfmonth$host_response_rate[is.na(dfmonth$host_response_rate)] <- 0
  # acceptance rate will be dropped as it is only counts NA
  dfmonth$host_response_time <- factor(dfmonth$host_response_time,
                                       levels=c("within an hour",
                                                "within a few hours",
                                                "within a day",
                                                "a few days or more",
                                                "no info"))
  dfmonth$host_response_rate[dfmonth$host_response_rate == ""]  <- 0
  dfmonth$host_response_rate <- as.numeric(sub("%", "",
                                               dfmonth$host_response_rate))
  dfmonth[, c(19,20)][is.na(dfmonth[, c(19,20)])] <- 0
  dfmonth$host_response_rate   <- as.numeric(dfmonth$host_response_rate)
  dfmonth$host_acceptance_rate <- as.numeric(dfmonth$host_acceptance_rate)
  # some variables recored as '' for missing values:
  # I drop them as we can't assign the neighbourhood

  # date objects
  dfmonth$last_scraped <- as.Date(dfmonth$last_scraped)
  dfmonth$month        <- month.abb[month(dfmonth$last_scraped)] # get the month 
  dfmonth$host_since   <- as.Date(dfmonth$host_since) #change into days since --> more easy to use
  dfmonth$first_review <- as.Date(dfmonth$first_review) # change into days since
  dfmonth$last_review  <- as.Date(dfmonth$last_review) # change into days since
  
  dfmonth$days_host_since   <- as.integer(difftime(as.Date(min(dfmonth$last_scraped)),
                                               as.Date(dfmonth$host_since), units = "days")) 
  dfmonth$days_first_review <- as.integer(difftime(as.Date(min(dfmonth$last_scraped)),
                                                 as.Date(dfmonth$first_review), units = "days"))
  dfmonth$days_last_review  <- as.integer(difftime(as.Date(min(dfmonth$last_scraped)),
                                                as.Date(dfmonth$last_review), units = "days")) 
  # Price variables 
  dfmonth$price           <- tonumber(dfmonth$price, 2, nchar(dfmonth$price))
  dfmonth$weekly_price    <- tonumber(dfmonth$weekly_price, 2, nchar(dfmonth$weekly_price))
  dfmonth$weekly_discount <- ifelse(is.na(dfmonth$weekly_price), 0, 1) 
  dfmonth$monthly_price   <- tonumber(dfmonth$monthly_price, 2, nchar(dfmonth$monthly_price))
  dfmonth$monthly_discount<- ifelse(is.na(dfmonth$monthly_price), 0, 1) 
  dfmonth$security_deposit<- tonumber(dfmonth$security_deposit, 2, nchar(dfmonth$security_deposit))
  dfmonth$cleaning_fee    <- tonumber(dfmonth$cleaning_fee, 2, nchar(dfmonth$cleaning_fee))
  dfmonth$extra_people    <- tonumber(dfmonth$extra_people, 2, nchar(dfmonth$extra_people))
  # y = total price will be price+cleaning fee
  dfmonth$monthly_price   <- round((((dfmonth$weekly_price*4)-
                                   dfmonth$monthly_price)*100)/(dfmonth$weekly_price*4),2)
  dfmonth$weekly_price    <- round((((dfmonth$price*7)-
                                  dfmonth$weekly_price)*100)/(dfmonth$price*7),2)
  # change NA value to zero for:
  # square_feet      --> penalize the missing info
  # weekly_price     --> missing info assumed as no discount
  # monthly_price    --> missing info assumed as no discount
  # security_deposit --> missing info assumed as no security deposit
  # cleaning_fee     --> missing info assumed as no costs
  dfmonth[, c(38:42)][is.na(dfmonth[, c(38:42)])] <- 0
  dfmonth$price <- rowSums(dfmonth[c(38, 42)])
  dfmonth <- dfmonth %>%
    filter(between(price, 1, 750))
  # Dummies  
  dfmonth$summary     <- ifelse(dfmonth$summary != "", 1, 0)
  dfmonth$space       <- ifelse(dfmonth$space != "", 1, 0)
  dfmonth$description <- ifelse(dfmonth$description != "", 1, 0)
  dfmonth$neighborhood_overview <- ifelse(dfmonth$neighborhood_overview != "", 1, 0)
  dfmonth$notes       <- ifelse(dfmonth$notes != "", 1, 0)
  dfmonth$transit     <- ifelse(dfmonth$transit != "", 1, 0)
  dfmonth$access      <- ifelse(dfmonth$access != "", 1, 0)
  dfmonth$interaction <- ifelse(dfmonth$interaction != "", 1, 0)
  dfmonth$house_rules <- ifelse(dfmonth$house_rules != "", 1, 0)
  
  dfmonth$host_about <- gsub("[[:blank:]]", "", dfmonth$host_about)
  dfmonth$host_about <- ifelse(dfmonth$host_about != "", 1, 0)
  
  milan = 'Milan' 
  dfmonth$host_location          <- ifelse(grepl(milan, dfmonth$host_location, fixed = TRUE), 1,0)
  dfmonth$host_is_superhost      <- ifelse(dfmonth$host_is_superhost == "t", 1, 0)
  dfmonth$host_has_profile_pic   <- ifelse(dfmonth$host_has_profile_pic == "t", 1, 0)
  dfmonth$host_identity_verified <- ifelse(dfmonth$host_identity_verified == "t", 1, 0)
  dfmonth$is_location_exact      <- ifelse(dfmonth$is_location_exact == "t", 1, 0)
  dfmonth$host_identity_verified <- ifelse(dfmonth$host_identity_verified == "t", 1, 0)
  dfmonth$has_availability       <- ifelse(dfmonth$has_availability == "t", 1, 0)
  dfmonth$instant_bookable       <- ifelse(dfmonth$instant_bookable == "t", 1, 0)
  dfmonth$is_business_travel_ready <- ifelse(dfmonth$is_business_travel_ready == "t", 1, 0)
  dfmonth$require_guest_profile_picture <- ifelse(dfmonth$require_guest_profile_picture == "t", 1, 0)
  dfmonth$require_guest_phone_verification <- ifelse(dfmonth$require_guest_phone_verification == "t", 1, 0)
  
  # factors
  dfmonth$cancellation_policy <- factor(dfmonth$cancellation_policy,
                                      levels=c('flexible_new',
                                               'moderate_new',
                                               'strict_new',
                                               'super_strict_30_new',
                                               'super_strict_60_new'))
  dfmonth$bed_type            <- factor(dfmonth$bed_type,
                                      levels=c("Real Bed",
                                               "Pull-out Sofa",
                                               "Futon",
                                               "Couch",
                                               "Airbed"))
  dfmonth$month               <- factor(dfmonth$month,
                                        levels=month.abb)
  dfmonth$room_type           <- factor(dfmonth$room_type,
                                        levels=c("Shared room",
                                                 "Private room",
                                                 "Entire home/apt",
                                                 "Hotel room"))
  # strings
  for (i in(1:nrow(dfmonth))){
    x <- gsub("([,])|[[:punct:]]", "\\1", dfmonth$host_verifications[i])
    x <- gsub(" ", "", x, fixed = TRUE)
    x <- strsplit(x, split = ",")
    x <- unique(x[[1]])
    dfmonth$host_verifications[i] <- length(x)
  }
  
  dfmonth$host_verifications <- as.integer(dfmonth$host_verifications)
  
  dfmonth$number_amenities <- ''
  for (i in (1:nrow(dfmonth))){
    x <- gsub("([,])|[[:punct:]]", "\\1", dfmonth$amenities[i])
    x <- strsplit(x, split = ",")
    x <- unique(x[[1]])
    dfmonth$number_amenities[i] <- length(x)
    x <- list(x)
    dfmonth$amenities[i] <- x
  }
  dfmonth$number_amenities <- as.numeric(dfmonth$number_amenities)
  lista <- dfmonth[, 36]
  res <- do.call(c, lista)
  #length(unique(res))
  element <- c()
  count_occ <- c()
  for (elem in unique(res)){
    n <- length(grep(elem, res))
    element <- c(element, elem)
    count_occ <- c(count_occ, n)
  }

  for (i in (1:nrow(dfmonth))){
    dfmonth$amen_wifi[i]         <- ifelse ( length(grep('Wifi', 
                                               dfmonth$amenities[i][[1]])) > 0, 1,0)
    dfmonth$amen_kitchen[i]      <- ifelse ( length(grep('Kitchen', 
                                                  dfmonth$amenities[i][[1]])) > 0, 1,0)
    dfmonth$amen_hair_dryer[i]   <- ifelse ( length(grep('Hair dryer', 
                                                     dfmonth$amenities[i][[1]])) > 0, 1,0)
    dfmonth$amen_hosts_greets[i] <- ifelse ( length(grep('Host greets you', 
                                                       dfmonth$amenities[i][[1]])) > 0, 1,0)
    dfmonth$amen_free_parking[i] <- ifelse ( length(grep('Free street parking', 
                                                       dfmonth$amenities[i][[1]])) > 0, 1,0)
    dfmonth$amen_bedlinens[i]    <- ifelse ( length(grep('Bed linens', 
                                                    dfmonth$amenities[i][[1]])) > 0, 1,0)
    dfmonth$amen_pets[i]         <- ifelse ( length(grep('Pets allowed', 
                                               dfmonth$amenities[i][[1]])) > 0, 1,0)
    dfmonth$amen_smoke[i]        <- ifelse ( length(grep('Smoking allowed', 
                                                dfmonth$amenities[i][[1]])) > 0, 1,0)
  }
  dfmonth <- dfmonth[, -c(2, 3, 13, 14, 15, 20, 36, 42, 53, 54, 67)]
  dfmonth <- merge(dfmonth, list_room[, c(1, 20:22)], by = 'id', all.x = TRUE)
  dfmonth <- merge(dfmonth, occ_rate[, c(1,4,5)], by.x = 'id', by.y = 'list_id', all.x = TRUE)
  
  return(dfmonth)
}

jan <- cleanlisting(listjan2019, jan_to_merge) 
feb <- cleanlisting(listfeb2019, feb_to_merge) 
mar <- cleanlisting(listmar2019, mar_to_merge) 
apr <- cleanlisting(listapr2019, apr_to_merge) 
may <- cleanlisting(listmay2019, may_to_merge) 
jun <- cleanlisting(listjun2019, jun_to_merge) 
jul <- cleanlisting(listjul2019, jul_to_merge) 
aug <- cleanlisting(listaug2019, aug_to_merge) 
sep <- cleanlisting(listsep2019, sep_to_merge) 
oct <- cleanlisting(listoct2019, oct_to_merge) 
nov <- cleanlisting(listnov2019, nov_to_merge) 
dec <- cleanlisting(listdec2019, dec_to_merge) 

#FINAL DATASET
df_month <- rbind(jan, feb, mar, apr, may, jun,
                  jul, aug, sep, oct, nov, dec)

write.csv(x=df_month, file="/Users/veronicacipriani/Desktop/tesi/dati/listing_2019/listing_complete/price_pred/df_month.csv")

setwd = ("/Users/veronicacipriani/Desktop/tesi/dati/listing_2019/listing_complete/price_pred")
# First, I check the average number of max people accommodates in each listing
# when the avg is less than people included in the pruce per night, there must 
# be some mistake
df_month %>% 
  dplyr::group_by(guests_included) %>%
  dplyr::summarise(peoplemax = mean(accommodates))

# --> CHECKING IN THE WEBSITES FOR SOME OF THE ENTRIES OF THE SUBSET2, WE CAN
#     SEE THAT THE MAX NUMBER OF PEOPLE THAT CAN BOOK THE PLACE IS ACCOMMODATES,
#     GUESTS_INCLUDED DO NOT FIT. WE CHANGE THOSE VALUES WITH GUESTS INCLUDED
subset2 <- df_month %>%
  dplyr::select(id, guests_included, accommodates, bathrooms, bedrooms, beds,
                price, property_type) %>%
  #dplyr::filter(guests_included > 9 & guests_included != 12) %>%
  dplyr::filter(guests_included > accommodates)

df_month <- mutate(df_month,
                   guests_included = case_when(
                     (guests_included > accommodates) ~ as.numeric(accommodates),
                     TRUE ~ as.numeric(guests_included)))

# Then, I check the average number of guest_included per accommodates 
df_month %>% 
  dplyr::group_by(accommodates) %>%
  dplyr::summarise(guest_included = mean(guests_included))
# It makes sense
# Final check if there is any observation with guests_included > accommodates
df_month %>%
  dplyr::filter(guests_included > accommodates) %>%
  nrow
range(df_month$guests_included)

# another problem is: number of bedrooms and number of bathrooms 
# BEDROOMS
df_month %>% 
  dplyr::group_by(bedrooms) %>%
  dplyr::summarise(guest_included = mean(guests_included),
                   bed_included   = mean(beds))

subset_bedapt <- df_month %>%
  dplyr::select(id, guests_included, accommodates, bathrooms, bedrooms, beds,
                price, property_type, room_type) %>%
  dplyr::filter(bedrooms > accommodates) %>%
  dplyr::filter(room_type == 'Entire home/apt' | room_type == 'Hotel room')

subset_bedpri <- df_month %>%
  dplyr::select(id, guests_included, accommodates, bathrooms, bedrooms, beds,
                price, property_type, room_type) %>%
  dplyr::filter(bedrooms > accommodates) %>%
  dplyr::filter(room_type != 'Entire home/apt' | room_type != 'Hotel room')

df_month <- mutate(df_month,
                   bedrooms = case_when(
                     (room_type == 'Private room') ~ 1, 
                     (room_type == 'Shared room')  ~ 1,
                     #(room_type %in% c('Entire home/apt', 'Hotel room') &
                     #   bedrooms > beds) ~ as.numeric(beds),
                     TRUE ~ as.numeric(bedrooms)))
df_month %>%
  dplyr::group_by(bedrooms) %>%
  dplyr::summarise(guest_included = mean(guests_included),
                   bed_included   = mean(beds),
                   accomm         = mean(accommodates))
#subset_prova1 <- df_month[df_month$bedrooms == 8 | df_month$bedroom == 13 ,]
df_month      <- df_month[! df_month$bedrooms == 8 | df_month$bedroom == 13 ,]
# non sense, cannot check on the 
# website for more info
# these lines are dropped

df_month %>%
  dplyr::group_by(guests_included) %>%
  dplyr::summarise(bedrooms     = mean(bedrooms),
                   bed_included = mean(beds),
                   accomm       = mean(accommodates))
prova1_out <- df_month[df_month$guests_included == 16 ,] # not possible for a house
# to have a single room and a single bed and accommodates/include 16 people.
# I change this to 2, as 1 bed can be for 2 people max.
df_month <- df_month[!df_month$guests_included == 16 ,]

# BATHROOM
df_month %>% 
  dplyr::group_by(bathrooms) %>%
  dplyr::summarise(guest_included = mean(guests_included),
                   bed_included   = mean(beds))
# as for bedrooms, some weird numbers are register in the case of 
# bathrooms == 7, 8, 20
subset_bat <- df_month %>%       # closer look on this subset
  dplyr::select(id, guests_included, accommodates, bathrooms, bedrooms, beds,
                price, property_type, room_type) %>%
  dplyr::filter(bathrooms >= 7)

# As before, I assume that a private room that includes 2 people cannot have 20 
# bathrooms. The solutions are as before:
df_month <- mutate(df_month,
                   bathrooms = case_when(
                     (room_type == 'Private room') ~ 1, 
                     (room_type == 'Shared room')  ~ 1,
                     #(room_type %in% c('Entire home/apt', 'Hotel room') &
                     #bathrooms > accommodates) ~ as.numeric(accommodates),
                     TRUE ~ as.numeric(bathrooms)))
df_month %>%
  dplyr::group_by(bathrooms) %>%
  dplyr::summarise(guest_included = mean(guests_included),
                   bed_included   = mean(beds))
subset_prova1bat <- df_month[df_month$bathrooms >= 6,] # they are all entire apt: ok

df_month %>%
  dplyr::select(guests_included, beds, bathrooms, bedrooms) %>%
  dplyr::group_by(guests_included) %>%
  dplyr::summarise(bathrooms = mean(bathrooms),
                   bedrooms  = mean(bedrooms),
                   beds      = mean(beds))


df_month    <- df_month[, -c(2:12,21,30,32,33,36,55,56,62,63)]
df_month    <- df_month %>%
  mutate(review_scores_rating        = review_scores_rating * number_of_reviews,
         review_scores_accuracy      = review_scores_accuracy * number_of_reviews,
         review_scores_checkin       = review_scores_checkin * number_of_reviews,
         review_scores_cleanliness   = review_scores_cleanliness * number_of_reviews,
         review_scores_communication = review_scores_communication * number_of_reviews,
         review_scores_location      = review_scores_location * number_of_reviews,
         review_scores_value         = review_scores_value * number_of_reviews
  )

df_month <- subset(df_month, select = -c(number_of_reviews))
df_month %>%
  dplyr::count(neighbourhood) %>%
  dplyr::arrange(neighbourhood)

df_month$neighbourhood <- ifelse(df_month$neighbourhood   == 'Bovisasca' |
                                   df_month$neighbourhood == 'Bruzzano' |
                                   df_month$neighbourhood == 'Comasina',
                                 'Zona 9', 
                                 as.character(df_month$neighbourhood))

df_month$neighbourhood <- ifelse(df_month$neighbourhood == 'Baggio' ,
                                 'Zona 7', 
                                 as.character(df_month$neighbourhood))

df_month$neighbourhood <- as.factor(df_month$neighbourhood)
property_type <- df_month %>%
  dplyr::group_by(property_type) %>%
  dplyr::summarise(room = unique(room_type))

df_month <- df_month[! df_month$property_type %in% c('Trullo (Italy)', 'Island') ,]
df_month[df_month$property_type %in% c('Castle') ,]
df_month$property_type <- plyr::mapvalues(df_month$property_type,
                                         from = c('Apartment',
                                                  'Condominium',
                                                  'Boutique hotel',
                                                  'Loft',
                                                  'Bed and breakfast',
                                                  'House',
                                                  'Serviced apartment',
                                                  'Villa' ,
                                                  'Other',
                                                  'Townhouse',
                                                  'Guesthouse',
                                                  'Guest suite',
                                                  'Hostel',
                                                  'Hotel',
                                                  'Bungalow',
                                                  'Aparthotel',
                                                  'Tiny house',
                                                  'Camper/RV',
                                                  'Dome house',
                                                  'Chalet',
                                                  'Farm stay',
                                                  'Nature lodge',
                                                  'Casa particular (Cuba)',
                                                  'Vacation home',
                                                  'Cave',
                                                  'Castle',
                                                  'Tent',
                                                  'Earth house'),
                                         to   = c('Apartment',
                                                  'Condominium',
                                                  'Boutique hotel',
                                                  'Loft',
                                                  'Bed and breakfast',
                                                  'House',
                                                  'Serviced apartment',
                                                  'Villa' ,
                                                  'Other',
                                                  'Townhouse',
                                                  'Guesthouse',
                                                  'Guest suite',
                                                  'Hostel',
                                                  'Hotel',
                                                  'Other',
                                                  'Aparthotel',
                                                  'Tiny house',
                                                  'Other',
                                                  'Other',
                                                  'House',
                                                  'Other',
                                                  'Other',
                                                  'House',
                                                  'House',
                                                  'Other',
                                                  'House', 
                                                  'Other',
                                                  'Other'))
df_month$property_type <- as.factor(df_month$property_type)

df_month <- mutate(df_month,
                   room_type = case_when(
                     (property_type == 'Bed and breakfast' &
                        room_type == 'Entire home/apt') ~ 'Private room',
                     TRUE ~ as.character(room_type)))

# law in Italy say that B&B are rooms, not entire apartment

df_month$room_type <- factor(df_month$room_type,
                             levels=c("Shared room",
                                      "Private room",
                                      "Entire home/apt",
                                      "Hotel room"))
df_month %>%
  dplyr::select_if(is.character) %>%
  head()

col    <- apply(df_month, 2, function(a) length(unique(a))==1)
names(col[col == TRUE])
# has_availability
# host_identity_verified
# is_business_travel_ready
df_month <- subset(df_month, select = -c(host_identity_verified,
                                         has_availability,
                                         is_business_travel_ready))

