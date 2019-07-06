# To import the CSV file into a dataframe
NI_Postcode_Data <- read.csv("NIPostcodes.csv", header = F)
# To show the number of rows 
nrow(NI_Postcode_Data)
# To show the structure of the dataset
str(NI_Postcode_Data)
# To show the first 10 rows of the dataset
head(NI_Postcode_Data, 10)

# To add the titles for each columns of data
colnames(NI_Postcode_Data) <- c ("Organization Name", "Sub-building Name", "Building Name", "Number", "Primary Thorfare", "Alt Thorfare", "Secondary Thorfare
", "Locality", "Townland", "Town", "County", "Postcode", "x-coordinates", "y-coordinates", "Primary Key")

# To record the null values with NA

NI_Postcode_Data[NI_Postcode_Data == ""] <- NA

# To find the sum and mean of missing values for each attribute

sapply(NI_Postcode_Data, function(missing_data) sum(is.na(missing_data)))
sapply(NI_Postcode_Data, function(mean_missing_values) mean(is.na(mean_missing_values)))

# To modify County to categorising factor

NI_Postcode_Data$County <- factor(NI_Postcode_Data$County, levels = c("Antrim", "Armagh", "Down", "Fermanagh", "Londonderry", "Tyrone"))

# To check if County is a factor or Not

str(NI_Postcode_Data)
levels(NI_Postcode_Data$County)

# To move the primary key to the begining of the dataset

NI_Postcode_Data <- NI_Postcode_Data[ , c(15, 1:14)]
head(NI_Postcode_Data, 5)

# To create a new dataset as Limavady_data

Limavady_data <- NI_Postcode_Data[NI_Postcode_Data$Town == "Limavady",]

head(Limavady_data, 5)

write.csv(NI_Postcode_Data, "C:\\Users\\Shubhanshu Sharma\\Documents\\Limavady.csv")

# To write dataset into the CSV

write.csv(NI_Postcode_Data, "C:\\Users\\Shubhanshu Sharma\\Documents\\CA2-Shubhanshu\\CleanNIPostcodedata.csv")

# To choose files in working directory

location_path <- choose.files(default = "", caption = "Select files", multi = TRUE, filters = Filters, index = nrow(Filters))
location_path

# Converting the csv files into a single dataframe with the use of base R funtions

AllNICrimeData = do.call(rbind, lapply(location_path, function(crime_data) read.csv(crime_data, stringsAsFactors = FALSE)))
AllNICrimeData

# To save dataset into csv file with the name AllNICrimeData

write.csv(AllNICrimeData, "C:\\Users\\Shubhanshu Sharma\\Documents\\CA2-Shubhanshu\\AllNICrimeData.csv", )

# To show the number of rows in AllNICrimeData

AllNICrimeData = read.csv("C:\\Users\\Shubhanshu Sharma\\Documents\\CA2-Shubhanshu\\AllNICrimeData.csv", header = TRUE )

nrow(AllNICrimeData)

str(AllNICrimeData)

# To remove unwanted columns

AllNICrimeData[, c("Crime.ID", "Reported.by", "Falls.within", "LSOA.code", "LSOA.name", 
                   "Last.outcome.category", "context")] <- list(NULL)

AllNICrimeData

str(AllNICrimeData)

# To factorize Crime type

AllNICrimeData$Crime.type <- factor(AllNICrimeData$Crime.type)
str(AllNICrimeData)

# To remove "On or near" from the locations

AllNICrimeData$Location <- gsub( "On or near ", "", AllNICrimeData$Location)
AllNICrimeData$Location <- factor(AllNICrimeData$Location)
AllNICrimeData$Location[AllNICrimeData$Location == ""] <- NA

sum(is.na(AllNICrimeData$Location))

# Creating the subset of AllNICrimedata with locations not containing NA

new_data <- subset(AllNICrimeData, !is.na(Location), select = c(Month, Longitude, Latitude, Location, Crime.type))
new_data

sum(is.na(new_data$Location))

random_crime_sample <-new_data[sample(1:nrow(new_data), 1000, replace = FALSE),]
random_crime_sample


library(dplyr)

cleanNIPostcode <- read.csv("C:\\Users\\Shubhanshu Sharma\\Documents\\CA2-Shubhanshu\\CleanNIPostcodedata.csv")
cleanNIPostcode

# To compare change both Primary_thorfare and Location to upper case

random_crime_sample$Location <- toupper(random_crime_sample$Location)
head(random_crime_sample, 2)
head(cleanNIPostcode, 2)

new_postcode <- cleanNIPostcode[, c(7, 14)]
head(new_postcode, 10)
sum(is.na(new_postcode$Postcode))

library(plyr)
new_postcode <- ddply(new_postcode, .(Primary.Thorfare, Postcode), nrow)
new_postcode
str(new_postcode)

colnames(new_postcode) <- c("Primary.Thorfare", "Postcode", "Number_of_Postcodes")

# To remove duplicates in the new_postcode data

new_postcode <- new_postcode[!duplicated(new_postcode$Primary.Thorfare), ]

sum(is.na(new_postcode$Postcode))

# To create a new column and call it as Postcode in random_crime_sample

random_crime_sample$Postcode <- NA
head(random_crime_sample, 5)

# To add values to Postcode

random_crime_sample$Postcode <- new_postcode$Postcode[match(random_crime_sample$Location, new_postcode$Primary.Thorfare)]
str(random_crime_sample)
nrow(random_crime_sample)

write.csv(random_crime_sample, "C:\\Users\\Shubhanshu Sharma\\Documents\\CA2-Shubhanshu\\random_crime_sample.csv")

updated_random_sample <- random_crime_sample
chart_data <- updated_random_sample

# To sort data with Postcode and Crime type

chart_data[order(chart_data$Postcode == "BT1", chart_data$Crime.type), ]
chart_data

# To create a dataset of new chart that contains postcode = 'BT1'

new_chart <- filter(chart_data, grepl('BT1', Postcode))
new_chart
new_chart[order(new_chart$Postcode == 'BT1', new_chart$Crime.type),  ]
str(new_chart)

# Summary of crime type with respect to Postcode and location

crime_type <- data_frame(new_chart$Crime.type)
crime_type <- ddply(crime_type, .(new_chart$Crime.type), nrow)
colnames(crime_type) <- c('Crime_type', 'Count')
crime_type

# Creating the barplot of crime type in chart_data

crime_data <- table(chart_data$Crime.type) 
barplot(crime_data, main = 'Frequency of crime type',
        xlab = 'Crime type',
        ylab = 'Frequency',
        col= "purple")
