---
title: "SundayMorning"
output: html_document
date: "2024-05-12"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


## Install libraries.
```{r}
library("data.table")
library("plyr")
library("dplyr")
library("stringr")
library("zoo")
library(BGLR)
library("WeightIt")
```


## Reading lambing record data from 2012-2023, lamb card data, and sheep tsu data.
```{r}
# E23_lamb_reports <- read.csv("C:\\Users\\anshe\\OneDrive - University of Nevada, Reno\\Desktop\\GWAS S24\\Rafter 7 Lambing Reports 2023.csv")
# E22_lamb_reports <- read.csv("C:\\Users\\anshe\\OneDrive - University of Nevada, Reno\\Desktop\\GWAS S24\\R7 Lambing Reports 2022.csv")
# E21_lamb_reports <- read.csv("C:\\Users\\anshe\\OneDrive - University of Nevada, Reno\\Desktop\\GWAS S24\\R7 Lambing Reports 2021.csv")
# E20_lamb_reports <- read.csv("C:\\Users\\anshe\\OneDrive - University of Nevada, Reno\\Desktop\\GWAS S24\\Rafter 7 Lambing Reports 2020.csv")
# E12_19_lamb_reports <- read.csv("C:\\Users\\anshe\\OneDrive - University of Nevada, Reno\\Desktop\\GWAS S24\\R7 Lamb Reports 2012-2019.csv")
# lamb_card <- read.csv("C:\\Users\\anshe\\OneDrive - University of Nevada, Reno\\Desktop\\GWAS S24\\Lamb Card Data 2023.csv")
# sheep_tsu <- read.csv("C:\\Users\\anshe\\OneDrive - University of Nevada, Reno\\Desktop\\GWAS S24\\Sheep_TSU_Master_Sheet_V5.csv")

E23_lamb_reports <- read.csv("Files/Rafter 7 Lambing Reports 2023.csv")
E22_lamb_reports <- read.csv("Files/R7 Lambing Reports 2022.csv")
E21_lamb_reports <- read.csv("Files/R7 Lambing Reports 2021.csv")
E20_lamb_reports <- read.csv("Files/Rafter 7 Lambing Reports 2020.csv")
E12_19_lamb_reports <- read.csv("Files/R7 Lamb Reports 2012-2019.csv")
lamb_card <- read.csv("Files/Lamb Card Data 2023.csv")
sheep_tsu <- read.csv("Files/Sheep_TSU_Master_Sheet_V5.csv")
```


## Filtering lamb_and_tsu for paint brand only, sample scanned only, and both.
```{r}
lamb_and_tsu <- merge(lamb_card, sheep_tsu, by.x = "TSU", by.y = "TSU..", all = TRUE)

paint_brand_filter <- filter(lamb_and_tsu, !is.na(lamb_and_tsu$Paint.Brand) & is.na(lamb_and_tsu$Sample.Scanned.by.GNZ.LAB))
sample_scanned_filter <- filter(lamb_and_tsu, is.na(lamb_and_tsu$Paint.Brand) & !is.na(lamb_and_tsu$Sample.Scanned.by.GNZ.LAB))
complete_filter <- filter(lamb_and_tsu, !is.na(lamb_and_tsu$Paint.Brand) & !is.na(lamb_and_tsu$Sample.Scanned.by.GNZ.LAB))
```


## Splitting up E12_19_lamb_reports into individual years (2012,2013,2014,2015,2016,2017,2018,2019).
```{r}
# Function to format the year from YY to YYYY.
add_2000 <- function(x){
    strtoi(x) + 2000   
}

# Getting each unique year from the E12_19_lamb_report.
dates <- as.Date(E12_19_lamb_reports$Lambing.Dates, origin = "1900-01-01")
strYY <- substring(dates, 1, 2)
intYY <- lapply(strYY, strtoi)
E12_19_lamb_reports$Years <- lapply(intYY, add_2000)
unique_years <- unique(E12_19_lamb_reports$Years)

# Going through the E12_19_lamb_report and splitting up the dataframe into each individual year.
list_of_years <- list()
for(year in unique_years){
    year_df <- E12_19_lamb_reports[E12_19_lamb_reports$Years == year, ]
    list_of_years[[as.character(year)]] <- year_df
}

for(i in 1:9){
    name <- paste("df", unique_years[[i]], sep="_")
    assign(name, as.data.frame(list_of_years[i]))
}

# Removing duplicated rows.
df_2012 <- unique(df_2012)
df_2013 <- unique(df_2013)
df_2014 <- unique(df_2014)
df_2015 <- unique(df_2015)
df_2016 <- unique(df_2016)
df_2017 <- unique(df_2017)
df_2018 <- unique(df_2018)
df_2019 <- unique(df_2019)

```


## Determine single, twin, triplet status for 2012-2019
```{r}
# A for loop that goes through each year, from 2012-2019, and associates a specific ewe with the amount of lambs that she has birthed that year.

# Unique EIDs
lamb_card_eids <- unique(lamb_card$Ewe.ID)
for(i in 1:9){
    # Creating names of intermediary data frames.
    name <- paste("lamb_report_eids", unique_years[[i]], sep="_")
    similar_name <- paste("similar", unique_years[[i]], sep="_")
    
    # Getting data frames for years 2012-2019.
    df <- get(paste("df", unique_years[[i]], sep="_"))
    
    # Specifying the column names to aggregate.
    column_lamb <- paste("X", unique_years[[i]], ".Lamb.Ear.Tag..", sep="")
    column_ewe <-paste("X", unique_years[[i]], ".Ewe.Ear.Tag..", sep="")

    # Creating the name of the final data frames.
    progeny_name <- paste("progeny", unique_years[[i]], sep="_")
    
    # Assigning final data frames to their name and associated data.
    assign(name, unique(df[[column_lamb]]))
    assign(similar_name, lamb_card_eids[name %in% lamb_card_eids])
    if(column_lamb != "XNA.Lamb.Ear.Tag.."){
        assign(progeny_name, aggregate(df[[column_lamb]]~df[[column_ewe]], df, FUN = length))   
    }

}
```


## Get Ewe Ear Tags From Ewes With More Than One Progeny
```{r}
# Identifies only the ewes that have had more than one lamb birthed. Also removes any unwanted data from our data frame.
ewe_2012 <- progeny_2012[which(progeny_2012$`df[[column_lamb]]` > 1 & progeny_2012$`df[[column_ewe]]` != "" & progeny_2012$`df[[column_ewe]]` != "?"),]
ewe_2013 <- progeny_2013[which(progeny_2013$`df[[column_lamb]]` > 1 & progeny_2013$`df[[column_ewe]]` != "" & progeny_2013$`df[[column_ewe]]` != "?"),]
ewe_2014 <- progeny_2014[which(progeny_2014$`df[[column_lamb]]` > 1 & progeny_2014$`df[[column_ewe]]` != "" & progeny_2014$`df[[column_ewe]]` != "?"),]
ewe_2015 <- progeny_2015[which(progeny_2015$`df[[column_lamb]]` > 1 & progeny_2015$`df[[column_ewe]]` != "" & progeny_2015$`df[[column_ewe]]` != "?"),]
ewe_2016 <- progeny_2016[which(progeny_2016$`df[[column_lamb]]` > 1 & progeny_2016$`df[[column_ewe]]` != "" & progeny_2016$`df[[column_ewe]]` != "?"),]
ewe_2017 <- progeny_2017[which(progeny_2017$`df[[column_lamb]]` > 1 & progeny_2017$`df[[column_ewe]]` != "" & progeny_2017$`df[[column_ewe]]` != "?"),]
ewe_2018 <- progeny_2018[which(progeny_2018$`df[[column_lamb]]` > 1 & progeny_2018$`df[[column_ewe]]` != "" & progeny_2018$`df[[column_ewe]]` != "?"),]
ewe_2019 <- progeny_2019[which(progeny_2019$`df[[column_lamb]]` > 1 & progeny_2019$`df[[column_ewe]]` != "" & progeny_2019$`df[[column_ewe]]` != "?"),]

# Identifies which ewes from the entirety of 2012 are in the progeny data frame.
found_2012 <- df_2012[which(df_2012$X2012.Ewe.Ear.Tag.. %in% ewe_2012$`df[[column_ewe]]`),]

# Formatting of the data.
found_2012$X2012.Ewe.Ear.Tag.. <- as.character(found_2012$X2012.Ewe.Ear.Tag..)
found_2012$X2012.Lambing.Dates <- as.Date(found_2012$X2012.Lambing.Dates)
found_2012$X2012.Ewe.Ear.Tag.. <- trimws(found_2012$X2012.Ewe.Ear.Tag..)

# Splits the found data frame up by each ewe.
found_2012_split <- split(found_2012, found_2012$X2012.Ewe.Ear.Tag..)

# Checks whether or not the birthdays of lambs from the same ewe are the same.
same_birthday_check_2012 <- lapply(test, function(found_2012) {
     all(found_2012$X2012.Lambing.Dates == found_2012$X2012.Lambing.Dates[1])
})

# Converts our birthday data into a data frame.
same_birthday_df_2012 <- data.frame(
     Ewe_Ear_Tag = names(same_birthday_check_2012),
     Same_Birthday = unlist(same_birthday_check_2012)
)

# We merge this birthday data onto the data for all of 2012. Then we select only the rows where lambs born from the same lamb did not have the same birthdays.
df_2012_birthday <- merge(df_2012, same_birthday_df_2012, by.x = 'X2012.Ewe.Ear.Tag..', by.y = 'Ewe_Ear_Tag', all = TRUE)
lambs_different_birthdays_2012 <- df_2012_birthday[which(df_2012_birthday$Same_Birthday == FALSE),]
lambs_different_birthdays_2012 <- lambs_different_birthdays_2012[, c(1,2,4,7,13,14)]

# The steps above are repeated for 2013-2019.
found_2013 <- df_2013[which(df_2013$X2013.Ewe.Ear.Tag.. %in% ewe_2013$`df[[column_ewe]]`),]
found_2013$X2013.Ewe.Ear.Tag.. <- as.character(found_2013$X2013.Ewe.Ear.Tag..)
found_2013$X2013.Lambing.Dates <- as.Date(found_2013$X2013.Lambing.Dates)
found_2013$X2013.Ewe.Ear.Tag.. <- trimws(found_2013$X2013.Ewe.Ear.Tag..)

found_2013_split <- split(found_2013, found_2013$X2013.Ewe.Ear.Tag..)

same_birthday_check_2013 <- lapply(test, function(found_2013) {
     all(found_2013$X2013.Lambing.Dates == found_2013$X2013.Lambing.Dates[1])
})

same_birthday_df_2013 <- data.frame(
     Ewe_Ear_Tag = names(same_birthday_check_2013),
     Same_Birthday = unlist(same_birthday_check_2013)
)

df_2013_birthday <- merge(df_2013, same_birthday_df_2013, by.x = 'X2013.Ewe.Ear.Tag..', by.y = 'Ewe_Ear_Tag', all = TRUE)
lambs_different_birthdays_2013 <- df_2013_birthday[which(df_2013_birthday$Same_Birthday == FALSE),]
lambs_different_birthdays_2013 <- lambs_different_birthdays_2013[, c(1,2,4,7,13,14)]

found_2014 <- df_2014[which(df_2014$X2014.Ewe.Ear.Tag.. %in% ewe_2014$`df[[column_ewe]]`),]
found_2014$X2014.Ewe.Ear.Tag.. <- as.character(found_2014$X2014.Ewe.Ear.Tag..)
found_2014$X2014.Lambing.Dates <- as.Date(found_2014$X2014.Lambing.Dates)
found_2014$X2014.Ewe.Ear.Tag.. <- trimws(found_2014$X2014.Ewe.Ear.Tag..)

found_2014_split <- split(found_2014, found_2014$X2014.Ewe.Ear.Tag..)

same_birthday_check_2014 <- lapply(test, function(found_2014) {
     all(found_2014$X2014.Lambing.Dates == found_2014$X2014.Lambing.Dates[1])
})

same_birthday_df_2014 <- data.frame(
     Ewe_Ear_Tag = names(same_birthday_check_2014),
     Same_Birthday = unlist(same_birthday_check_2014)
)

df_2014_birthday <- merge(df_2014, same_birthday_df_2014, by.x = 'X2014.Ewe.Ear.Tag..', by.y = 'Ewe_Ear_Tag', all = TRUE)
lambs_different_birthdays_2014 <- df_2014_birthday[which(df_2014_birthday$Same_Birthday == FALSE),]
lambs_different_birthdays_2014 <- lambs_different_birthdays_2014[, c(1,2,4,7,13,14)]

found_2015 <- df_2015[which(df_2015$X2015.Ewe.Ear.Tag.. %in% ewe_2015$`df[[column_ewe]]`),]
found_2015$X2015.Ewe.Ear.Tag.. <- as.character(found_2015$X2015.Ewe.Ear.Tag..)
found_2015$X2015.Lambing.Dates <- as.Date(found_2015$X2015.Lambing.Dates)
found_2015$X2015.Ewe.Ear.Tag.. <- trimws(found_2015$X2015.Ewe.Ear.Tag..)

found_2015_split <- split(found_2015, found_2015$X2015.Ewe.Ear.Tag..)

same_birthday_check_2015 <- lapply(test, function(found_2015) {
     all(found_2015$X2015.Lambing.Dates == found_2015$X2015.Lambing.Dates[1])
})

same_birthday_df_2015 <- data.frame(
     Ewe_Ear_Tag = names(same_birthday_check_2015),
     Same_Birthday = unlist(same_birthday_check_2015)
)

df_2015_birthday <- merge(df_2015, same_birthday_df_2015, by.x = 'X2015.Ewe.Ear.Tag..', by.y = 'Ewe_Ear_Tag', all = TRUE)
lambs_different_birthdays_2015 <- df_2015_birthday[which(df_2015_birthday$Same_Birthday == FALSE),]
lambs_different_birthdays_2015 <- lambs_different_birthdays_2015[, c(1,2,4,7,13,14)]

found_2016 <- df_2016[which(df_2016$X2016.Ewe.Ear.Tag.. %in% ewe_2016$`df[[column_ewe]]`),]
found_2016$X2016.Ewe.Ear.Tag.. <- as.character(found_2016$X2016.Ewe.Ear.Tag..)
found_2016$X2016.Lambing.Dates <- as.Date(found_2016$X2016.Lambing.Dates)
found_2016$X2016.Ewe.Ear.Tag.. <- trimws(found_2016$X2016.Ewe.Ear.Tag..)

found_2016_split <- split(found_2016, found_2016$X2016.Ewe.Ear.Tag..)

same_birthday_check_2016 <- lapply(test, function(found_2016) {
     all(found_2016$X2016.Lambing.Dates == found_2016$X2016.Lambing.Dates[1])
})

same_birthday_df_2016 <- data.frame(
     Ewe_Ear_Tag = names(same_birthday_check_2016),
     Same_Birthday = unlist(same_birthday_check_2016)
)

df_2016_birthday <- merge(df_2016, same_birthday_df_2016, by.x = 'X2016.Ewe.Ear.Tag..', by.y = 'Ewe_Ear_Tag', all = TRUE)
lambs_different_birthdays_2016 <- df_2016_birthday[which(df_2016_birthday$Same_Birthday == FALSE),]
lambs_different_birthdays_2016 <- lambs_different_birthdays_2016[, c(1,2,4,7,13,14)]

found_2017 <- df_2017[which(df_2017$X2017.Ewe.Ear.Tag.. %in% ewe_2017$`df[[column_ewe]]`),]
found_2017$X2017.Ewe.Ear.Tag.. <- as.character(found_2017$X2017.Ewe.Ear.Tag..)
found_2017$X2017.Lambing.Dates <- as.Date(found_2017$X2017.Lambing.Dates)
found_2017$X2017.Ewe.Ear.Tag.. <- trimws(found_2017$X2017.Ewe.Ear.Tag..)

found_2017_split <- split(found_2017, found_2017$X2017.Ewe.Ear.Tag..)

same_birthday_check_2017 <- lapply(test, function(found_2017) {
     all(found_2017$X2017.Lambing.Dates == found_2017$X2017.Lambing.Dates[1])
})

same_birthday_df_2017 <- data.frame(
     Ewe_Ear_Tag = names(same_birthday_check_2017),
     Same_Birthday = unlist(same_birthday_check_2017)
)

df_2017_birthday <- merge(df_2017, same_birthday_df_2017, by.x = 'X2017.Ewe.Ear.Tag..', by.y = 'Ewe_Ear_Tag', all = TRUE)
lambs_different_birthdays_2017 <- df_2017_birthday[which(df_2017_birthday$Same_Birthday == FALSE),]
lambs_different_birthdays_2017 <- lambs_different_birthdays_2017[, c(1,2,4,7,13,14)]

found_2018 <- df_2018[which(df_2018$X2018.Ewe.Ear.Tag.. %in% ewe_2018$`df[[column_ewe]]`),]
found_2018$X2018.Ewe.Ear.Tag.. <- as.character(found_2018$X2018.Ewe.Ear.Tag..)
found_2018$X2018.Lambing.Dates <- as.Date(found_2018$X2018.Lambing.Dates)
found_2018$X2018.Ewe.Ear.Tag.. <- trimws(found_2018$X2018.Ewe.Ear.Tag..)

found_2018_split <- split(found_2018, found_2018$X2018.Ewe.Ear.Tag..)

same_birthday_check_2018 <- lapply(test, function(found_2018) {
     all(found_2018$X2018.Lambing.Dates == found_2018$X2018.Lambing.Dates[1])
})

same_birthday_df_2018 <- data.frame(
     Ewe_Ear_Tag = names(same_birthday_check_2018),
     Same_Birthday = unlist(same_birthday_check_2018)
)

df_2018_birthday <- merge(df_2018, same_birthday_df_2018, by.x = 'X2018.Ewe.Ear.Tag..', by.y = 'Ewe_Ear_Tag', all = TRUE)
lambs_different_birthdays_2018 <- df_2018_birthday[which(df_2018_birthday$Same_Birthday == FALSE),]
lambs_different_birthdays_2018 <- lambs_different_birthdays_2018[, c(1,2,4,7,13,14)]

found_2019 <- df_2019[which(df_2019$X2019.Ewe.Ear.Tag.. %in% ewe_2019$`df[[column_ewe]]`),]
found_2019$X2019.Ewe.Ear.Tag.. <- as.character(found_2019$X2019.Ewe.Ear.Tag..)
found_2019$X2019.Lambing.Dates <- as.Date(found_2019$X2019.Lambing.Dates)
found_2019$X2019.Ewe.Ear.Tag.. <- trimws(found_2019$X2019.Ewe.Ear.Tag..)

found_2019_split <- split(found_2019, found_2019$X2019.Ewe.Ear.Tag..)

same_birthday_check_2019 <- lapply(test, function(found_2019) {
     all(found_2019$X2019.Lambing.Dates == found_2019$X2019.Lambing.Dates[1])
})

same_birthday_df_2019 <- data.frame(
     Ewe_Ear_Tag = names(same_birthday_check_2019),
     Same_Birthday = unlist(same_birthday_check_2019)
)

df_2019_birthday <- merge(df_2019, same_birthday_df_2019, by.x = 'X2019.Ewe.Ear.Tag..', by.y = 'Ewe_Ear_Tag', all = TRUE)
lambs_different_birthdays_2019 <- df_2019_birthday[which(df_2019_birthday$Same_Birthday == FALSE),]
lambs_different_birthdays_2019 <- lambs_different_birthdays_2019[, c(1,2,4,7,13,14)]
```

## Determine single, twin, triplet status for 2020-2023
```{r}
# Unique EIDs
lamb_card_eids <- unique(lamb_card$Ewe.ID)

# Goes through the years 2020-2023 and associates each unique EID with the amount of lambs that that ewe birthed that year.
lamb_report_eids_2020 <- unique(E20_lamb_reports$Lamb.Ear.Tag..)
similar_2020 <- lamb_card_eids[lamb_report_eids_2020 %in% lamb_card_eids]
progeny_2020 <- aggregate(E20_lamb_reports$Lamb.Ear.Tag..~E20_lamb_reports$Ewe.Ear.Tag.., E20_lamb_reports, FUN = length)

# Filling in incomplete lamb ear tag data from another column.
E21_lamb_reports$Lamb.Ear.tag[1001:1941] <- E21_lamb_reports$Lamb.eletronic.ear.tag[1001:1941] 

lamb_report_eids_2021 <- unique(E21_lamb_reports$Lamb.Ear.tag)
similar_2021 <- lamb_card_eids[lamb_report_eids_2021 %in% lamb_card_eids]
progeny_2021 <- aggregate(E21_lamb_reports$Lamb.Ear.tag~E21_lamb_reports$Dam.ear.tag, E21_lamb_reports, FUN = length)

lamb_report_eids_2022 <- unique(E22_lamb_reports$Lamb.Ear.tag)
similar_2022 <- lamb_card_eids[lamb_report_eids_2022 %in% lamb_card_eids]
progeny_2022 <- aggregate(E22_lamb_reports$Lamb.Ear.tag~E22_lamb_reports$Dam.ear.tag, E22_lamb_reports, FUN = length)

lamb_report_eids_2023 <- unique(E23_lamb_reports$Ewe..Ear.tag...)
similar_2023 <- lamb_card_eids[lamb_report_eids_2023 %in% lamb_card_eids]
progeny_2023 <- aggregate(E23_lamb_reports$Lamb.Ear.Tag..~E23_lamb_reports$Ewe..Ear.tag..., E23_lamb_reports, FUN = length)
```


## Merge progeny with associated lambing report and then add on the age of the ewe
```{r}
# Taking our original lamb reports and adding in the progeny data associated with that year. Resulting data frame is full_report_[2020-2023].
E23_lamb_reports <- unique(E23_lamb_reports)
full_report_2023 <- merge(E23_lamb_reports, progeny_2023, by.x = "Ewe..Ear.tag...", by.y = "E23_lamb_reports$Ewe..Ear.tag...", all = TRUE)
full_report_2023 <- unique(full_report_2023)

full_report_2022 <- merge(E22_lamb_reports, progeny_2022, by.x = "Dam.ear.tag", by.y = "E22_lamb_reports$Dam.ear.tag")
full_report_2021 <-merge(E21_lamb_reports, progeny_2021, by.x = "Dam.ear.tag", by.y = "E21_lamb_reports$Dam.ear.tag")
full_report_2020 <- merge(E20_lamb_reports, progeny_2020, by.x = "Ewe.Ear.Tag..", by.y = "E20_lamb_reports$Ewe.Ear.Tag..")

# A for loop that takes our original lamb reports and adds the progeny data associated with that year. Resulting data frame is full_report_[2012-2019].
for(i in c(1, 3:9)){
    # Creates the name of the final year's data frame.
    name <- paste("full_report", unique_years[[i]], sep="_")
    
    # Gets the associated year's progeny data for the lamb reports.
    if(unique_years[[i]]){
     progeny <- get(paste("progeny", unique_years[[i]], sep="_"))
    }
    
    # Gets the associated year's lamb report data.
    df <- get(paste("df", unique_years[[i]], sep="_"))
    
    # Gets the column names to merge on..
    column_x = paste("X", unique_years[[i]], ".Ewe.Ear.Tag..", sep="")
    column_y <- "df[[column_ewe]]"
    
    # Makes the actual year's data frame including progeny data.
    assign(name, merge(df, progeny, by.x = column_x, by.y = column_y))
}

```


## Determine the age of the ewe 2012 - 2019
```{r}
# A list to contain our data: year of birth, genline number, identifier number, and the eid number.
year_data <- vector("list", 13)

# A for loop to get our year's data frame and create a place in year_data for that year's data which includes sub-lists of all the necessary data we need.
for(i in c(1, 3:9)){
    # Counter to increment through the sub-lists.
    counter <- 1
    
    # Gets the year's full_report data frame.
    report_name <- get(paste("full_report", unique_years[[i]], sep="_"))
    
    # Gets the column's name to loop over, which contains the EIDs.
    column <- paste("X", unique_years[[i]], ".Ewe.Ear.Tag..", sep="")
    
    # Creates a place in year_data for that year's data which includes sublists of all necessary data we need.
    year_data[[i]] <- list(
        yob = list(),
        genline = list(),
        identifier = list(),
        value = list()
    )
    
    # A for loop to go through the year's EID column and break it up into the parts that we want.
    for(j in 1:length(report_name[[column]])){
        
        # The actual EID.
        value <- report_name[[column]][[j]]
        
        # The length of the EID.
        clength <- nchar(value)
        
        # Filters out any EIDs that start with the letter A and aren't 5 characters long.
        if(clength == 5 && substring(value, 1, 1) != 'A'){
            
            # Splits up the EID into its components: year of birth, genline number, and identifier number
            yob_value = substring(value, 1, 1)
            genline_value = substring(value, 2, 2)
            identifier_value = substring(value, 3, 5)
            
            # Places these components into their corresponding sub-lists in year_data.
            year_data[[i]]$yob[[counter]] <- yob_value
            year_data[[i]]$genline[[counter]] <- genline_value
            year_data[[i]]$identifier[[counter]] <- identifier_value
            year_data[[i]]$eid[[counter]] <- value
            
            # Formats the birth year as YYYY
            birth_year <- as.numeric(yob_value) + 2010
            
            # Puts the year in as YY
            year_data[[i]]$year[[counter]] <- substring(birth_year, 3, 4)
            
            # Subtracts birth year by 10 if it is greater than the year that we are working with.
            if(birth_year > unique_years[[i]]){
                birth_year <- birth_year - 10
            }
            
            # Calculates age and puts it into year_data in the age sub-list.
            age <- unique_years[[i]] - birth_year
            year_data[[i]][["age"]][[counter]] <- age
            
            # Increment counter by 1 to move onto the next EID.
            counter <- counter + 1

            
        }
    }
}
```


## Merge 2012-2019 with age
```{r}
# A for loop to add the age data for each year onto the full_report data.
for(i in c(1, 3:9)){
    # Get's the eids from year_data.
    eid <- unlist(year_data[[i]]$eid)
    
    # Creates an intermediary data frame which contains the EIDs and their associated ages.
    name <- paste("merged", unique_years[[i]], "data", sep="_")
    assign(name, data.frame(age = unlist(year_data[[i]]$age),eid = eid))
    
    # Creates the name of the full_report and final data frame.
    report <- paste("full_report", unique_years[[i]], sep="_")
    data <- paste("E",unique_years[[i]], "data", sep="_")
    
    # Creates the name of the column to merge on.
    x <- paste("X", unique_years[[i]], ".Ewe.Ear.Tag..", sep="")
    
    # Creates the final data frame (E_[year]_data).
    assign(data, merge(get(report), get(name), by.x = x, by.y = "eid", all = TRUE))
}

# A for loop to remove any duplicated rows.
for(i in c(1, 3:9)){
    # Creates the name of the data frame we just created.
    data_name <- get(paste("E", unique_years[[i]], "data", sep="_"))
    
    # Creates the column which we are trying to remove duplicates from.
    y <- paste("X", unique_years[[i]], ".Ewe.Ear.Tag..", sep="")
    
    # Removes duplicated rows.
    distinct(data_name, get(y), .keep_all = TRUE)
}
```


## Cleaning up the E reports
```{r}
# Goes through the 2012-2019 data frames which includes lamb reports, progeny, and age, and removes any duplicated rows.
E_2012_data <- E_2012_data[-which(duplicated(E_2012_data)),]
E_2013_data <- E_2013_data[-which(duplicated(E_2013_data)),]
E_2014_data <- E_2014_data[-which(duplicated(E_2014_data)),]
E_2015_data <- E_2015_data[-which(duplicated(E_2015_data)),]
E_2016_data <- E_2016_data[-which(duplicated(E_2016_data)),]
E_2017_data <- E_2017_data[-which(duplicated(E_2017_data)),]
E_2018_data <- E_2018_data[-which(duplicated(E_2018_data)),]
E_2019_data <- E_2019_data[-which(duplicated(E_2019_data)),]
```


## Determine the age of the ewe 2023
```{r}
# A list to contain the animal's year of birth for animals in the progeny_2023 data frame.
yob_numeric_2023 <- list()

# Counter to increment through the sub-lists.
counter <- 1

# A for loop to go through the year's EID column and break it up into the parts that we want.
for(i in 1:length(progeny_2023$`E23_lamb_reports$Ewe..Ear.tag...`)){
    
    # The actual EID.
    value <- progeny_2023$`E23_lamb_reports$Ewe..Ear.tag...`[[i]]
    
    # The length of the EID.
    clength <- nchar(value)
    
    # The different lengths of EIDs mean that the components we want will be in different places. We filter by length to specify the location of each component.
    # Some EIDs that also start with 2 will also have different places for the components so we filter by those to also specify the location of each component.
    if(clength == 7){
        # Splits up the EID into its components: year of birth, genline number, and identifier number.
        yob_value <- substring(value, 1, 2)
        genline_value <- substring(value, 3, 3)
        identifier_value <- substring(value, 4, 7)
 
        # Places these components into their corresponding sub-lists in year_data.
        year_data[[10]]$yob[[counter]] <- yob_value
        year_data[[10]]$genline[[counter]] <- genline_value
        year_data[[10]]$identifier[[counter]] <- identifier_value
        year_data[[10]]$eid[[counter]] <- value
        
        # Calculates the age of the animal and places the value (YY) into the year_data list.
        age <- as.numeric(yob_value) + 2000
        year_data[[10]]$year[[counter]] <- substring(age, 3, 4)
        
        # Places the age we calculated into the list we made containing all the ages.
        yob_numeric_2023[[counter]] <- 2023 - age
        
        # Increment counter by 1 to move onto the next EID.
        counter <- counter + 1
    }
    
    # The steps in between each if-else statements will be repeated for all the EIDs that make it through the filters we have put in place.
    # Some EIDs that we want though, may not make it through the filters and would impact our final results.
    else if(clength == 6 && substring(value, 1, 1) == 2){
        yob_value <- substring(value, 1, 2)
        genline_value <- substring(value, 3, 3)
        identifier_value <- substring(value, 4, 6)

        year_data[[10]]$yob[[counter]] <- yob_value
        year_data[[10]]$genline[[counter]] <- genline_value
        year_data[[10]]$identifier[[counter]] <- identifier_value
        year_data[[10]]$eid[[counter]] <- value
        
        age <- as.numeric(yob_value) + 2000
        year_data[[10]]$year[[counter]] <- substring(age, 3, 4)
        
        yob_numeric_2023[[counter]] <- 2023 - age
 
        counter <- counter + 1
    }
    else if(clength == 6 && substring(value,1,1) != 2){
        yob_value <- substring(value, 1, 1)
        genline_value <- substring(value, 2,2)
        identifier_value <- substring(value, 3, 6)

        year_data[[10]]$yob[[counter]] <- yob_value
        year_data[[10]]$genline[[counter]] <- genline_value
        year_data[[10]]$identifier[[counter]] <- identifier_value
        year_data[[10]]$eid[[counter]] <- value
        
        age <- as.numeric(yob_value) + 2010
        year_data[[10]]$year[[counter]] <- substring(age, 3, 4)
        
        yob_numeric_2023[[counter]] <- 2023 - age

        counter <- counter + 1
    }
    else if(clength == 5){
        yob_value <- substring(value, 1, 1)
        genline_value <- substring(value, 2,2)
        identifier_value <- substring(value, 3, 5)

        year_data[[10]]$yob[[counter]] <- yob_value
        year_data[[10]]$genline[[counter]] <- genline_value
        year_data[[10]]$identifier[[counter]] <- identifier_value
        year_data[[10]]$eid[[counter]] <- value
        
        age <- as.numeric(yob_value) + 2010
        year_data[[10]]$year[[counter]] <- substring(age, 3, 4)
        
        yob_numeric_2023[[counter]] <- 2023 - age
        
        counter <- counter + 1
    }
}
```


## Merge full_report with age 2023
```{r}
# Creates an intermediary data frame that contains all EIDs that made it through and their associated ages.
eid_2023 <- unlist(year_data[[10]]$eid)
merged_2023_data <- data.frame(age = unlist(yob_numeric_2023), eid = eid_2023)

# Adds the age data onto the full_report data.
E_2023_data <- merge(full_report_2023, merged_2023_data, by.x = "Ewe..Ear.tag...", by.y = "eid", all = TRUE)

# Changes the name of the progeny data.
colnames(E_2023_data)[colnames(E_2023_data) == "E23_lamb_reports$Lamb.Ear.Tag.."] <- "# of Lambs Birthed"
```


## Determine the age of the ewe 2022
```{r}
# A list to contain the animal's year of birth for animals in the progeny_2022 data frame.
yob_numeric_2022 <- list()

# Counter to increment through the sub-lists.
counter <- 1

# A for loop to go through the year's EID column and break it up into the parts that we want.
for(i in 1:length(progeny_2022$`E22_lamb_reports$Dam.ear.tag`)){
    
    # The actual EID.
    value <- progeny_2022$`E22_lamb_reports$Dam.ear.tag`[[i]]
    
    # The length of the EID.
    clength <- nchar(value)
    
    # The different lengths of EIDs mean that the components we want will be in different places. We filter by length to specify the location of each component.
    # Some EIDs that also start with 2 will also have different places for the components so we filter by those to also specify the location of each component.
    if(clength == 7){
        # Splits up the EID into its components: year of birth, genline number, and identifier number.
        yob_value <- substring(value, 1, 2)
        genline_value <- substring(value, 3, 3)
        identifier_value <- substring(value, 4, 7)

        # Places these components into their corresponding sub-lists in year_data.
        year_data[[11]]$yob[[counter]] <- yob_value
        year_data[[11]]$genline[[counter]] <- genline_value
        year_data[[11]]$identifier[[counter]] <- identifier_value
        year_data[[11]]$eid[[counter]] <- value
        
        # Calculates the age of the animal and places the value (YY) into the year_data list.
        age <- as.numeric(yob_value) + 2000
        year_data[[11]]$year[[counter]] <- substring(age, 3, 4)
        
        # Places the age we calculated into the list we made containing all the ages.
        yob_numeric_2022[[counter]] <- 2022 - age

        # Increment counter by 1 to move onto the next EID.
        counter <- counter + 1
    }
    
    # The steps in between each if-else statements will be repeated for all the EIDs that make it through the filters we have put in place.
    # Some EIDs that we want though, may not make it through the filters and would impact our final results.
    else if(clength == 6 && substring(value, 1, 1) == 2){
        yob_value <- substring(value, 1, 2)
        genline_value <- substring(value, 3, 3)
        identifier_value <- substring(value, 4, 6)

        year_data[[11]]$yob[[counter]] <- yob_value
        year_data[[11]]$genline[[counter]] <- genline_value
        year_data[[11]]$identifier[[counter]] <- identifier_value
        year_data[[11]]$eid[[counter]] <- value
        
        age <- as.numeric(yob_value) + 2000
        year_data[[11]]$year[[counter]] <- substring(age, 3, 4)
        
        yob_numeric_2022[[counter]] <- 2022 - age

        counter <- counter + 1
    }
    else if(clength == 6 && substring(value,1,1) != 2){
        yob_value <- substring(value, 1, 1)
        genline_value <- substring(value, 2,2)
        identifier_value <- substring(value, 3, 6)
        
        year_data[[11]]$yob[[counter]] <- yob_value
        year_data[[11]]$genline[[counter]] <- genline_value
        year_data[[11]]$identifier[[counter]] <- identifier_value
        year_data[[11]]$eid[[counter]] <- value
        
        age <- as.numeric(yob_value) + 2010
        year_data[[11]]$year[[counter]] <- substring(age, 3, 4)
        
        yob_numeric_2022[[counter]] <- 2022 - age
        
        counter <- counter + 1
    }
    else if(clength == 5){
        yob_value <- substring(value, 1, 1)
        genline_value <- substring(value, 2,2)
        identifier_value <- substring(value, 3, 5)
        
        year_data[[11]]$yob[[counter]] <- yob_value
        year_data[[11]]$genline[[counter]] <- genline_value
        year_data[[11]]$identifier[[counter]] <- identifier_value
        year_data[[11]]$eid[[counter]] <- value
        
        age <- as.numeric(yob_value) + 2010
        year_data[[11]]$year[[counter]] <- substring(age, 3, 4)
        
        yob_numeric_2022[[counter]] <- 2022 - age
        
        counter <- counter + 1
    }
}
```


## Merge full_report with age 2022
```{r}
# Creates an intermediary data frame that contains all EIDs that made it through and their associated ages.
eid_2022 <- unlist(year_data[[11]]$eid)

# Adds the age data onto the full_report data.
merged_2022_data <- data.frame(age = unlist(yob_numeric_2022), eid = eid_2022)
E_2022_data <- merge(full_report_2022, merged_2022_data, by.x = "Dam.ear.tag", by.y = "eid", all = TRUE)

# Changes the name of the progeny data.
colnames(E_2022_data)[colnames(E_2022_data) == "E22_lamb_reports$Lamb.Ear.Tag.."] <- "# of Lambs Birthed"
```


## Determining the age of the ewe 2021
```{r}
# Removed unnecessary data.
progeny_2021 <- progeny_2021[-c(1415:1416), ]

# A list to contain the animal's year of birth for animals in the progeny_2021 data frame.
yob_numeric_2021 <- list()

# Counter to increment through the sub-lists.
counter <- 1

# A for loop to go through the year's EID column and break it up into the parts that we want.
for(i in 1:length(progeny_2021$`E21_lamb_reports$Dam.ear.tag`)){
    
    # The actual EID.
    value <- progeny_2021$`E21_lamb_reports$Dam.ear.tag`[[i]]
    
    # The length of the EID.
    clength <- nchar(value)
    
    # The different lengths of EIDs mean that the components we want will be in different places. We filter by length to specify the location of each component.
    # Some EIDs that also start with 2 will also have different places for the components so we filter by those to also specify the location of each component.
    if(clength == 7){
        
        # Splits up the EID into its components: year of birth, genline number, and identifier number.
        yob_value <- substring(value, 1, 2)
        genline_value <- substring(value, 3, 3)
        identifier_value <- substring(value, 4, 7)
   
        # Places these components into their corresponding sub-lists in year_data.
        year_data[[12]]$yob[[counter]] <- yob_value
        year_data[[12]]$genline[[counter]] <- genline_value
        year_data[[12]]$identifier[[counter]] <- identifier_value
        year_data[[12]]$eid[[counter]] <- value
        
        # Calculates the age of the animal and places the value (YY) into the year_data list.
        age <- as.numeric(yob_value) + 2000
        year_data[[12]]$year[[counter]] <- substring(age, 3, 4)
        
        # Places the age we calculated into the list we made containing all the ages.
        yob_numeric_2021[[counter]] <- 2021 - age
   
        # Increment counter by 1 to move onto the next EID.
        counter <- counter + 1
    }
    
    # The steps in between each if-else statements will be repeated for all the EIDs that make it through the filters we have put in place.
    # Some EIDs that we want though, may not make it through the filters and would impact our final results.
    else if(clength == 6 && substring(value, 1, 1) == 2){
        yob_value <- substring(value, 1, 2)
        genline_value <- substring(value, 3, 3)
        identifier_value <- substring(value, 4, 6)
    
        year_data[[12]]$yob[[counter]] <- yob_value
        year_data[[12]]$genline[[counter]] <- genline_value
        year_data[[12]]$identifier[[counter]] <- identifier_value
        year_data[[12]]$eid[[counter]] <- value
        
        age <- as.numeric(yob_value) + 2000
        year_data[[12]]$year[[counter]] <- substring(age, 3, 4)
        
        yob_numeric_2021[[counter]] <- 2021 - age
 
        counter <- counter + 1
    }
    else if(clength == 6 && substring(value,1,1) != 2){
        yob_value <- substring(value, 1, 1)
        genline_value <- substring(value, 2,2)
        identifier_value <- substring(value, 3, 6)
        
        year_data[[12]]$yob[[counter]] <- yob_value
        year_data[[12]]$genline[[counter]] <- genline_value
        year_data[[12]]$identifier[[counter]] <- identifier_value
        year_data[[12]]$eid[[counter]] <- value
        
        age <- as.numeric(yob_value) + 2010
        year_data[[12]]$year[[counter]] <- substring(age, 3, 4)
        
        yob_numeric_2021[[counter]] <- 2021 - age

        counter <- counter + 1
    }
    else if(clength == 5){
        yob_value <- substring(value, 1, 1)
        genline_value <- substring(value, 2,2)
        identifier_value <- substring(value, 3, 5)
        
        year_data[[12]]$yob[[counter]] <- yob_value
        year_data[[12]]$genline[[counter]] <- genline_value
        year_data[[12]]$identifier[[counter]] <- identifier_value
        year_data[[12]]$eid[[counter]] <- value
        
        age <- as.numeric(yob_value) + 2010
        year_data[[12]]$year[[counter]] <- substring(age, 3, 4)
        
        yob_numeric_2021[[counter]] <- 2021 - age
    
        counter <- counter + 1
    }
}
```


## Merge full_report with age 2021
```{r}
# Creates an intermediary data frame that contains all EIDs that made it through and their associated ages.
eid_2021 <- unlist(year_data[[12]]$eid)

# Adds the age data onto the full_report data.
merged_2021_data <- data.frame(age = unlist(yob_numeric_2021), eid = eid_2021)
E_2021_data <- merge(full_report_2021, merged_2021_data, by.x = "Dam.ear.tag", by.y = "eid", all = TRUE)

# Changes the name of the progeny data.
colnames(E_2021_data)[colnames(E_2021_data) == "E21_lamb_reports$Lamb.Ear.Tag.."] <- "# of Lambs Birthed"
```


## Determining the age of the ewe 2020
```{r}
# Removed unnecessary data.
progeny_2020 <- progeny_2020[-c(1511:1518),]

# A list to contain the animal's year of birth for animals in the progeny_2020 data frame.
yob_numeric_2020 <- list()

# Counter to increment through the sub-lists.
counter <- 1

# A for loop to go through the year's EID column and break it up into the parts that we want.
for(i in 1:length(progeny_2020$`E20_lamb_reports$Ewe.Ear.Tag..`)){
    
    # The actual EID.
    value <- progeny_2020$`E20_lamb_reports$Ewe.Ear.Tag..`[[i]]
    
    # The length of the EID.
    clength <- nchar(value)
    
    # The different lengths of EIDs mean that the components we want will be in different places. We filter by length to specify the location of each component.
    # Some EIDs that also start with 2 will also have different places for the components so we filter by those to also specify the location of each component.
    if(clength == 7){
        
        # Splits up the EID into its components: year of birth, genline number, and identifier number.
        yob_value <- substring(value, 1, 2)
        genline_value <- substring(value, 3, 3)
        identifier_value <- substring(value, 4, 7)

        # Places these components into their corresponding sub-lists in year_data.
        year_data[[13]]$yob[[counter]] <- yob_value
        year_data[[13]]$genline[[counter]] <- genline_value
        year_data[[13]]$identifier[[counter]] <- identifier_value
        year_data[[13]]$eid[[counter]] <- value
        
        # Calculates the age of the animal and places the value (YY) into the year_data list.
        age <- as.numeric(yob_value) + 2000
        year_data[[13]]$year[[counter]] <- substring(age, 3, 4)
        
        # Places the age we calculated into the list we made containing all the ages.
        yob_numeric_2020[[counter]] <- 2020 - age
        
        # Increment counter by 1 to move onto the next EID.
        counter <- counter + 1
    }
    
    # The steps in between each if-else statements will be repeated for all the EIDs that make it through the filters we have put in place.
    # Some EIDs that we want though, may not make it through the filters and would impact our final results.
    else if(clength == 6 && substring(value, 1, 1) == 2){
        yob_value <- substring(value, 1, 2)
        genline_value <- substring(value, 3, 3)
        identifier_value <- substring(value, 4, 6)
        
        year_data[[13]]$yob[[counter]] <- yob_value
        year_data[[13]]$genline[[counter]] <- genline_value
        year_data[[13]]$identifier[[counter]] <- identifier_value
        year_data[[13]]$eid[[counter]] <- value
        
        age <- as.numeric(yob_value) + 2000
        year_data[[13]]$year[[counter]] <- substring(age, 3, 4)
        
        yob_numeric_2020[[counter]] <- 2020 - age
    
        counter <- counter + 1
    }
    else if(clength == 6 && substring(value,1,1) != 2){
        yob_value <- substring(value, 1, 1)
        genline_value <- substring(value, 2,2)
        identifier_value <- substring(value, 3, 6)
        
        year_data[[13]]$yob[[counter]] <- yob_value
        year_data[[13]]$genline[[counter]] <- genline_value
        year_data[[13]]$identifier[[counter]] <- identifier_value
        year_data[[13]]$eid[[counter]] <- value
        
        age <- as.numeric(yob_value) + 2010
        year_data[[13]]$year[[counter]] <- substring(age, 3, 4)
        
        yob_numeric_2020[[counter]] <- 2020 - age
        
        counter <- counter + 1
    }
    else if(clength == 5){
        yob_value <- substring(value, 1, 1)
        genline_value <- substring(value, 2,2)
        identifier_value <- substring(value, 3, 5)
        
        year_data[[13]]$yob[[counter]] <- yob_value
        year_data[[13]]$genline[[counter]] <- genline_value
        year_data[[13]]$identifier[[counter]] <- identifier_value
        year_data[[13]]$eid[[counter]] <- value
        
        age <- as.numeric(yob_value) + 2010
        year_data[[13]]$year[[counter]] <- substring(age, 3, 4)
        
        yob_numeric_2020[[counter]] <- 2020 - age
        
        counter <- counter + 1
    }
}
```


## Merge full report with age 2020
```{r}
# Creates an intermediary data frame that contains all EIDs that made it through and their associated ages.
eid_2020 <- unlist(year_data[[13]]$eid)

# Adds the age data onto the full_report data.
merged_2020_data <- data.frame(age = unlist(yob_numeric_2020), eid = eid_2020)
E_2020_data <- merge(full_report_2020, merged_2020_data, by.x = "Ewe.Ear.Tag..", by.y = "eid", all = TRUE)

# Changes the name of the progeny data.
colnames(E_2020_data)[colnames(E_2020_data) == "E20_lamb_reports$Lamb.Ear.Tag.."] <- "# of Lambs Birthed"
```

## Adjusting the colnames of each report for merging
```{r}
# The test data frames removed unnecessary data from the E_[year]_data data frames, removed any duplicated rows, and renamed all the columns. For 2012-2023.
test_12 <- E_2012_data[-c(8:12)]
test_12 <- unique(test_12)
colnames(test_12) <- c("Ewe.Ear.Tag", "Lambing.Dates", "Ewe.Paint.Brand", "Lamb.Ear.Tag", "Sex", "Reference", "Comments", "Years", "Lambs.Birthed", "Age")

test_13 <- E_2013_data[-c(8:12)]
test_13 <- unique(test_13)
colnames(test_13) <- c("Ewe.Ear.Tag", "Lambing.Dates", "Ewe.Paint.Brand", "Lamb.Ear.Tag", "Sex", "Reference", "Comments", "Years", "Lambs.Birthed", "Age")

test_14 <- E_2014_data[-c(8:12)]
test_14 <- unique(test_14)
colnames(test_14) <- c("Ewe.Ear.Tag", "Lambing.Dates", "Ewe.Paint.Brand", "Lamb.Ear.Tag", "Sex", "Reference", "Comments", "Years", "Lambs.Birthed", "Age")

test_15 <- E_2015_data[-c(8:12)]
test_15 <- unique(test_15)
colnames(test_15) <- c("Ewe.Ear.Tag", "Lambing.Dates", "Ewe.Paint.Brand", "Lamb.Ear.Tag", "Sex", "Reference", "Comments", "Years", "Lambs.Birthed", "Age")

test_16 <- E_2016_data[-c(8:12)]
test_16 <- unique(test_16)
colnames(test_16) <- c("Ewe.Ear.Tag", "Lambing.Dates", "Ewe.Paint.Brand", "Lamb.Ear.Tag", "Sex", "Reference", "Comments", "Years", "Lambs.Birthed", "Age")

test_17 <- E_2017_data[-c(8:12)]
test_17 <- unique(test_17)
colnames(test_17) <- c("Ewe.Ear.Tag", "Lambing.Dates", "Ewe.Paint.Brand", "Lamb.Ear.Tag", "Sex", "Reference", "Comments", "Years", "Lambs.Birthed", "Age")

test_18 <- E_2018_data[-c(8:12)]
test_18 <- unique(test_18)
colnames(test_18) <- c("Ewe.Ear.Tag", "Lambing.Dates", "Ewe.Paint.Brand", "Lamb.Ear.Tag", "Sex", "Reference", "Comments", "Years", "Lambs.Birthed", "Age")

test_19 <- E_2019_data[-c(8:12)]
test_19 <- unique(test_19)
colnames(test_19) <- c("Ewe.Ear.Tag", "Lambing.Dates", "Ewe.Paint.Brand", "Lamb.Ear.Tag", "Sex", "Reference", "Comments", "Years", "Lambs.Birthed", "Age")


# From 2020-2023, we added a years column formatted YYYY. This tells us the year of lambing report the animal came from.
test_20 <- unique(E_2020_data)
test_20$Years <- 2020
colnames(test_20) <- c("Ewe.Ear.Tag", "Lambing.Dates", "Ewe.Paint.Brand", "Lamb.Ear.Tag", "Sex", "Comments", "X", "Page", "Lambs.Birthed", "Age", "Years")

test_21 <- E_2021_data[-c(3:4, 10)]
test_21 <- unique(test_21)
colnames(test_21) <- c("Ewe.Ear.Tag", "Lambing.Dates", "X", "Birth.Year", "Dam.Line", "Dam.Breed", "Lamb.Ear.Tag", "Twin", "Sex", "AI.Ram", "Color", "Comments", "Lambs.Birthed", "Age")
test_21$Years <- 2021

test_22 <- E_2022_data
test_22 <- unique(test_22)
colnames(test_22) <- c("Ewe.Ear.Tag", "Lambing.Dates","Ewe.Paint.Brand", "Dam.Electronic.Ear.Tag", "X", "Birth.Year", "Dam.Line", "Dam.Breed","Lamb.Ear.Tag", "Lamb.Electronic.Ear.Tag", "Twin", "Sex", "AI.Ram", "Color", "Comments", "Lambs.Birthed", "Age")
test_22$Years <- 2022

test_23 <- E_2023_data
test_23 <- unique(test_23)
colnames(test_23) <- c("Ewe.Ear.Tag", "Lamb.Dates", "Ewe.Paint.Brand", "Lamb.Ear.Tag", "Sex", "Comments", "Lambs.Birthed", "Age")
test_23$Years <- 2023
```


## Merging dataframes 2012-2023 containing age and # of lambs birthed
```{r}
# The final data frame contains all the lambing report information from 2012-2023. This information contains the lamb data, progeny information, and age.
first_merge <- rbind.fill(test_12, test_13)
second_merge <- rbind.fill(first_merge, test_14)
third_merge <- rbind.fill(second_merge, test_15)
fourth_merge <- rbind.fill(third_merge, test_16)
fifth_merge <- rbind.fill(fourth_merge, test_17)
sixth_merge <- rbind.fill(fifth_merge, test_18)
seventh_merge <- rbind.fill(sixth_merge, test_19)
eighth_merge <- rbind.fill(seventh_merge, test_20)
ninth_merge <- rbind.fill(eighth_merge, test_21)
tenth_merge <- rbind.fill(ninth_merge, test_22)
eleventh_merge <- rbind.fill(tenth_merge, test_23)
final <- eleventh_merge
final <- unique(final)
```


## Converting EIDs to UIDs and adding them to the dataframe
```{r}
# Reading in the necessary files.
# lamb_card_2023 <- read.csv("C:\\Users\\anshe\\OneDrive - University of Nevada, Reno\\Desktop\\GWAS S24\\Lamb Card Data 2023.csv")
# sample_table <- read.csv("C:\\Users\\anshe\\OneDrive - University of Nevada, Reno\\Desktop\\GWAS S24\\s11645Samples Table.csv")
# samples_table_2 <- read.csv("C:\\Users\\anshe\\OneDrive - University of Nevada, Reno\\Desktop\\GWAS S24\\Samples Table.csv")

lamb_card_2023 <- read.csv("Files/Lamb Card Data 2023[95].csv")
sample_table <- read.csv("Files/s11645Samples Table.csv")
samples_table_2 <- read.csv("Files/Samples Table.csv")

# Merges lamb_card_2023, which contains the animal's data, and sample_table, which contains more of the animal's data.
card_and_table <- merge(lamb_card_2023, sample_table, by.x = "TSU", by.y = "SampleID", all = TRUE)

# A list which will be used to convert the genline number to the breed.
num_to_breed <- list(
    "1" = "MER",
    "5" = "RAMB",
    "9" = "UNKN",
    "7" = "MER"
)

# A list that will hold the EID and UID information.
tags <- list(
    uids <- list(),
    eids <- list()
)

# A counter to increment through the UID and EID lists.
counter_tags <- 1

# A for loop to iterate through each year in the year_data list.
for(i in c(1,3:13)){
    
    # A counter to iterate through each EID.
    counter <- 1
    
    # A for loop that will iterate through each EID for each year's data and convert it to the corresponding UID.
    for(j in 1:length(year_data[[i]]$yob)){
        
        # Filters out any EIDs that start with the letter A.
        if(substring(year_data[[i]]$eid[[j]], 1, 1) != 'A' ){
            
            # Splits the EID into the componenets that we want.
            year_value <- year_data[[i]]$year[[j]]
            genline_value <- year_data[[i]]$genline[[j]]
            identifier_value <- year_data[[i]]$identifier[[j]]
    
            # Converts the genline number to the breed.
            breed <- num_to_breed[[genline_value]]
            
            # Creates the UID.
            uid <- paste(identifier_value, "/", year_value, "_", breed, sep="")
            
            # Adds the EIDs and UIDs to the tags list
            tags$uids[[counter_tags]] <- uid
            tags$eids[[counter_tags]] <- year_data[[i]]$eid[[counter]]
            
            # Increments the counters to move onto the next EID and UID.
            counter <- counter + 1
            counter_tags <- counter_tags + 1

        }
    }
}

# Creates a data frame from the tags list.
tag_df <- data.frame(uid = unlist(tags$uids), eid = unlist(tags$eids))

# Adds the UID information to our lamb card.
lamb_card_tag <- merge(lamb_card_2023, tag_df, by.x = "Ewe.ID", by.y = "eid")

# Adds the UID information to our data frame containing the animal's data and removes any duplicated rows.
card_and_table_tag <- merge(card_and_table, tag_df, by.x = "Ewe.ID", by.y = "eid")
card_and_table_tag <- unique(card_and_table_tag)

# Creates a data frame with only the necessary data we need: TSU, AnimalID, Paint Brand, Sex, Weight, Birth Date, Birth Time, LambID
ctt <- subset(card_and_table_tag, select =-c(AnimalID, Stud, YOB, UIDTag, Breed, Species, Plate_ID, Manifest, ControlPos, Index, Sample.ID, Call.Rate, Gender, Gender.Est, Comment, uid))

# Creates a data frame with the animals ID, TSU, and related data.
sample_ctt <- merge(sample_table, ctt, by.x = "SampleID", by.y = "TSU")

# Filters out any animals that don't have a TSU or UID.
test <- filter(card_and_table_tag, !is.na(card_and_table_tag$TSU) & !is.na(card_and_table_tag$uid))

# Creates a data frame with only the necessary information that we need: TSU, EweID, Paint Brand, Sex, Weight, Birth Date, Birth Time, LambID
test_test <- subset(test, select = -c(AnimalID, Stud, YOB, UIDTag, Breed, Species, Plate_ID, Manifest, ControlPos, Index, Sample.ID, Call.Rate, Gender, Gender.Est, Comment, uid))

# Creates a data frame with the animals ID, TSU, and related data.
sample_and_tst <- merge(sample_table, test_test, by.x = "SampleID", by.y = "TSU", all = TRUE)

# Filters out any animal that doesn't have a EweID, AnimalID, and year of birth greater than 0.
sample_and_tst <- filter(sample_and_tst, !is.na(sample_and_tst$Ewe.ID) & !is.na(sample_and_tst$AnimalID) & sample_and_tst$YOB != 0)

# Added a UID column.
samples_table_2$uid <- paste(samples_table_2$UIDTag,"MER",sep="_")

# Creatrs a data frame with all of our animals from the sample tables and their data.
final_2 <- rbind.fill(samples_table_2, ctt)

# Combines the two sample tables, fixes mistakes from the uid column adds more related to the animals together.
tables_combined <- rbind.fill(sample_table, samples_table_2)
tables_combined$uid[1:1577] <- tables_combined$UIDTag[1:1577]
tables_and_samples <- merge(test_test, tables_combined, by.x = "TSU", by.y = "SampleID")
```


## Merging the metadata of the ewes together
```{r}
# merino_ewes <- read.csv("C:\\Users\\anshe\\OneDrive - University of Nevada, Reno\\Desktop\\GWAS S24\\Merino Ewes 1-11-24-689.csv")
# R7_ewes <- read.csv("C:\\Users\\anshe\\OneDrive - University of Nevada, Reno\\Desktop\\GWAS S24\\R7 Ewes 12-23_fix_650.csv")
# problem_tsus <- fread("C:\\Users\\anshe\\OneDrive - University of Nevada, Reno\\Desktop\\GWAS S24\\Problem_TSUs.csv")

# Reading in files.
merino_ewes <- read.csv("Files/Merino Ewes 1-11-24-689.csv")
R7_ewes <- read.csv("Files/R7 Ewes 12-23_fix_650.csv")
problem_tsus <- fread("Files/Problem_TSUs.csv")

# Creates a data frame with all the animal's phenotypic data.
merged <- rbind.fill(merino_ewes, R7_ewes)

# Removes duplicated rows, unnecessary data, and fixes the formatting of some of the data.
merged <- unique(merged)
merged <- merged[-c(1168), ]
merged$Animal.Eartag[120] <- "211072"
merged$Animal.Eartag[73] <- "201682"
merged$Animal.Eartag[158] <- "5695"
merged$Animal.Eartag[164] <- "71729"
merged$Animal.Eartag[73] <- "211223"
merged$Animal.Eartag[63] <- "2210819"
merged$Animal.Eartag[304] <- "211223"

# Changes all the Ear Tags to a character type.
merged$Animal.Eartag=as.character(as.numeric(merged$Animal.Eartag))

# Creates a data frame containing the metadata of the animal.
file <- merge(merged, sample_and_tst, by.x = "Animal.Eartag", by.y = "Ewe.ID")

# Removes any duplicated rows.
file <- file[-which(duplicated(file$Animal.Eartag)),]

# Adds on the lambing report data of the animal and removes any duplicated rows.
merging <- merge(sample_ctt, final, by.x = "Ewe.ID", by.y = "Ewe.Ear.Tag")
merging <- merging[-which(duplicated(merging$Ewe.ID) == TRUE),]

# Creates a new list for converting EIDs to UIDs.
new_tags <- list(
    uids <- list(),
    eids <- list()
)

# Counter to increment through the sub-lists
new_counter_tags <- 1

# A for loop to convert EIDs to UIDs.
for(i in 1:length(merged$Animal.Eartag)){
    # Some EIDs that  start with 2 will  have different places for the components so we filter by those to also specify the location of each component.
    # We also don't want to interact with any empty values in the column, so we filter by those as well.
    if(!is.na(merged$Animal.Eartag[[i]]) & substring(merged$Animal.Eartag[[i]],1,1) == 2){
        
        # Breaks up the EID into the components that we want: year of birth, genline number, and identifier number.
        year_value <- substring(merged$Animal.Eartag[[i]],1,2)
        genline_value <- substring(merged$Animal.Eartag[[i]],3,3)
        identifier_value <- substring(merged$Animal.Eartag[[i]],4, 7)


        # Converts the genline to the breed and creates the UID.
        breed <- num_to_breed[[genline_value]]
        uid <- paste(identifier_value, "/", as.character(year_value), "_", breed, sep="")
        
        # Adds the UID and EID to the new_tags list.
        new_tags$uids[[new_counter_tags]] <- uid
        new_tags$eids[[new_counter_tags]] <- merged$Animal.Eartag[[i]]

        # Increments counter to move onto the next EID.
        new_counter_tags <- new_counter_tags + 1
    }
    
    # The steps from above are repeated, except the location of our components are different so we account for that change.
    else if(!is.na(merged$Animal.Eartag[[i]])){
        year_value <- substring(merged$Animal.Eartag[[i]],1,1)
        genline_value <- substring(merged$Animal.Eartag[[i]],2,2)
        identifier_value <- substring(merged$Animal.Eartag[[i]],3, 7)

        # Gets the year the animal was born
        year_value <- as.numeric(year_value) + 10

        breed <- num_to_breed[[genline_value]]
        uid <- paste(identifier_value, "/", as.character(year_value), "_", breed, sep="")
        new_tags$uids[[new_counter_tags]] <- uid
        new_tags$eids[[new_counter_tags]] <- merged$Animal.Eartag[[i]]

        new_counter_tags <- new_counter_tags + 1
    }

}

# Creates a data frame from the new_tag list, which contains the EIDs and associated UIDs.
new_tag_df <- data.frame(uid = unlist(new_tags$uids), eid = unlist(new_tags$eids))

# Adds the UID information to the phenotypic data.
merged_and_tags <- merge(merged, new_tag_df, by.x = "Animal.Eartag", by.y = "eid")
merged_and_tags <- unique(merged_and_tags)

# Adds the UID and phenotypic data to our metadata. Remove any duplicated rows.
final_3 <- merge(final_2, merged_and_tags, by.x = "uid", by.y = "uid")
final_3 <- unique(final_3)
final_3 <- final_3[-which(duplicated(final_3$uid)),]
```


## Combining genotypic and phenotypic data
```{r}
# Adds UID, phenotypic data, and meta data to our combined samples table data. Removes any problematic and duplicated data.
genotypes_and_phenotypes <- merge(tables_combined, merged_and_tags, by.x = "uid", by.y = "uid")
genotypes_and_phenotypes <- genotypes_and_phenotypes[-which(genotypes_and_phenotypes$SampleID %in% problem_tsus$`TSU#`),]
genotypes_and_phenotypes <- genotypes_and_phenotypes[-which(duplicated(genotypes_and_phenotypes$uid)),]

# Changes the EIDs to a character type.
genotypes_and_phenotypes$Animal.Eartag <- as.character(genotypes_and_phenotypes$Animal.Eartag)

# Adds the lamb report data to our data frame from above. Removes any duplicated rows.
gp_and_lambing_records <- merge(genotypes_and_phenotypes, final, by.x = "Animal.Eartag", by.y = "Ewe.Ear.Tag")
gp_and_lambing_records <- unique(gp_and_lambing_records)

# A list to help with converting genline numbers to the associated breed.
num_to_breed <- list(
    "1" = "MER",
    "5" = "RAMB",
    "9" = "UNKN",
    "7" = "MER"
)

# A list to hold the information regarding EID and its associated breed.
breed_list <- list(
    breed <- list(),
    eid <- list()
)

# Counter to increment through the sub-lists.
counter <- 1

# A for loop to identify the animal's breed.
for(i in 1:length(gp_and_lambing_records$Animal.Eartag)){
    
    # Some EIDs that  start with 2 will  have different places for the components so we filter by those to also specify the location of each component.
    if(substring(gp_and_lambing_records$Animal.Eartag[[i]], 1, 1) == '2'){
        
        # Gets the genline number and converts it to the associated breed.
        number <- str_sub(gp_and_lambing_records$Animal.Eartag[[i]], 3, 3)
        breedline <- num_to_breed[[number]]
        
        # Adds the EID and breed to our breed_list list.
        breed_list$breed[[counter]] <- breedline
        breed_list$eid[[counter]] <- gp_and_lambing_records$Animal.Eartag[[i]]
        
        counter <- counter + 1
    }
    
    # The steps from above are repeated, accounting for the different location of the genline number.
    else{
        number <- str_sub(gp_and_lambing_records$Animal.Eartag[[i]],2,2)
        breedline <- num_to_breed[[number]]
        breed_list$breed[[counter]] <- breedline
        breed_list$eid[[counter]] <- gp_and_lambing_records$Animal.Eartag[[i]]
        
        counter <- counter + 1
        
    }
}

# Creates a data frame from the breed list data. Removes any duplicated rows.
breed_list <- data.frame(Gen.Line = unlist(breed_list$breed), Ewe.Id = unlist(breed_list$eid))
breed_list <- unique(breed_list)

# Removes any duplicated rows.
gp_and_lambing_records <- unique(gp_and_lambing_records)

# Adds on our breed data to genotype, phenotype, and lambing records data frame.
gp_and_lambing_records_test <- merge(gp_and_lambing_records, breed_list, by.x = "Animal.Eartag", by.y = "Ewe.Id")

# Filters out any animals that were born in 2023. Removes any duplicated rows.
filtered_df <- gp_and_lambing_records_test[which(gp_and_lambing_records_test$Years == '2023'),]
filtered_df <- unique(filtered_df)

# Finds the unique duplicated EIDs and filters out those animals from filtered_df
dupnames=unique(filtered_df[which(duplicated(filtered_df$Animal.Eartag)),"Animal.Eartag"])
filtered_df[which(filtered_df$Animal.Eartag %in% dupnames),c(1:3,60:70)]

# Removes any unneeded columns.
filtered_df=as.data.frame(filtered_df)
filtered_df <- filtered_df[, -which(names(filtered_df) %in% c("Sex","Lamb.Ear.Tag","Comments","Lambing.Dates","Ewe.Paint.Brand"))]

# Removes duplicated rows.
filtered_df <- unique(filtered_df)

# Gets the unique, duplicated EIDs and fixes formatting in EID data.
filtered_dupnames <- unique(filtered_df[which(duplicated(filtered_df$Animal.Eartag)), "Animal.Eartag"])
filtered_df[which(filtered_df$Animal.Eartag %in% filtered_dupnames),]
final$Lamb.Ear.Tag <- gsub('-','',final$Lamb.Ear.Tag)

# Merges animals born in 2023 with the our lambing records data.
gp_and_lambing_records_test <- merge(final, filtered_df, by.x = "Lamb.Ear.Tag", by.y = "Animal.Eartag", all.y = TRUE)

# Identifies duplicated EIDs and removes them from the data.
lamb_dupnames <- unique(gp_and_lambing_records_test[which(duplicated(gp_and_lambing_records_test$Lamb.Ear.Tag)), "Lamb.Ear.Tag"])
gp_and_lambing_records_test[which(gp_and_lambing_records_test$Lamb.Ear.Tag %in% lamb_dupnames),]
gp_and_lambing_records_test <- gp_and_lambing_records_test[-which(gp_and_lambing_records_test$Lamb.Ear.Tag %in% lamb_dupnames),]

# Fixes formatting of the EIDs.
gp_and_lambing_records_test$Lamb.Ear.Tag <- gsub("-", "", gp_and_lambing_records_test$Lamb.Ear.Tag)

# Identifies number of progeny for each ewe.
lambs_list <- list(
    num_of_lambs <- list(),
    eid <- list(),
    in_or_not <- list()
)

# Counter to increment through the sublists.
counter <- 1

# A for loop to get the progeny, EID, and whether or not the animal is in the complete lambing record data.
for(i in 1:length(gp_and_lambing_records_test$Ewe.Ear.Tag)){
    
    # Gets the row that the animal is in only if they match up with the animal in our gp_and_lambing_records data.
    u=final[which(final$Ewe.Ear.Tag==gp_and_lambing_records_test$Ewe.Ear.Tag[[i]]  & final$Years==gp_and_lambing_records_test$Years.x[[i]]),]
    
    # Adds their data to our lambs_list
    lambs_list$num_of_lambs[[counter]] <- nrow(u)
    lambs_list$eid[[counter]] <- gp_and_lambing_records_test$Ewe.Ear.Tag[[i]]
    lambs_list$in_or_not[[counter]] <- gp_and_lambing_records_test$Lamb.Ear.Tag[[i]] %in% u$Lamb.Ear.Tag
    
    # Increments counter to move onto the next animal.
    counter <- counter + 1
}

# Makes a data frame from the lambs_list data.
lambs_list <- data.frame(Num = unlist(lambs_list$num_of_lambs), Ewe.ID = unlist(lambs_list$eid), In = unlist(lambs_list$in_or_not))

# Converts gp_and_lambing_records_test into a data frame.
gp_and_lambing_records_test <- as.data.frame(gp_and_lambing_records_test)

# Replaces any empty values in Years.x as NA.
gp_and_lambing_records_test$Years.x[unlist(lapply(gp_and_lambing_records_test$Years, is.null))] <- NA

# Adds in the number of lambs birthed and whether or not the animal was in the "final" data frame. Removed any duplciated rows
gp_and_lambing_records_list <- merge(gp_and_lambing_records_test, lambs_list, by.x = "Ewe.Ear.Tag", by.y = "Ewe.ID")
gp_and_lambing_records_list <- unique(gp_and_lambing_records_list)

# Reads in genotypic data.
# snp_genotypes <- fread("C:\\Users\\anshe\\OneDrive - University of Nevada, Reno\\Desktop\\GWAS S24\\snp_genotypes.csv")
# snp_info_from_linux <- fread("C:\\Users\\anshe\\OneDrive - University of Nevada, Reno\\Desktop\\GWAS S24\\output.txt")

snp_genotypes <- fread("Files/snp_genotypes.csv")
snp_info_from_linux <- fread("Files/output.txt")

# Changed the name of the first column.
names(snp_info_from_linux)[names(snp_info_from_linux) == 'V1'] <- 'ID'

# Created a data frame containing all our genotypic information
snps <- rbind(snp_genotypes, snp_info_from_linux)

# Created a data frame with animals only born in 2023.
animals_from_2023 <- final[which(final$Years == '2023'),]

# Sorts the lambing dates by ascending order
animals_from_2023 <- animals_from_2023[order(as.Date(animals_from_2023$Lambing.Dates, format="%m/%d/%Y")),]

# Creates management groups by splitting the animals_from_2023 into thirds.
animals_from_2023$Management.Group <- 0
animals_from_2023$Management.Group[c(767:1532)] <- 1
animals_from_2023$Management.Group[c(1533:2299)] <- 2

# Adds in management group information. Removes any duplicated values.
gp_and_lambing_records_test <- merge(gp_and_lambing_records_test, animals_from_2023, by.x = "Lamb.Ear.Tag", by.y = "Ewe.Ear.Tag")
gp_and_lambing_records_test <- unique(gp_and_lambing_records_test)
gp_and_lambing_records_test <- gp_and_lambing_records_test[-which(duplicated(gp_and_lambing_records_test$Lamb.Ear.Tag)),]

# Removes any unwanted columns
colnames_keep <- c("Lamb.Ear.Tag", "Sample.ID", "Gen.Line", "Management.Group", "Lambs.Birthed.x", "Age.x", "Age.y", "Lambs.Birthed.y", "CEM", "Mic.Ave", "CV.Mic", "SF.Mic", "SL.mm")
gp_and_lambing_records_test <- gp_and_lambing_records_test[, colnames_keep]
 
# Combines all our meta data, phenotypic data, and genetic data into a data frame which will be used for analysis.
gpl_and_snps <- merge(gp_and_lambing_records_test, snps, by.x = "Sample.ID", by.y = "ID")

# Filters out any missing values from the columns
gpl_and_snps <- gpl_and_snps[-which(is.na(gpl_and_snps$Age.x)),]
```


## Analysis
```{r}
# Splits the data frame into phenotypic and genotypic information.
cn = which(colnames(gpl_and_snps) == "SL.mm")
phenotypes = gpl_and_snps[,1:cn]
genotypes = gpl_and_snps[,(cn+1):ncol(gpl_and_snps)]


map <- fread("Files/snp_info_file_[AgR_Ovine_60Kplus_20007095X378523_A1]_[6559].txt")

# Removes any unnecessary Chromosomes
genotypes <- genotypes[,-which(map$Chromosome_No == 'X' | map$Chromosome_No == '29' | map$Chromosome_No == '0' | map$Chromosome_No == 'MT')]

# Finding the call rates
miss = is.na(genotypes)
snp_cr = (1 - colMeans(miss))
animal_cr = (1 - rowMeans(miss))

# Filtered for the minor allele frequency
gf1 = genotypes[, -which(snp_cr < 0.95)]
af = colMeans(gf1, na.rm=TRUE)/2
gf2 = gf1[, -which(af > 0.95 | af < 0.05)]

# Replaces the associated numbers with the corresponding genotypes
genotypeCounts = function(genotypes){
    t(apply(genotypes,2,function(d){c(AA=sum(d==0),AB=sum(d==1),BB=sum(d==2))}))
}

# Filles in missing data.
gf3 = na.aggregate(gf2)

# Normalizes the data.
gf5 = scale(gf3)

# Standardizes the data and finds the cross-product of X.
X = gf5/sqrt(ncol(gf5))
G = tcrossprod(X)
```


## Heritability
```{r}
# The design matrix we used.
design.mat = model.matrix(~as.factor(phenotypes$Age.x)+as.factor(phenotypes$Age.y)+as.factor(phenotypes$Gen.Line)+as.factor(phenotypes$Management.Group)+as.factor(phenotypes$Lambs.Birthed.x)+as.factor(phenotypes$Lambs.Birthed.y))
design.mat <- make_full_rank(design.mat,with.intercept=FALSE)

# Used to find the mean and standard deviation of our heritability for the following fives traits:
    # Coarse Edge Micron (CEM)
    # Average Fiber Diameter (MIC.AVE)
    # Coefficient of Variation in Microns (CV.MIC)
    # Spinning Fineness in Microns (SF.MIC)
    # Staple Length in Millimeters (SL.MM)

# Sets RNG state
set.seed(123)

# Runs our Bayesian Generalized Linear Regression model
#fm1=BGLR(y=unlist(phenotypes$CEM),ETA=list(list(X=design.mat, model="FIXED"),list(K=G,model='RKHS')),R2=0.44,nIter=60000,burnIn=10000,verbose=T,saveAt='eig_CEM_')
fm1=BGLR(y=unlist(phenotypes$CEM),ETA=list(list(X=design.mat, model="FIXED"),list(K=G,model='RKHS')),R2=0.44,nIter=60000,burnIn=10000,verbose=T,saveAt='eig_CEM_')

# Reads in our environmental and genotypic variance data.
varE=scan( 'eig_CEM_varE.dat')
varU=scan('eig_CEM_ETA_2_varU.dat')

# Calculates our heritability, as well as its mean and standard deviation.
h2_CEM=varU[-c(1:2000)]/(varU[-c(1:2000)]+varE[-c(1:2000)])
mean_CEM_h2 <- mean(h2_CEM)  
sd_CEM_h2 <- sd(h2_CEM)

# The steps above are repeated for the other four traits that we are looking at.
set.seed(123)
fm1=BGLR(y=unlist(phenotypes$CEM),ETA=list(list(X=design.mat, model="FIXED"),list(K=G,model='RKHS')),R2=0.44,nIter=60000,burnIn=10000,verbose=T,saveAt='eig_MIC.AVE_')
varE=scan( 'eig_MIC.AVE_varE.dat')
varU=scan('eig_MIC.AVE_ETA_2_varU.dat')
h2_MIC.AVE=varU[-c(1:2000)]/(varU[-c(1:2000)]+varE[-c(1:2000)])
mean_MIC.AVE_h2 <- mean(h2_MIC.AVE)  
sd_MIC.AVE_h2 <- sd(h2_MIC.AVE)

set.seed(123)
fm1=BGLR(y=unlist(phenotypes$CEM),ETA=list(list(X=design.mat, model="FIXED"),list(K=G,model='RKHS')),R2=0.44,nIter=60000,burnIn=10000,verbose=T,saveAt='eig_CV.MIC_')
varE=scan( 'eig_CV.MIC_varE.dat')
varU=scan('eig_CV.MIC_ETA_2_varU.dat')
h2_CV.MIC=varU[-c(1:2000)]/(varU[-c(1:2000)]+varE[-c(1:2000)])
mean_CV.MIC_h2 <- mean(h2_CV.MIC)  
sd_CV.MIC_h2 <- sd(h2_CV.MIC)

set.seed(123)
fm1=BGLR(y=unlist(phenotypes$CEM),ETA=list(list(X=design.mat, model="FIXED"),list(K=G,model='RKHS')),R2=0.44,nIter=60000,burnIn=10000,verbose=T,saveAt='eig_SF.MIC_')
varE=scan( 'eig_SF.MIC_varE.dat')
varU=scan('eig_SF.MIC_ETA_2_varU.dat')
h2_SF.MIC=varU[-c(1:2000)]/(varU[-c(1:2000)]+varE[-c(1:2000)])
mean_SF.MIC_h2 <- mean(h2_SF.MIC)  
sd_SF.MIC_h2 <- sd(h2_SF.MIC)

set.seed(123)
fm1=BGLR(y=unlist(phenotypes$CEM),ETA=list(list(X=design.mat, model="FIXED"),list(K=G,model='RKHS')),R2=0.44,nIter=60000,burnIn=10000,verbose=T,saveAt='eig_SL.MM_')
varE=scan( 'eig_SL.MM_varE.dat')
varU=scan('eig_SL.MM_ETA_2_varU.dat')
h2_SL.MM=varU[-c(1:2000)]/(varU[-c(1:2000)]+varE[-c(1:2000)])
mean_SL.MM_h2 <- mean(h2_SL.MM)  
sd_SL.MM_h2 <- sd(h2_SL.MM)

# Groups SNPs into 1 Mb windows along each chromosome
getSNPWindows = function(map){
    map=map[order(as.numeric(map$Chromosome_No),map$Basepair_Position),]
    map$win = floor(map$Basepair_Position/1000000)
    map$ChrWin = paste(map$Chromosome_No,map$win,sep="_")
    w = as.data.frame(cbind(ChrWin = map$ChrWin[!duplicated(map$ChrWin)],Window = 1:length(unique(map$ChrWin))))
    m = merge(map,w)
    map = m[,c("SNP_Name","Chromosome_No","Basepair_Position","Window")]
    map
}

# Calculates the genetic variance within each window
getWindowVariances = function(model,genotypes,map){
    map = map[map$SNP_Name %in% colnames(genotypes),]
    map = getSNPWindows(map[,1:3])
    map$Window = as.numeric(as.character(map$Window))
    bhat=model[[21]][[2]][[14]]
    bv = as.matrix(genotypes) %*% as.matrix(bhat)
    varG = var(bv)
    outFile = c()
    for(w in 1:max(map$Window)){
        mark = map$SNP_Name[map$Window == w]
        bHatW = bhat[mark]
        ZW = genotypes[,mark]
        bvW = as.matrix(ZW) %*% as.matrix(bHatW)
        varGW = var(bvW)
        outFile = rbind(outFile,c(w,map$Chromosome_No[map$Window == w][1],floor(map$Basepair_Position[map$Window == w][1]/1000000),length(mark),round(varGW,6),round(varGW/varG*100,4)))
    }
    outFile = as.data.frame(outFile)
    colnames(outFile) = c("Window","Chr","Mb","numSNPs","GenVar","PCGenVar")
    outFile
}

# Used to find the percentage of genetic variance for the following fives traits:
    # Coarse Edge Micron (CEM)
    # Average Fiber Diameter (MIC.AVE)
    # Coefficient of Variation in Microns (CV.MIC)
    # Spinning Fineness in Microns (SF.MIC)
    # Staple Length in Millimeters (SL.MM)

# Sets RNG state
set.seed(123)

# Runs our Bayesian Generalized Linear Regression model.
fm2=BGLR(y=unlist(phenotypes$CEM),ETA=list(list(X=design.mat, model="FIXED"),list(X=X,model='BayesB',probIn=0.01,saveEffects=T)),R2=0.298593,nIter=60000,burnIn=10000,verbose=T,saveAt='BayesB_CEM_')

# Gets the window variance and sorts in decreasing order.
winvar=getWindowVariances(fm2,X,map)
winvar=winvar[order(winvar$PCGenVar,decreasing = T),]

# Writes the data onto a file.
file_CEM <- fwrite(winvar, file="winvar_CEM.csv")


# The steps above are repeated for the other four traits we are looking at.
set.seed(123)
fm2=BGLR(y=unlist(phenotypes$CEM),ETA=list(list(X=design.mat, model="FIXED"),list(X=X,model='BayesB',probIn=0.01,saveEffects=T)),R2=0.298593,nIter=60000,burnIn=10000,verbose=T,saveAt='BayesB_MIC.AVE_')
winvar=getWindowVariances(fm2,X,map)
winvar=winvar[order(winvar$PCGenVar,decreasing = T),]
file_MIC.AVE <- fwrite(winvar, file="winvar_MIC.AVE.csv")

set.seed(123)
fm2=BGLR(y=unlist(phenotypes$CEM),ETA=list(list(X=design.mat, model="FIXED"),list(X=X,model='BayesB',probIn=0.01,saveEffects=T)),R2=0.298593,nIter=60000,burnIn=10000,verbose=T,saveAt='BayesB_CV.MIC_')
winvar=getWindowVariances(fm2,X,map)
winvar=winvar[order(winvar$PCGenVar,decreasing = T),]
file_CV.MIC <- fwrite(winvar, file="winvar_CV.MIC.csv")

set.seed(123)
fm2=BGLR(y=unlist(phenotypes$CEM),ETA=list(list(X=design.mat, model="FIXED"),list(X=X,model='BayesB',probIn=0.01,saveEffects=T)),R2=0.298593,nIter=60000,burnIn=10000,verbose=T,saveAt='BayesB_SF.MIC_')
winvar=getWindowVariances(fm2,X,map)
winvar=winvar[order(winvar$PCGenVar,decreasing = T),]
file_SF.MIC <- fwrite(winvar, file="winvar_SF.MIC.csv")

set.seed(123)
fm2=BGLR(y=unlist(phenotypes$CEM),ETA=list(list(X=design.mat, model="FIXED"),list(X=X,model='BayesB',probIn=0.01,saveEffects=T)),R2=0.298593,nIter=60000,burnIn=10000,verbose=T,saveAt='BayesB_SL.MM_')
winvar=getWindowVariances(fm2,X,map)
winvar=winvar[order(winvar$PCGenVar,decreasing = T),]
file_SL.MM <- fwrite(winvar, file="winvar_SL.MM.csv")

# Calculates the genetic variance of each trait.
CEM_varU <- fread("eig_CEM_ETA_2_varU.dat")
mean_CEM_varU <- mean(CEM_varU$V1)
sd_CEM_varU <- sd(CEM_varU$V1)

MIC.AVE_varU <- fread("eig_MIC.AVE_ETA_2_varU.dat")
mean_MIC.AVE_varU <- mean(MIC.AVE_varU$V1)
sd_MIC.AVE_varU <- sd(MIC.AVE_varU$V1)

CV.MIC_varU <- fread("eig_CV.MIC_ETA_2_varU.dat")
mean_CV.MIC_varU <- mean(CV.MIC_varU$V1)
sd_CV.MIC_varU <- sd(CV.MIC_varU$V1)

SF.MIC_varU <- fread("eig_SF.MIC_ETA_2_varU.dat")
mean_SF.MIC_varU <- mean(SF.MIC_varU$V1)  
sd_SF.MIC_varU <- sd(SF.MIC_varU$V1)

SL.MM_varU <- fread("eig_SL.MM_ETA_2_varU.dat")
mean_SL.MM_varU <- mean(SL.MM_varU$V1)
sd_SL.MM_varU <- sd(SL.MM_varU$V1)

# Calculates the environmnetal variance of each trait.
CEM_varE <- fread("eig_CEM_varE.dat")
mean_CEM_varE <-mean(CEM_varE$V1)
sd_CEM_varE <- sd(CEM_varE$V1)

MIC.AVE_varE <- fread("eig_MIC.AVE_varE.dat")
mean_MIC.AVE_varE <- mean(MIC.AVE_varE$V1)
sd_MIC.AVE_varE <- sd(MIC.AVE_varE$V1)

CV.MIC_varE <- fread("eig_CV.MIC_varE.dat")
mean_CV.MIC_varE <- mean(CV.MIC_varE$V1)
sd_CV.MIC_varE <- sd(CV.MIC_varE$V1)

SF.MIC_varE <- fread("eig_SF.MIC_varE.dat")
mean_SF.MIC_varE <- mean(SF.MIC_varE$V1)
sd_SF.MIC_varE <- sd(SF.MIC_varE$V1)

SL.MM_varE <- fread("eig_SL.MM_varE.dat")
mean_SL.MM_varE <- mean(SL.MM_varE$V1)
sd_SL.MM_varE <- sd(SL.MM_varE$V1)

# Calculates the phenotypic variance of each trait.
CEM.varU <- as.numeric(readLines("eig_CEM_ETA_2_varU.dat"))
CEM.varE <- as.numeric(readLines("eig_CEM_varE.dat"))
CEM_varP <- CEM.varU + CEM.varE

MIC.AVE.varU <- as.numeric(readLines("eig_MIC.AVE_ETA_2_varU.dat"))
MIC.AVE.varE <- as.numeric(readLines("eig_MIC.AVE_varE.dat"))
MIC.AVE_varP <- MIC.AVE.varU + MIC.AVE.varE

CV.MIC.varU <- as.numeric(readLines("eig_CV.MIC_ETA_2_varU.dat"))
CV.MIC.varE <- as.numeric(readLines("eig_CV.MIC_varE.dat"))
CV.MIC_varP <- CV.MIC.varU + CV.MIC.varE

SF.MIC.varU <- as.numeric(readLines("eig_SF.MIC_ETA_2_varU.dat"))
SF.MIC.varE <- as.numeric(readLines("eig_SF.MIC_varE.dat"))
SF.MIC_varP <- SF.MIC.varU + SF.MIC.varE

SL.MM.varU <- as.numeric(readLines("eig_SL.MM_ETA_2_varU.dat"))
SL.MM.varE <- as.numeric(readLines("eig_SL.MM_varE.dat"))
SL.MM_varP <- SL.MM.varU + SL.MM.varE

# Finds the mean and standard deviation of the phenotypic variance.
mean_CEM_varP <- mean(CEM_varP)
sd_CEM_varP <- sd(CEM_varP)

mean_MIC.AVE_varP <- mean(MIC.AVE_varP)
sd_MIC.AVE_varP <- sd(MIC.AVE_varP)

mean_CV.MIC_varP <- mean(CV.MIC_varP)
sd_CV.MIC_varP <- sd(CV.MIC_varP)

mean_SF.MIC_varP <- mean(SF.MIC_varP)
sd_SF.MIC_varP <- sd(SF.MIC_varP)

mean_SL.MM_varP <- mean(SL.MM_varP)
sd_SL.MM_varP <- sd(SL.MM_varP)
```


## Multi-Trait
```{r}
# Performs a principal component analysis
pca <- prcomp(G)
per <- 100*pca$sdev^2/sum(pca$sdev^2)

# Sets RNG state
set.seed(123)

# Performs a multi-trait analysis on the following five traits:
    # Coarse Edge Micron (CEM)
    # Average Fiber Diameter (MIC.AVE)
    # Coefficient of Variation in Microns (CV.MIC)
    # Spinning Fineness in Microns (SF.MIC)
    # Staple Length in Millimeters (SL.MM)
y2 = as.matrix(phenotypes[,9:13])
fm3 <- Multitrait(y=y2 ,ETA=list(list(X=design.mat, model="FIXED"),list(K=G,model='RKHS')),nIter=60000,burnIn=10000,verbose=T,saveAt='eig_CEM_')

# The steps above are repeated for the other four traits.
set.seed(123)
y2 = as.matrix(phenotypes[,9:13])
fm3 <- Multitrait(y=y2 ,ETA=list(list(X=design.mat, model="FIXED"),list(K=G,model='RKHS')),nIter=60000,burnIn=10000,verbose=T,saveAt='eig_MIC.AVE_')

set.seed(123)
y2 = as.matrix(phenotypes[,9:13])
fm3 <- Multitrait(y=y2 ,ETA=list(list(X=design.mat, model="FIXED"),list(K=G,model='RKHS')),nIter=60000,burnIn=10000,verbose=T,saveAt='eig_CV.MIC_')

set.seed(123)
y2 = as.matrix(phenotypes[,9:13])
fm3 <- Multitrait(y=y2 ,ETA=list(list(X=design.mat, model="FIXED"),list(K=G,model='RKHS')),nIter=60000,burnIn=10000,verbose=T,saveAt='eig_SF.MIC_')

set.seed(123)
y2 = as.matrix(phenotypes[,9:13])
fm3 <- Multitrait(y=y2 ,ETA=list(list(X=design.mat, model="FIXED"),list(K=G,model='RKHS')),nIter=60000,burnIn=10000,verbose=T,saveAt='eig_SL.MM_')

# Used to calculate the genotypic correlation of our traits.

# Reads in our data.
fm3$ETA[[2]]$Cov
g=fread("eig_CEM_Omega_2.dat")

# Selects the necessary rows.
kg=g[(10000/10+1):nrow(g),]

# Calculates the mean, standard deviation, and number of traits respectively.
est=colMeans(kg)
se=apply(g,2,sd)
ntr=ncol(fm3$ETA[[2]]$u)

# Setting up our matrix.
mat= matrix(0, ntr, ntr)

# The lower triangle of the matrix is filled with data from the first row of kg.
mat[lower.tri(mat, diag=T)] <- as.vector(t(kg[1,]))

# The upper triangle is a copy of the lower triangle
mat[upper.tri(mat)] <- t(mat)[upper.tri(mat)]

# Converts our covariance matrix to a correlational one.
cor=cov2cor(mat)

# Renames the column and row names
rownames(cor)=colnames(cor)=seq(1:ntr)

# Creates a long data frame and transposes it.
corlong=as.data.frame(matrix(cor, dimnames=list(t(outer(colnames(cor), rownames(cor), FUN=paste)), NULL)))
cor.long=t(data.frame(corlong))

# Does the steps above for each row in kg.
for(i in 2:nrow(kg)){
    mat= matrix(0, ntr, ntr)
    mat[lower.tri(mat, diag=T)] <- as.vector(t(kg[i,]))
    mat[upper.tri(mat)] <- t(mat)[upper.tri(mat)]
    cor=cov2cor(mat)
    rownames(cor)=colnames(cor)=seq(1:ntr)
    corlong=as.data.frame(matrix(cor, dimnames=list(t(outer(colnames(cor), rownames(cor), FUN=paste)), NULL)))
    cor.long=rbind(cor.long,t(data.frame(corlong)))
}

# Calculates summary statistics.

# Calculates the mean and standard deviation of cor.long
pmcor=colMeans(cor.long)
ssdcor=apply(cor.long,2,sd)

# Creates our summary matrices for mean and standard deviation and renames the rows and columns.
pmmat=matrix(as.vector(t(pmcor)),ntr,ntr)
ssdmat=matrix(as.vector(t(ssdcor)),ntr,ntr)
colnames(pmmat)=rownames(pmmat)=colnames(fm3$ETA[[2]]$Cov$Omega)
colnames(ssdmat)=rownames(ssdmat)=colnames(fm3$ETA[[2]]$Cov$Omega)

# Used to calculate the phenotypic correlation of our traits. The steps above are repeated for phenotypic variance.
g=fread("eig_CEM_Omega_2.dat")
r=fread("eig_CEM_R.dat")
p=g+r
kg=p[(10000/10+1):nrow(p),]
est=colMeans(kg)
se=apply(kg,2,sd)
ntr=ncol(fm3$ETA[[2]]$u)

mat= matrix(0, ntr, ntr)
mat[lower.tri(mat, diag=T)] <- as.vector(t(kg[1,]))
mat[upper.tri(mat)] <- t(mat)[upper.tri(mat)]
cor=cov2cor(mat)
rownames(cor)=colnames(cor)=seq(1:ntr)
corlong=as.data.frame(matrix(cor, dimnames=list(t(outer(colnames(cor), rownames(cor), FUN=paste)), NULL)))
cor.long=t(data.frame(corlong))

for(i in 2:nrow(kg)){
    mat= matrix(0, ntr, ntr)
    mat[lower.tri(mat, diag=T)] <- as.vector(t(kg[i,]))
    mat[upper.tri(mat)] <- t(mat)[upper.tri(mat)]
    cor=cov2cor(mat)
    rownames(cor)=colnames(cor)=seq(1:ntr)
    corlong=as.data.frame(matrix(cor, dimnames=list(t(outer(colnames(cor), rownames(cor), FUN=paste)), NULL)))
    cor.long=rbind(cor.long,t(data.frame(corlong)))
}
pmcor=colMeans(cor.long)
ssdcor=apply(cor.long,2,sd)
pmmat=matrix(as.vector(t(pmcor)),ntr,ntr)
ssdmat=matrix(as.vector(t(ssdcor)),ntr,ntr)
colnames(pmmat)=rownames(pmmat)=colnames(fm3$ETA[[2]]$Cov$Omega)
colnames(ssdmat)=rownames(ssdmat)=colnames(fm3$ETA[[2]]$Cov$Omega)
```


## Plots
```{r}
# Reads in the files.
cv.mic <- fread("winvar_CV.MIC.csv")
cem <- fread("winvar_CEM.csv")

# Function to create the manhattan plot.
manhattanPlot = function(outFile,threshold=NA){
    plot(outFile$Window,outFile$PCGenVar,col=as.numeric((outFile$Chr)),pch=19,xlab="Window",ylab="% Genetic Variance (Coarse Edge Micron)")
    if(!is.na(threshold)){
        abline(h=threshold)
    }
}

# Creates out manhattan plot for CEM and CV.MIC.
manhattanPlot(cv.mic[which(cv.mic$Chr>0),])
manhattanPlot(cem[which(cem$Chr>0),])

# Creates a plot for our PCA data.
plot(pca$x, col = as.numeric(as.factor(phenotypes$Gen.Line)), pch = 19, xlab="PC1: 17.1%", ylab = "PC2: 5.7%")
legend("bottomleft",legend=c("Merino","Rafter 7", "Unknowns"),pch=15,col=c("black","red", "green"), cex = 0.6)
```

## Run Everything
```{r}
# A blank cell just to run everthying in a single go.
```
