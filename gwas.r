#==========================================================================================================================================#

# Setting Up
if(getwd() != "/Users/andrewhsu/Documents/PREP/S24"){
    setwd("/Users/andrewhsu/Documents/PREP/S24")
}

# Libraries
library("data.table")
library("dplyr")
library("plyr")
library("stringr")
library("zoo")
library(BGLR)

# Files
E23_lamb_reports <- read.csv("Files/Rafter 7 Lambing Reports 2023.csv")
E22_lamb_reports <- read.csv("Files/R7 Lambing Reports 2022.csv")
E21_lamb_reports <- read.csv("Files/R7 Lambing Reports 2021.csv")
E20_lamb_reports <- read.csv("Files/Rafter 7 Lambing Reports 2020.csv")
E12_19_lamb_reports <- read.csv("Files/R7 Lamb Reports 2012-2019.csv")
lamb_card <- read.csv("Files/Lamb Card Data 2023.csv")
sheep_tsu <- read.csv("Files/Sheep_TSU_Master_Sheet_V5.csv")


#==========================================================================================================================================#
#Merging Files
lamb_and_tsu <- merge(lamb_card, sheep_tsu, by.x = "TSU", by.y = "TSU..", all = TRUE)
#==========================================================================================================================================#

# Filtering lamb_and_tsu for paint brand only, sample scanned only, and both
paint_brand_filter <- filter(lamb_and_tsu, !is.na(lamb_and_tsu$Paint.Brand) & is.na(lamb_and_tsu$Sample.Scanned.by.GNZ.LAB))
sample_scanned_filter <- filter(lamb_and_tsu, is.na(lamb_and_tsu$Paint.Brand) & !is.na(lamb_and_tsu$Sample.Scanned.by.GNZ.LAB))
complete_filter <- filter(lamb_and_tsu, !is.na(lamb_and_tsu$Paint.Brand) & !is.na(lamb_and_tsu$Sample.Scanned.by.GNZ.LAB))

#==========================================================================================================================================#

# Splitting up E12_19_lamb_reports into individual years
add_2000 <- function(x){
    strtoi(x) + 2000   
}

dates <- as.Date(E12_19_lamb_reports$Lambing.Dates, origin = "1900-01-01")
strYY <- substring(dates, 1, 2)
intYY <- lapply(strYY, strtoi)
E12_19_lamb_reports$Years <- lapply(intYY, add_2000)
unique_years <- unique(E12_19_lamb_reports$Years)

list_of_years <- list()
for(year in unique_years){
    year_df <- E12_19_lamb_reports[E12_19_lamb_reports$Years == year, ]
    list_of_years[[as.character(year)]] <- year_df
}

for(i in 1:9){
    name <- paste("df", unique_years[[i]], sep="_")
    assign(name, as.data.frame(list_of_years[i]))
}

df_2012 <- unique(df_2012)
df_2013 <- unique(df_2013)
df_2014 <- unique(df_2014)
df_2015 <- unique(df_2015)
df_2016 <- unique(df_2016)
df_2017 <- unique(df_2017)
df_2018 <- unique(df_2018)
df_2019 <- unique(df_2019)


#==========================================================================================================================================#

# Determine single, twin, triplet status for 2012-2019
lamb_card_eids <- unique(lamb_card$Ewe.ID)
for(i in 1:9){
    name <- paste("lamb_report_eids", unique_years[[i]], sep="_")
    df <- get(paste("df", unique_years[[i]], sep="_"))
    column_lamb <- paste("X", unique_years[[i]], ".Lamb.Ear.Tag..", sep="")
    column_ewe <-paste("X", unique_years[[i]], ".Ewe.Ear.Tag..", sep="")
    similar_name <- paste("similar", unique_years[[i]], sep="_")
    progeny_name <- paste("progeny", unique_years[[i]], sep="_")
    assign(name, unique(df[[column_lamb]]))
    assign(similar_name, lamb_card_eids[name %in% lamb_card_eids])
    if(column_lamb != "XNA.Lamb.Ear.Tag.."){
        assign(progeny_name, aggregate(df[[column_lamb]]~df[[column_ewe]], df, FUN = length))   
    }

}
#==========================================================================================================================================#

# Determine single, twin, triplet status for 2020-2023
lamb_card_eids <- unique(lamb_card$Ewe.ID)

lamb_report_eids_2020 <- unique(E20_lamb_reports$Lamb.Ear.Tag..)
similar_2020 <- lamb_card_eids[lamb_report_eids_2020 %in% lamb_card_eids]
progeny_2020 <- aggregate(E20_lamb_reports$Lamb.Ear.Tag..~E20_lamb_reports$Ewe.Ear.Tag.., E20_lamb_reports, FUN = length)

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

#==========================================================================================================================================#

# Merge progeny with associated lambing report and then add on the age of the ewe

E23_lamb_reports <- unique(E23_lamb_reports)
full_report_2023 <- merge(E23_lamb_reports, progeny_2023, by.x = "Ewe..Ear.tag...", by.y = "E23_lamb_reports$Ewe..Ear.tag...", all = TRUE)
full_report_2023 <- unique(full_report_2023)

full_report_2022 <- merge(E22_lamb_reports, progeny_2022, by.x = "Dam.ear.tag", by.y = "E22_lamb_reports$Dam.ear.tag")
full_report_2021 <-merge(E21_lamb_reports, progeny_2021, by.x = "Dam.ear.tag", by.y = "E21_lamb_reports$Dam.ear.tag")
full_report_2020 <- merge(E20_lamb_reports, progeny_2020, by.x = "Ewe.Ear.Tag..", by.y = "E20_lamb_reports$Ewe.Ear.Tag..")

for(i in c(1, 3:9)){
    name <- paste("full_report", unique_years[[i]], sep="_")
    if(unique_years[[i]]){
     progeny <- get(paste("progeny", unique_years[[i]], sep="_"))
    }
    df <- get(paste("df", unique_years[[i]], sep="_"))
    df_name <- paste("df", unique_years[[i]], sep="_")
    column_x = paste("X", unique_years[[i]], ".Ewe.Ear.Tag..", sep="")
    column_y <- "df[[column_ewe]]"
    assign(name, merge(df, progeny, by.x = column_x, by.y = column_y))
}



#==========================================================================================================================================#

# Determine the age of the ewe 2012 - 2019
year_data <- vector("list", 13)
for(i in c(1, 3:9)){
    counter <- 1
    report_name <- get(paste("full_report", unique_years[[i]], sep="_"))
    column <- paste("X", unique_years[[i]], ".Ewe.Ear.Tag..", sep="")
    year_data[[i]] <- list(
        yob = list(),
        genline = list(),
        identifier = list(),
        value = list()
    )
    for(j in 1:length(report_name[[column]])){
        value <- report_name[[column]][[j]]
        clength <- nchar(value)
        
        if(clength == 5 && substring(value, 1, 1) != 'A'){
            yob_value = substring(value, 1, 1)
            genline_value = substring(value, 2, 2)
            identifier_value = substring(value, 3, 5)
            
            year_data[[i]]$yob[[counter]] <- yob_value
            year_data[[i]]$genline[[counter]] <- genline_value
            year_data[[i]]$identifier[[counter]] <- identifier_value
            year_data[[i]]$eid[[counter]] <- value

            birth_year <- as.numeric(yob_value) + 2010
            year_data[[i]]$year[[counter]] <- substring(birth_year, 3, 4)
            
            if(birth_year > unique_years[[i]]){
                birth_year <- birth_year - 10
            }
            
            age <- unique_years[[i]] - birth_year
            year_data[[i]][["age"]][[counter]] <- age

            counter <- counter + 1

            
        }
    }
}
#==========================================================================================================================================#

# 2023
yob_numeric_2023 <- list()
counter <- 1
for(i in 1:length(progeny_2023$`E23_lamb_reports$Ewe..Ear.tag...`)){
    value <- progeny_2023$`E23_lamb_reports$Ewe..Ear.tag...`[[i]]
    clength <- nchar(value)
    if(clength == 7){
        yob_value <- substring(value, 1, 2)
        genline_value <- substring(value, 3, 3)
        identifier_value <- substring(value, 4, 7)
 
        year_data[[10]]$yob[[counter]] <- yob_value
        year_data[[10]]$genline[[counter]] <- genline_value
        year_data[[10]]$identifier[[counter]] <- identifier_value
        year_data[[10]]$eid[[counter]] <- value
        
        age <- as.numeric(yob_value) + 2000
        year_data[[10]]$year[[counter]] <- substring(age, 3, 4)
        
        yob_numeric_2023[[counter]] <- 2023 - age

        counter <- counter + 1
    }
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

#==========================================================================================================================================#
# Merge full_report with age 2023

eid_2023 <- unlist(year_data[[10]]$eid)
merged_2023_data <- data.frame(age = unlist(yob_numeric_2023), eid = eid_2023)
E_2023_data <- merge(full_report_2023, merged_2023_data, by.x = "Ewe..Ear.tag...", by.y = "eid", all = TRUE)
colnames(E_2023_data)[colnames(E_2023_data) == "E23_lamb_reports$Lamb.Ear.Tag.."] <- "# of Lambs Birthed"


#==========================================================================================================================================#

# 2022
yob_numeric_2022 <- list()
counter <- 1
for(i in 1:length(progeny_2022$`E22_lamb_reports$Dam.ear.tag`)){
    value <- progeny_2022$`E22_lamb_reports$Dam.ear.tag`[[i]]
    clength <- nchar(value)
    if(clength == 7){
        yob_value <- substring(value, 1, 2)
        genline_value <- substring(value, 3, 3)
        identifier_value <- substring(value, 4, 7)

        year_data[[11]]$yob[[counter]] <- yob_value
        year_data[[11]]$genline[[counter]] <- genline_value
        year_data[[11]]$identifier[[counter]] <- identifier_value
        year_data[[11]]$eid[[counter]] <- value
        
        age <- as.numeric(yob_value) + 2000
        year_data[[11]]$year[[counter]] <- substring(age, 3, 4)
        
        yob_numeric_2022[[counter]] <- 2022 - age

        counter <- counter + 1
    }
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
#==========================================================================================================================================#

# Merge full_report with age 2022

eid_2022 <- unlist(year_data[[11]]$eid)
merged_2022_data <- data.frame(age = unlist(yob_numeric_2022), eid = eid_2022)
E_2022_data <- merge(full_report_2022, merged_2022_data, by.x = "Dam.ear.tag", by.y = "eid", all = TRUE)
colnames(E_2022_data)[colnames(E_2022_data) == "E22_lamb_reports$Lamb.Ear.Tag.."] <- "# of Lambs Birthed"

#==========================================================================================================================================#

progeny_2021 <- progeny_2021[-c(1415:1416), ]
# 2021
yob_numeric_2021 <- list()
counter <- 1
for(i in 1:length(progeny_2021$`E21_lamb_reports$Dam.ear.tag`)){
    value <- progeny_2021$`E21_lamb_reports$Dam.ear.tag`[[i]]
    clength <- nchar(value)
    if(clength == 7){
        yob_value <- substring(value, 1, 2)
        genline_value <- substring(value, 3, 3)
        identifier_value <- substring(value, 4, 7)
   
        year_data[[12]]$yob[[counter]] <- yob_value
        year_data[[12]]$genline[[counter]] <- genline_value
        year_data[[12]]$identifier[[counter]] <- identifier_value
        year_data[[12]]$eid[[counter]] <- value
        
        age <- as.numeric(yob_value) + 2000
        year_data[[12]]$year[[counter]] <- substring(age, 3, 4)
        
        yob_numeric_2021[[counter]] <- 2021 - age
   
        counter <- counter + 1
    }
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
#==========================================================================================================================================#

# Merge full_report with age 2021

eid_2021 <- unlist(year_data[[12]]$eid)
merged_2021_data <- data.frame(age = unlist(yob_numeric_2021), eid = eid_2021)
E_2021_data <- merge(full_report_2021, merged_2021_data, by.x = "Dam.ear.tag", by.y = "eid", all = TRUE)
colnames(E_2021_data)[colnames(E_2021_data) == "E21_lamb_reports$Lamb.Ear.Tag.."] <- "# of Lambs Birthed"

#==========================================================================================================================================#

progeny_2020 <- progeny_2020[-c(1511:1518),]
# 2020
yob_numeric_2020 <- list()
counter <- 1
for(i in 1:length(progeny_2020$`E20_lamb_reports$Ewe.Ear.Tag..`)){
    value <- progeny_2020$`E20_lamb_reports$Ewe.Ear.Tag..`[[i]]
    clength <- nchar(value)
    if(clength == 7){
        yob_value <- substring(value, 1, 2)
        genline_value <- substring(value, 3, 3)
        identifier_value <- substring(value, 4, 7)

        year_data[[13]]$yob[[counter]] <- yob_value
        year_data[[13]]$genline[[counter]] <- genline_value
        year_data[[13]]$identifier[[counter]] <- identifier_value
        year_data[[13]]$eid[[counter]] <- value
        
        age <- as.numeric(yob_value) + 2000
        year_data[[13]]$year[[counter]] <- substring(age, 3, 4)
        
        yob_numeric_2020[[counter]] <- 2020 - age
        
        counter <- counter + 1
    }
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
#==========================================================================================================================================#

# Merge full_report with age 2020

eid_2020 <- unlist(year_data[[13]]$eid)
merged_2020_data <- data.frame(age = unlist(yob_numeric_2020), eid = eid_2020)
E_2020_data <- merge(full_report_2020, merged_2020_data, by.x = "Ewe.Ear.Tag..", by.y = "eid", all = TRUE)
colnames(E_2020_data)[colnames(E_2020_data) == "E20_lamb_reports$Lamb.Ear.Tag.."] <- "# of Lambs Birthed"
#==========================================================================================================================================#

# Merge full_report with age

for(i in c(1, 3:9)){
    eid <- unlist(year_data[[i]]$eid)
    name <- paste("merged", unique_years[[i]], "data", sep="_")
    assign(name, data.frame(age = unlist(year_data[[i]]$age),eid = eid))
    report <- paste("full_report", unique_years[[i]], sep="_")
    data <- paste("E",unique_years[[i]], "data", sep="_")
    x <- paste("X", unique_years[[i]], ".Ewe.Ear.Tag..", sep="")
    assign(data, merge(get(report), get(name), by.x = x, by.y = "eid", all = TRUE))
}

for(i in c(1, 3:9)){
    data_name <- get(paste("E", unique_years[[i]], "data", sep="_"))
    y <- paste("X", unique_years[[i]], ".Ewe.Ear.Tag..", sep="")
    distinct(data_name, get(y), .keep_all = TRUE)
}

#==========================================================================================================================================#

# combine all E reports into one

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
#==========================================================================================================================================#

# fix final merge dataframe
# make all the columns the same name and rbind.fill them

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

fwrite(final, "final.csv")
#==========================================================================================================================================#
# merge sample id and tsu with given files (email)
# convert uid tagsx to ewe ids or vice versa
# 456/12_MER -> 123456 or other way around

# 1 MER
# 5 RAMB
# 9 UNKN
# 7 AI MER

lamb_card_2023 <- read.csv("Files/Lamb Card Data 2023[95].csv")
sample_table <- read.csv("Files/s11645Samples Table.csv")
samples_table_2 <- read.csv("Files/Samples Table.csv")

card_and_table <- merge(lamb_card_2023, sample_table, by.x = "TSU", by.y = "UIDTag", all = TRUE)

num_to_breed <- list(
    "1" = "MER",
    "5" = "RAMB",
    "9" = "UNKN",
    "7" = "MER"
)

# Converts EIDs to UIDs
tags <- list(
    uids <- list(),
    eids <- list()
)

counter_tags <- 1
for(i in c(1,3:13)){
    counter <- 1
    for(j in 1:length(year_data[[i]]$yob)){
        if(substring(year_data[[i]]$eid[[j]], 1, 1) != 'A' ){
            year_value <- year_data[[i]]$year[[j]]
            genline_value <- year_data[[i]]$genline[[j]]
            identifier_value <- year_data[[i]]$identifier[[j]]
    
            breed <- num_to_breed[[genline_value]]
            
            uid <- paste(identifier_value, "/", year_value, "_", breed, sep="")
            
            tags$uids[[counter_tags]] <- uid
            tags$eids[[counter_tags]] <- year_data[[i]]$eid[[counter]]
            counter <- counter + 1
            counter_tags <- counter_tags + 1

        }
    }
}

# Combine files to attach the UIDs to dataframe
tag_df <- data.frame(uid = unlist(tags$uids), eid = unlist(tags$eids))
lamb_card_tag <- merge(lamb_card_2023, tag_df, by.x = "Ewe.ID", by.y = "eid", all = TRUE)

# card_and_table is a merge of lamb_card and sample_table on TSU
card_and_table_tag <- merge(card_and_table, tag_df, by.x = "Ewe.ID", by.y = "eid", all = TRUE)
card_and_table_tag <- unique(card_and_table_tag)

# Filters dataframes to remove unnecessary information
test <- filter(card_and_table_tag, !is.na(card_and_table_tag$TSU) & !is.na(card_and_table_tag$uid))
test_test <- subset(test, select = -c(AnimalID, Stud, YOB, Breed, Species, SampleID, Plate_ID, Manifest, ControlPos, Index, Sample.ID, Call.Rate, Gender, Gender.Est, Comment, Lamb.3.ID))
sample_and_tst <- merge(sample_table, test_test, by.x = "SampleID", by.y = "TSU", all = TRUE)
sample_and_tst <- filter(sample_and_tst, !is.na(sample_and_tst$Ewe.ID) & !is.na(sample_and_tst$AnimalID) & sample_and_tst$YOB != 0)

# Adding a UID column to samples_table
samples_table_2$uid <- paste(samples_table_2$UIDTag,"MER",sep="_")
final_2 <- rbind.fill(samples_table_2, sample_and_tst)

# Fixing UIDs for tables_combined and merging with test_test (filtered dataframe containing information from lamb_card, sample_table, and tag_df)
tables_combined <- rbind.fill(sample_table, samples_table_2)
tables_combined$uid[1:1577] <- tables_combined$UIDTag[1:1577]
tables_and_samples <- merge(test_test, tables_combined, by.x = "TSU", by.y = "SampleID")
 #==========================================================================================================================================#

# Merging some files together
merino_ewes <- read.csv("Files/Merino Ewes 1-11-24-689.csv")
R7_ewes <- read.csv("Files/R7 Ewes 12-23_fix_650.csv")
problem_tsus <- fread("Files/Problem_TSUs.csv")

# Combining these two files together and fixing small errors in data
merged <- rbind.fill(merino_ewes, R7_ewes)
merged <- unique(merged)
merged <- merged[-c(1168), ]
merged$Animal.Eartag[120] <- "211072"
merged$Animal.Eartag[73] <- "201682"
merged$Animal.Eartag[158] <- "5695"
merged$Animal.Eartag[164] <- "71729"
merged$Animal.Eartag[73] <- "211223"
merged$Animal.Eartag[63] <- "2210819"
merged$Animal.Eartag[304] <- "211223"
merged$Animal.Eartag=as.character(as.numeric(merged$Animal.Eartag))


file <- merge(merged, sample_and_tst, by.x = "Animal.Eartag", by.y = "Ewe.ID")
file <- file[-which(duplicated(file$Animal.Eartag)),]

fwrite(file, "file.csv")

merging <- merge(sample_and_tst, final, by.x = "Ewe.ID", by.y = "Ewe.Ear.Tag")
merging <- merging[-which(duplicated(merging$Ewe.ID) == TRUE),]
ids=merging[,1:2]

# only getting 10 rows for birth_merge
# no idea why we have these data frames when this is the only time they're used

# birth_merge <- merge(ids, merging, by.x = "Ewe.ID", by.y = "Lamb.Ear.Tag")
# final_ids_merge <- merge(ids, final, by.x = "Ewe.ID", by.y = "Lamb.Ear.Tag", all.x = TRUE)
# final_ids_merge <- unique(final_ids_merge)
# 
# final_ids_merge_test <-final_ids_merge[-which(is.na(final_ids_merge$Ewe.Ear.Tag)),]
# 
# meta_combination <- merge(final_2, merged, by.x = "Ewe.ID", by.y = "Animal.Eartag", all = TRUE)
# # meta_both <- merge(final_2, merged, by.x = "uid", by.y = "Animal.Eartag")
# # meta_both <- unique(meta_both)

#merged <- merged[-which(is.na(merged$Animal.Eartag)),]
#file.remove("output.txt")

# Converting EIDs to UIDs
new_tags <- list(
    uids <- list(),
    eids <- list()
)
new_counter_tags <- 1
for(i in 1:length(merged$Animal.Eartag)){
    if(!is.na(merged$Animal.Eartag[[i]]) & substring(merged$Animal.Eartag[[i]],1,1) == 2){
        year_value <- substring(merged$Animal.Eartag[[i]],1,2)
        genline_value <- substring(merged$Animal.Eartag[[i]],3,3)
        identifier_value <- substring(merged$Animal.Eartag[[i]],4, 7)
        
        
        breed <- num_to_breed[[genline_value]]
        uid <- paste(identifier_value, "/", as.character(year_value), "_", breed, sep="")
        new_tags$uids[[new_counter_tags]] <- uid
        new_tags$eids[[new_counter_tags]] <- merged$Animal.Eartag[[i]]

        new_counter_tags <- new_counter_tags + 1
    }
    else if(!is.na(merged$Animal.Eartag[[i]])){
        year_value <- substring(merged$Animal.Eartag[[i]],1,1)
        genline_value <- substring(merged$Animal.Eartag[[i]],2,2)
        identifier_value <- substring(merged$Animal.Eartag[[i]],3, 7)
        
        
        year_value <- as.numeric(year_value) + 10
        
        breed <- num_to_breed[[genline_value]]
        uid <- paste(identifier_value, "/", as.character(year_value), "_", breed, sep="")
        new_tags$uids[[new_counter_tags]] <- uid
        new_tags$eids[[new_counter_tags]] <- merged$Animal.Eartag[[i]]
        
        new_counter_tags <- new_counter_tags + 1
    }
    
}

new_tag_df <- data.frame(uid = unlist(new_tags$uids), eid = unlist(new_tags$eids))

# Combining UID information with information from r7 and merino ewe data frames
merged_and_tags <- merge(merged, new_tag_df, by.x = "Animal.Eartag", by.y = "eid", all = TRUE)
merged_and_tags <- unique(merged_and_tags)

merged_and_tags <- merged_and_tags[-c(1:2),]

# I don't know why we have final_3, I think this was used for debugging purposes
# final_3 <- merge(final_2, merged_and_tags, by.x = "uid", by.y = "uid")
# final_3 <- unique(final_3)
# 
# final_3 <- final_3[-which(duplicated(final_3$uid)),]
# 
# final_3 <- final_3[-which(final_3$SampleID %in% problem_tsus$`TSU#`),]
# 
# final_3_complete <- final_3[-which(is.na(final_3$AnimalID)),]
# final_3_incomplete <- final_3[which(is.na(final_3$AnimalID)),]
# 
# fwrite(final_3_complete, "final_3_complete.csv")
# fwrite(tables_combined, "tables_combined.csv")
# fwrite(merged_and_tags, "merged_and_tags.csv")
#==========================================================================================================================================#
if(getwd() != "/Users/andrewhsu/Documents/PREP/S24"){
    setwd("/Users/andrewhsu/Documents/PREP/S24")
}

library("data.table")
library("stringr")

#genotypes <- fread("Files/output.txt")
file <- fread("Files/file.csv")
final_3_complete <- fread("Files/final_3_complete.csv")
tables_combined <- fread("Files/tables_combined.csv")
merged_and_tags <- fread("Files/merged_and_tags.csv")
problem_tsus <- fread("Files/Problem_TSUs.csv")
final <- fread("Files/final.csv")

# split V1 into just TSU numbers and merge
#names <- genotypes[,1]
#names_split <-sapply(strsplit(names$V1,"_"),"[",3)


#genotypes$V1 <- names_split

# combine genotypes with lambing records and then with phenotypes files
genotypes_and_phenotypes <- merge(tables_combined, merged_and_tags, by.x = "uid", by.y = "uid")
genotypes_and_phenotypes <- genotypes_and_phenotypes[-which(genotypes_and_phenotypes$SampleID %in% problem_tsus$`TSU#`),]
genotypes_and_phenotypes <- genotypes_and_phenotypes[-which(duplicated(genotypes_and_phenotypes$uid)),]

genotypes_and_phenotypes$Animal.Eartag <- as.character(genotypes_and_phenotypes$Animal.Eartag)

# final is the dataframe with all lambing records spanning from 2012-2023
# combine genotypes_and_phenotypes with final to get the metadata
gp_and_lambing_records <- merge(genotypes_and_phenotypes, final, by.x = "Animal.Eartag", by.y = "Ewe.Ear.Tag")
gp_and_lambing_records <- unique(gp_and_lambing_records)


# Getting the breed of the animal
num_to_breed <- list(
    "1" = "MER",
    "5" = "RAMB",
    "9" = "UNKN",
    "7" = "MER"
)

breed_list <- list(
    breed <- list(),
    eid <- list()
)
counter <- 1
for(i in 1:length(gp_and_lambing_records$Animal.Eartag)){
    if(substring(gp_and_lambing_records$Animal.Eartag[[i]], 1, 1) == '2'){
        number <- str_sub(gp_and_lambing_records$Animal.Eartag[[i]], 3, 3)
        breedline <- num_to_breed[[number]]
        breed_list$breed[[counter]] <- breedline
        breed_list$eid[[counter]] <- gp_and_lambing_records$Animal.Eartag[[i]]
        
        counter <- counter + 1
    }
    else{
        number <- str_sub(gp_and_lambing_records$Animal.Eartag[[i]],2,2)
        breedline <- num_to_breed[[number]]
        breed_list$breed[[counter]] <- breedline
        breed_list$eid[[counter]] <- gp_and_lambing_records$Animal.Eartag[[i]]
        
        counter <- counter + 1
        
    }
}

breed_list <- data.frame(Gen.Line = unlist(breed_list$breed), Ewe.Id = unlist(breed_list$eid))

gp_and_lambing_records <- unique(gp_and_lambing_records)
breed_list <- unique(breed_list)

# Adding a column containing the breed information to our metadata data frame
gp_and_lambing_records_test <- merge(gp_and_lambing_records, breed_list, by.x = "Animal.Eartag", by.y = "Ewe.Id")

# Filtering, but I don't remember why we filtered it the way we did
filtered_df <- gp_and_lambing_records_test[which(gp_and_lambing_records_test$Years == '2023'),]
filtered_df <- unique(filtered_df)
dupnames=unique(filtered_df[which(duplicated(filtered_df$Animal.Eartag)),"Animal.Eartag"])
filtered_df[which(filtered_df$Animal.Eartag %in% dupnames),c(1:3,60:70)]
filtered_df <- filtered_df[, -which(names(filtered_df) %in% c("Sex","Lamb.Ear.Tag","Comments","Lambing.Dates","Ewe.Paint.Brand"))]

filtered_df <- unique(filtered_df)

filtered_dupnames <- unique(filtered_df[which(duplicated(filtered_df$Animal.Eartag)), "Animal.Eartag"])
filtered_df[which(filtered_df$Animal.Eartag %in% filtered_dupnames),]
final$Lamb.Ear.Tag <- gsub('-','',final$Lamb.Ear.Tag)


# Removing duplicated entries and fixing formatting of data
#gp_and_lambing_records_test <- merge(final, filtered_df, by.x = "Lamb.Ear.Tag", by.y = "Animal.Eartag", all.y = TRUE)

lamb_dupnames <- unique(gp_and_lambing_records_test[which(duplicated(gp_and_lambing_records_test$Lamb.Ear.Tag)), "Lamb.Ear.Tag"])
gp_and_lambing_records_test[which(gp_and_lambing_records_test$Lamb.Ear.Tag %in% lamb_dupnames),]

gp_and_lambing_records_test <- gp_and_lambing_records_test[-which(gp_and_lambing_records_test$Lamb.Ear.Tag %in% lamb_dupnames),]


gp_and_lambing_records_test$Lamb.Ear.Tag <- gsub("-", "", gp_and_lambing_records_test$Lamb.Ear.Tag)

# gets number of lambs associated with each ewe
lambs_list <- list(
    num_of_lambs <- list(),
    eid <- list(),
    in_or_not <- list()
)
counter <- 1
for(i in 1:length(gp_and_lambing_records_test$Ewe.Ear.Tag)){
    u=final[which(final$Ewe.Ear.Tag==gp_and_lambing_records_test$Animal.Eartag[[i]]  & final$Years==gp_and_lambing_records_test$Years.x[[i]]),]
    lambs_list$num_of_lambs[[counter]] <- nrow(u) 
    lambs_list$eid[[counter]] <- gp_and_lambing_records_test$Ewe.Ear.Tag[[i]]
    lambs_list$in_or_not[[counter]] <- gp_and_lambing_records_test$Lamb.Ear.Tag[[i+1]] %in% u$Lamb.Ear.Tag
    counter <- counter + 1
}

lambs_list <- data.frame(Num = unlist(lambs_list$num_of_lambs), Ewe.ID = unlist(lambs_list$eid), In = unlist(lambs_list$in_or_not))

gp_and_lambing_records_test <- as.data.frame(gp_and_lambing_records_test)


gp_and_lambing_records_test$Years.x[unlist(lapply(gp_and_lambing_records_test$Years.x, is.null))] <- NA

# Commented out because it wasn't working as intended
#gpl_and_records <- merge(gp_and_lambing_records_test, final, by.x = "Lamb.Ear.Tag", by.y = "Lamb.Ear.Tag")

# Adding number of lambs birthed from each ewe to our metadata
gp_and_lambing_records_list <- merge(gp_and_lambing_records_test, lambs_list, by.x = "Ewe.Ear.Tag", by.y = "Ewe.ID")
gp_and_lambing_records_list <- gp_and_lambing_records_list[-which(gp_and_lambing_records_list$In == "FALSE"), ]

gp_and_lambing_records_list <- unique(gp_and_lambing_records_list)


snp_genotypes <- fread("Files/snp_genotypes.csv")
snp_info_from_linux <- fread("Files/output.txt")

names(snp_info_from_linux)[names(snp_info_from_linux) == 'V1'] <- 'ID'

snps <- rbind(snp_genotypes, snp_info_from_linux)

animals_from_2023 <- final[which(final$Years == '2023'),]

# Creating management groups
animals_from_2023 <- animals_from_2023[order(as.Date(animals_from_2023$Lambing.Dates, format="%m/%d/%Y")),]
animals_from_2023$Management.Group <- 0
animals_from_2023$Management.Group[c(767:1532)] <- 1
animals_from_2023$Management.Group[c(1533:2299)] <- 2

# Adding on management group information to metadata
gp_and_lambing_records_test <- merge(gp_and_lambing_records_test, animals_from_2023, by.x = "Lamb.Ear.Tag", by.y = "Ewe.Ear.Tag")
gp_and_lambing_records_test <- unique(gp_and_lambing_records_test)
gp_and_lambing_records_test <- gp_and_lambing_records_test[-which(duplicated(gp_and_lambing_records_test$Lamb.Ear.Tag)),]

gp_and_lambing_records_test <- gp_and_lambing_records_test[-which(is.na(gp_and_lambing_records_test$Ewe.Ear.Tag)),]

colnames_keep <- c("Lamb.Ear.Tag", "Sample.ID", "Gen.Line", "Management.Group", "Lambs.Birthed.x", "Age.x", "Age.y", "Lambs.Birthed.y", "CEM", "Mic.Ave", "CV.Mic", "SF.Mic", "SL.mm")
gp_and_lambing_records_test <- gp_and_lambing_records_test[, colnames_keep]

gp_and_lambing_records_test <- gp_and_lambing_records_test[-which(is.na(gp_and_lambing_records_test$Age.x)),]

gpl_and_snps <- merge(gp_and_lambing_records_test, snps, by.x = "Sample.ID", by.y = "ID")
#==========================================================================================================================================#

# Analysis

# remove markers on chromosome x, 29, 0, or mt
#gpl_and_snps <- fread("dataframe.csv")

file <- fread("file.csv")
file <- as.data.frame(file)

µcn = which(colnames(gpl_and_snps) == "SL.mm")
phenotypes = gpl_and_snps[,1:cn]
genotypes = gpl_and_snps[,(cn+1):ncol(gpl_and_snps)]

map<-fread("Files/snp_info_file_[AgR_Ovine_60Kplus_20007095X378523_A1]_[6559].txt")

genotypes <- genotypes[,-which(map$Chromosome_No == 'X' | map$Chromosome_No == '29' | map$Chromosome_No == '0' | map$Chromosome_No == 'MT')]


miss = is.na(genotypes)
snp_cr = (1 - colMeans(miss))
animal_cr = (1 - rowMeans(miss))

gf1 = genotypes[, -which(snp_cr < 0.95)]
af = colMeans(gf1, na.rm=TRUE)/2
gf2 = gf1[, -which(af > 0.95 | af < 0.05)]

genotypeCounts = function(genotypes){
    t(apply(genotypes,2,function(d){c(AA=sum(d==0),AB=sum(d==1),BB=sum(d==2))}))
}

gf3 = na.aggregate(gf2)

gf5 = scale(gf3)
#gf5 = gf5[,-which(is.na(colMeans(gf5)))]

X = gf5/sqrt(ncol(gf5))
G = tcrossprod(X)
pca <- prcomp(G)
per <- 100*pca$sdev^2/sum(pca$sdev^2)

# include in poster (b = mer, r = r7, g = unknws)
# present heritability as a table (same as first one)
# phenotypic and genetic correlations in one table (phenotypics above diag, genotypics below diag, sd below value)
# 

design.mat = model.matrix(~as.factor(phenotypes$Age.x)+as.factor(phenotypes$Age.y)+as.factor(phenotypes$Gen.Line)+as.factor(phenotypes$Management.Group)+as.factor(phenotypes$Lambs.Birthed.x)+as.factor(phenotypes$Lambs.Birthed.y))

# Heritability
set.seed(123)
fm1=BGLR(y=unlist(phenotypes$CEM),ETA=list(list(X=design.mat, model="FIXED"),list(K=G,model='RKHS')),R2=0.44,nIter=60000,burnIn=10000,verbose=T,saveAt='eig_CEM_')
varE=scan( 'Heritability/eig_CEM_varE.dat')
varU=scan('Heritability/eig_CEM_ETA_2_varU.dat')
h2=varU[-c(1:2000)]/(varU[-c(1:2000)]+varE[-c(1:2000)])
mean(h2) #SL.mm: 0.1563923, SF.Mic: 0.4039165, CV.Mic: 0.4261906, Mic.Ave: 0.4422688, CEM: 0.3642834 
sd(h2) #SL.mm: 0.0424883, SF.Mic: 0.08037725, CV.Mic: 0.07915623, Mic.Ave: 0.08195741, CEM: 0.07637816

getSNPWindows = function(map){
    map=map[order(as.numeric(map$Chromosome_No),map$Basepair_Position),]
    map$win = floor(map$Basepair_Position/1000000)
    map$ChrWin = paste(map$Chromosome_No,map$win,sep="_")
    w = as.data.frame(cbind(ChrWin = map$ChrWin[!duplicated(map$ChrWin)],Window = 1:length(unique(map$ChrWin))))
    m = merge(map,w)
    map = m[,c("SNP_Name","Chromosome_No","Basepair_Position","Window")]
    map
}

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

set.seed(123)
fm2=BGLR(y=unlist(phenotypes$CEM),ETA=list(list(X=design.mat, model="FIXED"),list(X=X,model='BayesB',probIn=0.01,saveEffects=T)),R2=0.298593,nIter=60000,burnIn=10000,verbose=T,saveAt='BayesB_CEM_')
winvar=getWindowVariances(fm2,X,map)
winvar=winvar[order(winvar$PCGenVar,decreasing = T),]
file <- fwrite(winvar, file="winvar_CEM.csv")

# multi-trait
set.seed(123)
y2 = as.matrix(phenotypes[,9:13])
fm3 <- Multitrait(y=y2 ,ETA=list(list(X=design.mat, model="FIXED"),list(K=G,model='RKHS')),nIter=60000,burnIn=10000,verbose=T,saveAt='eig_CEM_')

###Genetic Correlations###
fm3$ETA[[2]]$Cov
g=fread("Heritability/eig_CEM_Omega_2.dat")
kg=g[(10000/10+1):nrow(g),]
est=colMeans(kg)
se=apply(g,2,sd)
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

###Phenotypic Correlations###
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

#==========================================================================================================================================#
# Genetic Variance
CEM_varU <- fread("eig_CEM_ETA_2_varU.dat")
mean(CEM_varU$V1) # SL.mm: 132.732, SF.Mic: 0.6168857, CV.Mic: 1.586454, Mic.Ave: 0.7586942, CEM: 0.2971568
sd(CEM_varU$V1) # SL.mm: 38.1 SF.Mic: 0.1409919, CV.Mic: 0.3423286, Mic.Ave: 0.1644307, CEM: 0.07029766

# Phenotypic Variance
CEM_varE <- fread("eig_CEM_varE.dat")
mean(CEM_varE$V1) #SL.mm: 720.3713, SF.Mic: 0.9058645, CV.Mic: 2.118772, Mic.Ave: 0.9518064, CEM: 0.5155126
sd(CEM_varE$V1) #SL.mm: 50.06066, SF.Mic: 0.1151572, CV.Mic: 0.2766281, Mic.Ave: 0.1299085, CEM: 0.06013003

CEM.varU <- as.numeric(readLines("eig_CEM_ETA_2_varU.dat"))
CEM.varE <- as.numeric(readLines("eig_CEM_varE.dat"))
CEM_varP <- CEM.varU + CEM.varE

mean(CEM_varP) #SL.MM: 853.1033, SF.Mic: 1.52275, CV.Mic: 3.705227, Mic.Ave: 1.710501, CEM: 0.8126694
sd(CEM_varP) #SL.MM: 44.71568, SF.Mic: 0.0863687, CV.Mic: 0.2102663, Mic.Ave: 0.09841123, CEM: 0.04521265

# Plot
winvar_SL <- fread("winvar_Sl.mm.csv")
manhattanPlot = function(outFile,threshold=NA){
    plot(outFile$Window,outFile$PCGenVar,col=as.numeric((outFile$Chr)),pch=19,xlab="Window",ylab="% Genetic Variance (Coarse Edge Micron)")
    if(!is.na(threshold)){
        abline(h=threshold)
    }
}

manhattanPlot(cv.mic[which(cv.mic$Chr>0),])
manhattanPlot(cem[which(cem$Chr>0),])
plot(pca$x, col = as.numeric(as.factor(phenotypes$Gen.Line)), pch = 19, xlab="PC1: 17.1%", ylab = "PC2: 5.7%")
legend("bottomleft",legend=c("Merino","Rafter 7", "Unknowns"),pch=15,col=c("black","red", "green"), cex = 0.6)

#==========================================================================================================================================#

