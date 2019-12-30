mypacks <- c("plotly", "dplyr", "stringr", "shiny", "rsconnect")  # what packages are needed?
packs <- installed.packages()   # find installed package list
install.me <- mypacks[!(mypacks %in% packs[,"Package"])]  #what needs to be installed?
if (length(install.me) >= 1) install.packages(install.me, repos = "http://cran.us.r-project.org")   # install (if needed)
lapply(mypacks[2:3], library, character.only=TRUE)  # load all packages

firstyear <- as.numeric(readline(prompt="Enter the starting year: ")) # first year of the data
lastyear <- as.numeric(readline(prompt="Enter the ending year: ")) # last year of the data

# open the files in a specified folder
open_func <- function(name) {
  filepath <- list.files(name, pattern="*.csv", full.names=TRUE)
  begin <- which(str_detect(filepath, as.character(firstyear)))
  end <- which(str_detect(filepath, as.character(lastyear)))
  file <- lapply(filepath[begin:end], read.csv, skip = 1)

  return(file)
} 

# function transposing the data frame
transpose_func <- function(df){
  df <- as.data.frame(t(as.matrix(df)))
  colnames(df) <- c("Tract1255", "Tract1256")
  df$Tract1255 <- as.numeric(as.character(df$Tract1255))
  df$Tract1256 <- as.numeric(as.character(df$Tract1256))
  
  return(df)
}

est_moe_percent_func <- function(df){
  # detect estimate values (omit margin of error, etc.)  
  est <- which(str_detect(colnames(df), "Estimate"))
  df <- df[,c(2,est)]
  
  # omit percent estimate and margin of error
  percent <- which(str_detect(colnames(df),"Percent.Estimate.."))
  percent2 <- which(str_detect(colnames(df),"Percent..Estimate.."))
  moe <- which(str_detect(colnames(df),"Margin.of.Error.."))
  df <- df %>% select(-c(percent,percent2, moe))
  
  return(df)
}

# function cleaning up the education data 
edu_clean_func <- function(n) {
  df <- edu[[n]] %>% est_moe_percent_func()
  
  # subset enrollment and educational level data
  df <- df[, c(which(str_detect(colnames(df),"Nursery.school.")):which(str_detect(colnames(df),"Graduate.or.professional.degree")))]

  df$pre.kin <- rowSums(df[,1:2]) # merge preschool and kindergarten enrollment
  df$less.high <- rowSums(df[,7:8]) # merge < K9 and < K12 education level

  # omit the pre-merged columnes
  df <- df[,c(14,3:5,15,9:13)]

  df <- transpose_func(df) # transpose the data with rows as each subsetion and columns as the tracts
  df$cum <- df$Tract1255+df$Tract1256 # sum data of the two tracts
  df$cate <- rownames(df)
  df <- df[,c(4,3)]
  
  return(df)
}

# function cleaning up the income/occupation data 
income_clean_func <- function(n){
  df <- income[[n]] %>% est_moe_percent_func()
  
  # clean up the column names
  colnames(df) <- gsub("Total.households..","",colnames(df))

  # detect columns with income level
  income_detect <- c(which(str_detect(colnames(df),"INFLATION.ADJUSTED.DOLLARS...Less.than..10.000"))[1]:which(str_detect(colnames(df),"INFLATION.ADJUSTED.DOLLARS...Mean.household")))

  # detect columns with occupational forms
  occupation_detect <- which(str_detect(colnames(df),"OCCUPATION"))[-1]

  # subset income and occupational forms data
  df <- df[,c(income_detect, occupation_detect)]

  # merge the income level categories
  df$less.14999 <- rowSums(df[,1:2])
  df$X15000.34999 <- rowSums(df[,3:4])
  df$X35000.74999 <- rowSums(df[,5:6])
  df$X75000.149999 <- rowSums(df[,7:8])
  df$more.150000 <- rowSums(df[,9:10])

  # omit the pre-merged categories
  df <- df[,c(11:22)] 

  df <- transpose_func(df) # transpose the data with rows as each subsetion and columns as the tracts
  df$cum <- df$Tract1255+df$Tract1256 # sum data of the two tracts
  df[1,3] <- round((df[1,1]*sum(df[3:7,1]) + df[1,2]*sum(df[3:7,2]))/(sum(df[3:7,1])+sum(df[3:7,2]))) # get the weighted average of the median
  df[2,3] <- round((df[2,1]*sum(df[3:7,1]) + df[2,2]*sum(df[3:7,2]))/(sum(df[3:7,1])+sum(df[3:7,2]))) # get the weighted average of the mean
  df$cate <- rownames(df)
  df <- df[,c(4,3)]

  return(df)
}

house_clean_func <- function(n){
  df <- house[[n]] %>% est_moe_percent_func()
  colnames(df) <- gsub("Occupied.units.paying.rent..","",colnames(df))
  
  # select occupancy, rent, and bedroom info
  occupancy <- which(str_detect(colnames(df),"OCCUPANCY"))
  bedroom <- which(str_detect(colnames(df),"BEDROOMS"))
  median.rent <- which(str_detect(colnames(df),"GROSS.RENT..Median..dollars."))
  df <- df[,c(occupancy,median.rent,bedroom)]
  
  df <- df[,-c(7,14)] # omit total units info (redundunt)
  df$vacancy <- (df[,3]/df[,1])*100 # calculate vacancy (in %)
  df$over4bed <- rowSums(df[,11:12]) # merge 4 bedroom and 5 bedroom or more
  df <- df[,c(1,13,6,4,5,7:10,14)] # select the columns in order
  
  df <- transpose_func(df)
  df$cum <- df$Tract1255+df$Tract1256
  df[2,3] <- round(df[2,1]*df[1,1]/df[1,3] + df[2,2]*df[1,2]/df[1,3], digits = 1)
  df[3,3] <- round(df[3,1]*df[1,1]/df[1,3] + df[3,2]*df[1,2]/df[1,3], digits = 1)
  df[4,3] <- round(df[4,1]*df[1,1]/df[1,3] + df[4,2]*df[1,2]/df[1,3], digits = 1)
  df[5,3] <- round(df[5,1]*df[1,1]/df[1,3] + df[5,2]*df[1,2]/df[1,3], digits = 1)
  df$cate <- rownames(df)
  df <- df[,c(4,3)]
  
  return(df)
}

# function cleaning up the age and gender data 
age_sex_clean_func <- function(n){
  df <- age_sex[[n]] %>% est_moe_percent_func()
  
  # subset race, sex ratio, and pecent column
  race <- which(str_detect(colnames(df),"RACE"))
  sexratio <- which(str_detect(colnames(df),"..Sex.ratio..males.per.100.females."))
  percent <- which(str_detect(colnames(df),"Percent"))
  df <- df[,-c(race,sexratio,percent)]
  
  # clean up the column names
  colnames(df) <- gsub("Estimate..SEX.AND.AGE..","", colnames(df))
  colnames(df) <- gsub("Total.population..","", colnames(df))

  # select, total population, gender, age group, and median age
  selected.cols <-
    c("Total.population","Male","Female","Under.5.years","5.to.9.years","10.to.14.years","15.to.19.years","20.to.24.years","25.to.34.years","35.to.44.years","45.to.54.years","55.to.59.years","60.to.64.years","65.to.74.years","75.to.84.years","85.years.and.over","Median.age..years.")
  df <- df[,selected.cols] 

  # merge the age group level
  df$age.under24 <- rowSums(df[,4:8]) 
  df$age.25.44 <- rowSums(df[,9:10])
  df$age.45.64 <- rowSums(df[,11:13])
  df$age.over.65 <- rowSums(df[,14:16])
  
  # omit the pre-merged catagories
  df <- df[,-c(4:16)]

  df <- transpose_func(df)
  df$cum <- df$Tract1255+df$Tract1256
  df[4,3] <- round(df[4,1]*df[1,1]/df[1,3] + df[4,2]*df[1,2]/df[1,3], digits = 1) # get the weighted average of median
  df$cate <- rownames(df)
  df <- df[,c(4,3)]

  return(df)
}

# function combining the annual data into one data frame
org_func <- function(cate){
  org <- matrix(nrow = nrow(cate[[1]]), ncol = length(cate))
  
  for (i in 1:length(cate)){
    org[,i] <- cate[[i]][,2]
  }
  
  colnames(org) <- paste0("Y",as.character(seq(firstyear, lastyear)))
  rownames(org) <- rownames(cate[[1]])
  
  return(as.data.frame(org))
}


edu <- open_func("DP02")
income <- open_func("DP03")
house <- open_func("DP04")
age_sex <- open_func("DP05")

edu_com <- lapply(1:length(edu), edu_clean_func)
income_com <- lapply(1:length(income), income_clean_func)
house_com <- lapply(1:length(house), house_clean_func)
age_sex_com <- lapply(1:length(age_sex), age_sex_clean_func)

edu_org <- org_func(edu_com)
income_org <- org_func(income_com)
house_org <- org_func(house_com)
age_sex_org <- org_func(age_sex_com)

path_out <- "ACS_Web/"

write.csv(edu_org, paste0(path_out,"edu.csv"))
write.csv(income_org,paste0(path_out,"income.csv"))
write.csv(house_org,paste0(path_out,"house.csv"))
write.csv(age_sex_org,paste0(path_out,"age_sex.csv"))

