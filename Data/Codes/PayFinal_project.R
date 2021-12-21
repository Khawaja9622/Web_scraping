# Clean the environment and load packages
rm(list = ls())

library(tidyverse)
library(rvest)
library(data.table)
library(jsonlite)
library(pbapply)

 path <- getwd()

## Industry Links
Industry <- read_html("https://www.payscale.com/research/US/Job")
Sectors <- Industry %>% html_nodes('.related-content-card__title') %>%  html_text()
Links <- Industry %>% html_nodes('.related-content-card') %>% html_attr('href')
Base_url <- ("https://www.payscale.com")

Industry_link <- paste0(Base_url,Links)
Industry_table<- data_frame(Sectors,Links)
saveRDS(a, paste0(path, "/Industries.rds")) 


### Jobs 
get_job_links <- function(x){    
  
  jobs <- read_html(paste0(Base_url, x))
  
  job_header <- jobs %>% 
    html_nodes(".subcats__links__item") %>% 
    html_text()
  
  job_link <-  jobs %>% 
    html_nodes(".subcats__links__item") %>% 
    html_attr('href')
  
  jobs_table <- data.frame(job_header,job_link) 
  jobs_table$Sector <- gsub("/research/US/Job/", "", x) 
  jobs_table$full_link <- paste0(Base_url,job_link)
  
  return(jobs_table)
}

jobs_table <- rbindlist(pblapply(Links, get_job_links))
jobs_table$Category <- sub('.*/', '', jobs_table$job_link)
saveRDS(jobs_table, paste0(path, "/Jobs_details.rds")) 



# job Details for individual 

 a <- read_html("https://www.payscale.com/research/US/Job=Pharmacist/Salary") 
 b <- fromJSON(a %>% html_node(xpath = '//script[@type="application/json"]') %>% html_text(), flatten = T)
 job_header <- b$props$pageProps$pageData$dimensions$job
 Average_salary <- b$props$pageProps$pageData$byDimension$`Average Salary Overall`$rows$range.50
 Skill_set <- b$props$pageProps$pageData$occupationalDetails$skills
 Average_Hourly_rate <- b$props$pageProps$pageData$byDimension$`Average Hourly Rate Overall`$rows$range.50                                                        
 Description <- b$props$pageProps$pageData$narratives$description
 Sample_size <- b$props$pageProps$pageData$occupationalDetails$sampleSize
 Job_satifaction <- b$props$pageProps$pageData$ratings$`Job Satisfaction Overall`$score
 Female_ratio <- round((b$props$pageProps$pageData$byDimension$`Gender Breakdown`$rows$profileCount[1])/(b$props$pageProps$pageData$byDimension$`Gender Breakdown`$profileCount)*100,0)
 Male_ratio <- round((b$props$pageProps$pageData$byDimension$`Gender Breakdown`$rows$profileCount[2])/(b$props$pageProps$pageData$byDimension$`Gender Breakdown`$profileCount)*100,0)
 Top_employer <- b$props$pageProps$pageData$byDimension$`Job by Employer`$rows$name[1]
 final_data <-  data.table(job_header,Average_Hourly_rate,Average_salary,Skill_set,Sample_size,Description,Job_satifaction,Female_ratio,Male_ratio,Top_employer)
combine <- left_join(final_data,jobs_table)

# creating function for Jobs
All_links <- jobs_table$full_link

All_jobs <- function(x){
  Links <- read_html(x) 
  Data <- fromJSON(Links %>% html_node(xpath = '//script[@type="application/json"]') %>% html_text(), flatten = T)
  job_header <- Data$props$pageProps$pageData$dimensions$job
  Average_salary <- Data$props$pageProps$pageData$byDimension$`Average Salary Overall`$rows$range.50
  Skill_set <- Data$props$pageProps$pageData$occupationalDetails$skills
  Average_Hourly_rate <- Data$props$pageProps$pageData$byDimension$`Average Hourly Rate Overall`$rows$range.50                                                        
  Description <- Data$props$pageProps$pageData$narratives$description
  Sample_size <- Data$props$pageProps$pageData$occupationalDetails$sampleSize
  Job_satifaction <- Data$props$pageProps$pageData$ratings$`Job Satisfaction Overall`$score
  Female_ratio <- round((Data$props$pageProps$pageData$byDimension$`Gender Breakdown`$rows$profileCount[1])/(Data$props$pageProps$pageData$byDimension$`Gender Breakdown`$profileCount)*100,0)
  Male_ratio <- round((Data$props$pageProps$pageData$byDimension$`Gender Breakdown`$rows$profileCount[2])/(Data$props$pageProps$pageData$byDimension$`Gender Breakdown`$profileCount)*100,0)
  Top_employer <- Data$props$pageProps$pageData$byDimension$`Job by Employer`$rows$name[1]
  
  final_data <-  data.table(job_header,Average_Hourly_rate,Average_salary,Skill_set,Sample_size,Description,Job_satifaction,Female_ratio,Male_ratio,Top_employer)
  
}
Salary_data <- rbindlist(pblapply(All_links, All_jobs ), fill = T)
combine_salary_data <- left_join(Salary_data,jobs_table)

saveRDS(combine_salary_data , paste0(path, "/combine_salary_data .rds"))
write_csv(combine_salary_data,file = "combine_salary_data.csv")



### experience wise data for Pharmacist ###


e_link<- read_html("https://www.payscale.com/research/US/Job=Pharmacist/Salary") 
e_json <- fromJSON(e_link %>% html_node(xpath = '//script[@type="application/json"]') %>% html_text(), flatten = T)
job_title <- e_json$props$pageProps$pageData$dimensions$job
Less_than_1_year<- e_json$props$pageProps$pageData$byDimension$`Job by Experience`$rows$range.50[1]
Four_Plus_years <- e_json$props$pageProps$pageData$byDimension$`Job by Experience`$rows$range.50[2]
Five_to_Nine_years <- e_json$props$pageProps$pageData$byDimension$`Job by Experience`$rows$range.50[3]
Ten_to_Nineteen_years <-e_json$props$pageProps$pageData$byDimension$`Job by Experience`$rows$range.50[4]
Twenty_plus_years <- e_json$props$pageProps$pageData$byDimension$`Job by Experience`$rows$range.50[5] 

experience_data <- data.table(job_title,Less_than_1_year,Four_Plus_years,Five_to_Nine_years,Ten_to_Nineteen_years,Twenty_plus_years)



### Creating function for experience ###

All_experience <- function(x){
  Links <- read_html(x) 
  Data <- fromJSON(Links %>% html_node(xpath = '//script[@type="application/json"]') %>% html_text(), flatten = T)
  job_title <- Data $props$pageProps$pageData$dimensions$job
  Less_than_1_year<- Data $props$pageProps$pageData$byDimension$`Job by Experience`$rows$range.50[1]
  Four_Plus_years <- Data $props$pageProps$pageData$byDimension$`Job by Experience`$rows$range.50[2]
  Five_to_Nine_years <- Data $props$pageProps$pageData$byDimension$`Job by Experience`$rows$range.50[3]
  Ten_to_Nineteen_years <-Data $props$pageProps$pageData$byDimension$`Job by Experience`$rows$range.50[4] 
  Twenty_plus_years <- Data $props$pageProps$pageData$byDimension$`Job by Experience`$rows$range.50[5]
  
  experience_data <- data.table(job_title,Less_than_1_year,Four_Plus_years,Five_to_Nine_years,Ten_to_Nineteen_years,Twenty_plus_years)
}

Experience_data <- rbindlist(pblapply(All_links, All_experience), fill = T)

saveRDS(Experience_data, paste0(path, "/Experience.rds")) 
write.csv(Experience_data,file = "Experience_data.csv")

## Location wise salary for Pharmacist ##
Location_link<- read_html("https://www.payscale.com/research/US/Job=Pharmacist/Salary") 
Location_json <- fromJSON(Location_link%>% html_node(xpath = '//script[@type="application/json"]') %>% html_text(), flatten = T)
State <- Location_json$props$pageProps$pageData$byDimension$`Job by Location`$rows$name
Salary_l <- (Location_json$props$pageProps$pageData$byDimension$`Job by Location`$rows$range.50)
data.
## Location function to get all the Locations salary for each job

All_Location <- function(x){
  Links <- read_html(x) 
  Data <- fromJSON(Links %>% html_node(xpath = '//script[@type="application/json"]') %>% html_text(), flatten = T)
  job_title <- Data$props$pageProps$pageData$dimensions$job
  State <- Data$props$pageProps$pageData$byDimension$`Job by Location`$rows$name
  Salary_l <- (Data$props$pageProps$pageData$byDimension$`Job by Location`$rows$range.50)
  Location_data <- data.table(job_title,State, Salary_l)
}

Location_data <- rbindlist(pblapply(All_links, All_Location), fill = T) 
Location_data <-separate(Location_data, State , c("City", "State"), sep = ", ")

saveRDS(Location_data, paste0(path, "/Location.rds")) 
write.csv(Location_data ,file="Location_data.csv")

## Payment of each jobs by employers ##
All_Employer<- function(x){
  Links <- read_html(x) 
  Data <- fromJSON(Links %>% html_node(xpath = '//script[@type="application/json"]') %>% html_text(), flatten = T)
  Job_title <- Data$props$pageProps$pageData$dimensions$job
  Company <- Data$props$pageProps$pageData$byDimension$`Job by Employer`$rows$name
  Salary <- Data$props$pageProps$pageData$byDimension$`Job by Employer`$rows$range.50
  Employer_data <- data.table(job_title,Company,Salary)
}
Employer_data <- rbindlist(pblapply(All_links, All_Employer), fill = T) 
saveRDS(Employer_data , paste0(path, "/Employer.rds"))  
write.csv(Employer_data ,file = "Employer_data.csv")






