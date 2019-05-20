library(dplyr)
library(devtools)
library(datapkg)
library(stringr)

##################################################################
#
# Processing Script for Municipal-Revenue-and-Expenditures
# Created by Jenna Daly
# On 02/15/2017
# Update 07/17/2017 Jenna Daly
#  - Remove Plymouth pop from state totals in 2014
# Update 01/18/2018 Jenna Daly
#  - Removed code that corrects for Plymouth in 14-15, no longer needed
##################################################################

sub_folders <- list.files()
data_location <- grep("raw", sub_folders, value=T)
path <- (paste0(getwd(), "/", data_location))
#grabs all csvs (even not FISCIN data)
all_csvs <- dir(path, recursive=T, pattern = ".csv") 
#isolates FISCIN csvs
only_FISCIN <- all_csvs[grep("FISCIN", all_csvs)] 

#create empty data frame with set columns
all_data <- setNames(data.frame(matrix(ncol = 13, nrow = 0)), 
                     c("Town",
                       "Year",
                       "Educational Expenditures",
                       "Intergovernmental Revenue",
                       "Total Revenue",
                       "Total Transfers into General Fund",
                       "Total Transfers from General Fund",
                       "Other Financing Sources",
                       "Net Change in Operating Funds",
                       "Property Tax Revenue",
                       "Total Expenditures",
                       "Current Year Adjusted Taxes Collectible",
                       "Population"))

#read in each raw file and get ready for master combine

for (i in 1:length(only_FISCIN)) {
  current_file <- read.csv(paste0(path, "/", only_FISCIN[i]), stringsAsFactors=F, header=T)
  remove_folder <- sub(".*/", "", only_FISCIN[i]) #filename
  get_year <- unique(as.numeric(unlist(gsub("[^0-9]", "", unlist(remove_folder)), "")))
  get_year <- get_year + 2000
  SFY <- paste(get_year - 1, get_year, sep = "-")
  SFY2 <- paste("SFY", SFY)
  current_file$Year <- SFY2
  print(current_file$Year)
  col_names <- colnames(current_file)
  
  pop_col <- grep("population", col_names, ignore.case=T, value=T)
  
  
  final_columns <- current_file[, c("Municipality",
                                    "Year",
                                    "Education",
                                    "Inter_Gov_Rev",
                                    "Total_Revenue",
                                    "TOTAL_Trans_to_Genl_Fund",
                                    "TOTAL_Trans_from_Genl_Fund",
                                    "Other_Financing_Sources",
                                    "Operating_Surplus_.Deficit._Including_Sources_And_Uses",
                                    "Tax_Rev",
                                    "Total_Expenditures",
                                    "Curr_Year_Adjusted_Taxes_Collectible",
                                    pop_col)]  
  names(final_columns) <- c("Town",
                            "Year",
                            "Educational Expenditures",
                            "Intergovernmental Revenue",
                            "Total Revenue",
                            "Total Transfers into General Fund",
                            "Total Transfers from General Fund",
                            "Other Financing Sources",
                            "Net Change in Operating Funds",
                            "Property Tax Revenue",
                            "Total Expenditures",
                            "Current Year Adjusted Taxes Collectible",
                            "Population")
  
  # take out "Groton (City of)" because it is a political subdivision of groton and not the town.
  final_columns <- final_columns[final_columns$Town != "GROTON (City of)",]
  
  # Add this iteration's data to main container, first removing duplicated year data
  all_data <- all_data[all_data$Year!=SFY2,]
  all_data <- rbind(all_data, final_columns)
}

# Town names to title case
all_data$Town <- str_to_title(all_data$Town)

## Round numeric columns to whole numbers
round_df <- function(x, digits) {
  columns_to_round <- c("Educational Expenditures",
                        "Intergovernmental Revenue",
                        "Total Revenue",
                        "Total Transfers from General Fund",
                        "Total Transfers into General Fund",
                        "Other Financing Sources",
                        "Net Change in Operating Funds",
                        "Property Tax Revenue",
                        "Total Expenditures",
                        "Current Year Adjusted Taxes Collectible",
                        "Population")
  columns_to_round <- sapply(x, mode) == 'numeric'
  all_data[columns_to_round] <-  round(all_data[columns_to_round], digits)
}

#all_data$Year

## !! For some unknown reason, applying this round_df function removes Year from the dataframe.
## - So I commented it out. - Added by Ilya on May 20, 2019
#all_data <- round_df(all_data, 0)

#Create Calculated Columns (percents will be rounded to 1 decimal)
all_data$"Non-Educational Expenditures" <- NA
all_data$"Non-Educational Expenditures" <- (all_data$"Total Expenditures" - all_data$"Educational Expenditures")

all_data$"Total Revenue including Other Sources" <- NA
all_data$"Total Revenue including Other Sources" <- (all_data$"Total Revenue" + all_data$"Other Financing Sources")

all_data$"Other Revenue" <- NA
all_data$"Other Revenue" <- (all_data$"Other Financing Sources" - all_data$"Total Transfers into General Fund")

all_data$"Current Year Adjusted Tax Levy per Capita" <- NA
all_data$"Current Year Adjusted Tax Levy per Capita" <-  round((all_data$"Current Year Adjusted Taxes Collectible" / all_data$"Population"), 0)

all_data$"Property Tax Revenue as Percent of Total Revenue and Transfers into General Fund" <- NA
all_data$"Property Tax Revenue as Percent of Total Revenue and Transfers into General Fund" <-  round(100 * (all_data$"Property Tax Revenue" / (all_data$"Total Revenue" + all_data$"Total Transfers into General Fund")), 1)

all_data$"Ratio of Intergovernmental Revenue to Total Revenue" <- NA
all_data$"Ratio of Intergovernmental Revenue to Total Revenue" <-  round(100 * (all_data$"Intergovernmental Revenue" / all_data$"Total Revenue"), 1)

all_data$"Ratio of Total Revenue to Total Expenditures" <- NA
all_data$"Ratio of Total Revenue to Total Expenditures" <-  round(100 * (all_data$"Total Revenue" / all_data$"Total Expenditures"), 1)

#Current Year Adjusted Tax Levy per Capita as Percent of State Average
#Calculated as: 

#[                        Current Year Adjusted Tax Levy per Capita of a given town for a given year                           ]
#[-----------------------------------------------------------------------------------------------------------------------------] X 100
#[(sum of all Current Year Adjusted Taxes Collectible for that given year) / (sum of all states population for that given year)]

sum_adjtaxcol <- aggregate(`Current Year Adjusted Taxes Collectible` ~ Year, all_data, sum)
sum_pop <- aggregate(Population ~ Year, all_data, sum)

plymouth2014pop <- all_data$Population[which(all_data$Year == "SFY 2014-2015" & all_data$Town == "Plymouth")]
state2014pop <- sum_pop$Population[which(sum_pop$Year == "SFY 2014-2015")]

#Remove Plymouth pop for 2014 (11813) from total state population for 2014
#sum_pop$Population[which(sum_pop$Year == "SFY 2014-2015")] <- (state2014pop - plymouth2014pop) <- no longer needed (01/18/2018)

joined_sums <- merge(all_data, sum_adjtaxcol, by="Year")
joined_sums <- merge(joined_sums, sum_pop, by="Year")

joined_sums$"Current Year Adjusted Tax Levy per Capita as Percent of State Average" <- NA
joined_sums$"Current Year Adjusted Tax Levy per Capita as Percent of State Average" <- round(((joined_sums$"Current Year Adjusted Tax Levy per Capita")/((joined_sums$`Current Year Adjusted Taxes Collectible.y`)/(joined_sums$Population.y)))*100, 1) 

arranged <- select(joined_sums, Town, Year, 
                   `Educational Expenditures`,
                   `Total Expenditures`,
                   `Intergovernmental Revenue`,
                   `Total Transfers from General Fund`,
                   `Total Transfers into General Fund`,
                   `Net Change in Operating Funds`,
                   `Property Tax Revenue`,
                   `Current Year Adjusted Taxes Collectible.x`,
                   `Non-Educational Expenditures`,
                   `Total Revenue including Other Sources`,
                   `Other Revenue`,
                   `Total Revenue`,
                   `Current Year Adjusted Tax Levy per Capita`,
                   `Property Tax Revenue as Percent of Total Revenue and Transfers into General Fund`,
                   `Ratio of Intergovernmental Revenue to Total Revenue`,
                   `Ratio of Total Revenue to Total Expenditures`, 
                   `Current Year Adjusted Tax Levy per Capita as Percent of State Average`) %>% 
  arrange(Town)

#Relabel column names
colnames(arranged) <- c( "Town", "Year", 
                         "Educational Expenditures",
                         "Total Expenditures",
                         "Intergovernmental Revenue",
                         "Total Transfers from General Fund",
                         "Total Transfers into General Fund",
                         "Net Change in Operating Funds",
                         "Property Tax Revenue",
                         "Current Year Adjusted Taxes Collectible",
                         "Non-Educational Expenditures",
                         "Total Revenue including Other Sources",
                         "Other Revenue",
                         "Total Revenue",
                         "Current Year Adjusted Tax Levy per Capita",
                         "Property Tax Revenue as Percent of Total Revenue and Transfers into General Fund",
                         "Ratio of Intergovernmental Revenue to Total Revenue",
                         "Ratio of Total Revenue to Total Expenditures", 
                         "Current Year Adjusted Tax Levy per Capita as Percent of State Average")

#convert to long format
cols_to_stack <- c("Current Year Adjusted Taxes Collectible",
                   "Current Year Adjusted Tax Levy per Capita",
                   "Educational Expenditures",
                   "Intergovernmental Revenue",
                   "Net Change in Operating Funds",
                   "Non-Educational Expenditures",
                   "Other Revenue",
                   "Property Tax Revenue",
                   "Total Expenditures",
                   "Total Revenue",
                   "Total Revenue including Other Sources",
                   "Total Transfers from General Fund",
                   "Total Transfers into General Fund",
                   "Current Year Adjusted Tax Levy per Capita as Percent of State Average",
                   "Property Tax Revenue as Percent of Total Revenue and Transfers into General Fund",
                   "Ratio of Intergovernmental Revenue to Total Revenue",
                   "Ratio of Total Revenue to Total Expenditures")

long_row_count = nrow(arranged) * length(cols_to_stack)

combined_final_long <- reshape(arranged, 
                               varying = cols_to_stack, 
                               v.names = "Value", 
                               timevar = "Variable", 
                               times = cols_to_stack, 
                               new.row.names = 1:long_row_count,
                               direction = "long"
)

combined_final_long$Variable <- factor(combined_final_long$Variable, levels = cols_to_stack)
combined_final_long <- combined_final_long[order(combined_final_long$Town, combined_final_long$Year, combined_final_long$Variable),]
combined_final_long$id <- NULL

#check to see if any duplicates made it into the data frame (this should be empty)
duplicates <- combined_final_long[duplicated(combined_final_long[,1:3]),]

#add Measure Type
combined_final_long$"Measure Type" <- NA
##fill in 'Measure Type' column based on criteria listed below
combined_final_long$"Measure Type"[which(combined_final_long$Variable %in% c("Current Year Adjusted Taxes Collectible",	                                             
                                                                             "Current Year Adjusted Tax Levy per Capita",	                                           
                                                                             "Educational Expenditures",           	                                             
                                                                             "Intergovernmental Revenue",                                                            
                                                                             "Net Change in Operating Funds",	                                                       
                                                                             "Non-Educational Expenditures",	                                                       
                                                                             "Other Revenue",	                                                                       
                                                                             "Property Tax Revenue",                                                                 
                                                                             "Total Expenditures",	                                                                 
                                                                             "Total Revenue",                                                                        
                                                                             "Total Revenue including Other Sources",                                                
                                                                             "Total Transfers from General Fund",	                                                   
                                                                             "Total Transfers into General Fund"))] <- "Number"

combined_final_long$"Measure Type"[which(combined_final_long$Variable %in% c("Current Year Adjusted Tax Levy per Capita as Percent of State Average",	               
                                                                             "Property Tax Revenue as Percent of Total Revenue and Transfers into General Fund",     
                                                                             "Ratio of Intergovernmental Revenue to Total Revenue",	                                 
                                                                             "Ratio of Total Revenue to Total Expenditures"))] <- "Percent"

#add FIPS (using raw URL from GitHub)
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

merge_long_fips <- merge(combined_final_long, fips, all=T)

#remove "Connecticut"
municipal_revenue_and_expenditures_data <- merge_long_fips[!merge_long_fips$Town == "Connecticut",]

#Reorder/sort columns
municipal_revenue_and_expenditures_data <- municipal_revenue_and_expenditures_data %>% 
  select(Town, FIPS, Year, `Measure Type`, Variable, Value) %>% 
  arrange(Town, Year, `Measure Type`, Variable)

# Write to File
write.table(
  municipal_revenue_and_expenditures_data,
  file.path(getwd(), "data", "municipal_revenue_and_expenditures_data_2017.csv"),
  sep = ",",
  na = "",
  row.names = F
)

