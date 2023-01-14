
## Remove all variables from the environment.
rm(list = ls())

## clean Console  as command (CTRL + L)
cat("\014")

# ========================================
# Check and install the required packages 
# ========================================

# list of required packages
requiredPackages <- c("tidyverse","rvest")

# check for each package and install if not present
# and display in console if already installed and load the package
for (pkg in requiredPackages){
  
  #if the package is not installed then install the package
  if (!(pkg %in% rownames(installed.packages()))){
    install.packages(pkg)
  }
  else{
    # print on console that the package is already installed...
    print(paste(pkg, "is already installed..."))
    
    #load the required package (i.e. library)
    lapply(pkg, require, character.only = TRUE)
  }
}


#****************************************************************************
#                       Part 1 (Web Scrapping)
#****************************************************************************
#*

#************************ Question 1 ***************************
#* Retrieve and load all the data from the url into R

# For reading the data for the endangered sites from Wikipedia,
# we will use the following url
web_url <- "https://en.wikipedia.org/wiki/List_of_World_Heritage_in_Danger"

#************************ Question 2 ***************************
# To get the data on the world heritage sites from Wikipedia into R,
# we will use the read_html command
web_scrapped_html <- read_html(web_url)

str(web_scrapped_html)

# We will use the html_nodes command that extracts all occurrences of the 
# desired tag. We are interested in scraping data presented in description list, 
# so in the source code, we look for the description list tag: <dl> ... </dl>.
WHsiteNodes <- html_nodes(web_scrapped_html, "dl") %>% html_text() %>% .[2]
WHsiteNodes

# We will now extract the text that we need (i.e. table legend)
WHTable_legend <- unlist(strsplit(WHsiteNodes,"\n"))
WHTable_legend <- data.frame(WHTable_legend)

# print the Table Legend
print(WHTable_legend, right = FALSE)

#************************ Question 3 ***************************
# We will use the html_nodes command that extracts all occurrences of the 
# desired tag. We are interested in scraping data presented in table, 
# so in the source code, we look for the table: <table> ... </table>.
tbl_endangered_sites <- 
  html_nodes(web_scrapped_html, ".wikitable") %>% 
  .[1] %>% 
  html_table()

tbl_endangered_sites <- tbl_endangered_sites[[1]]

tbl_endangered_sites

#************************ Question 4 ***************************
#* To get all available hyperlinks in the url

# Extract the URLs
extracted_urls <- web_scrapped_html %>% html_nodes("a") %>% html_attr("href")

# Extract the link text
extracted_links <- web_scrapped_html %>% html_nodes("a") %>% html_text()

All_hyperlinks <- tibble(link = extracted_links, url = extracted_urls)


#************************ Question 5 ***************************

#Extracting just the hyperlink which leads to Criteria with 
#both Cultural and Natural.
text_Criteria <- All_hyperlinks %>% filter(link == "Criteria")
text_Criteria[1,2]

#For making the url for selection criteria, we will use the base url
# and split it from the "wiki" keyword
base_url <- unlist(strsplit(web_url, split = "/wiki"))

#we only need the first part of the base url and only first line
url_selection_criteria <- paste0(base_url[1],text_Criteria[1,2])
url_selection_criteria

#************************ Question 6 ***************************

# scrape the data from the url (i.e. url_selection_criteria) obtained in 
# question 5 above
text_cultural_natural <- read_html(url_selection_criteria)

# We will use the html_nodes command that extracts all occurrences of the 
# desired tag. We are interested in scraping data presented in ordered list, 
# so in the source code, we look for the description list tag: <ol> ... </ol>.
CulturalNatural_Node <- text_cultural_natural %>% html_nodes ("ol")

# Now we will create two lists one for "cultural" and one for "natural"

#cultural heritage
list_cultural_heritage <- 
  html_text(CulturalNatural_Node[[1]]) %>% #extract the raw text from the html node 
  strsplit("\n") %>%                       #split the data by "\n"
  unlist() %>%                             #convert the list to a vector
  str_replace_all("\"","") %>%             #replace all the "\" character
  as_tibble()                              #convert it into a tibble

# natural heritage
list_natural_heritage <- 
  html_text(CulturalNatural_Node[[2]]) %>% #extract the raw text from the html node
  strsplit("\n") %>%                       #split the data by "\n"
  unlist() %>%                             #convert the list to a vector
  str_replace_all("\"","") %>%             #replace all the "\" character
  as_tibble()                              #convert it into a tibble

# we will now store the two list that we created above as list of lists
list_cultural_natural <- list(list_cultural_heritage, list_natural_heritage)

# view the structure of the list
str(list_cultural_natural)

#call the list to view the data
list_cultural_natural


#****************************************************************************
#                     Part 2 (Data Cleaning and Wrangling)
#****************************************************************************
#*
#*

#************************ Question 1 ***************************
# Drop Image and Refs columns
cols_to_drop <- c("Image","Refs")

# select all columns except the columns to be dropped
tbl_endangered_sites = tbl_endangered_sites[,!(names(tbl_endangered_sites) %in% cols_to_drop)]

# check if the columns have been dropped or not
colnames(tbl_endangered_sites)

#************************ Question 2 ***************************
# we will create a regular expression for getting the different country names
regex_for_country <- regex("
              # match country names starting with a comma [,] and ending
              # with lowe case letter followed by a digit and considering
              # the special cases
              ((?<=,\\s{0,2})[A-Z][^,]*[a-z](?=([,*]?\\s?\\d)|[.]))
              
              # match country names starting with upper case letter
              |([A-Z][^,A-Z]+([a-z]|[A-Z]{7})(?=(\\[.{1,4}])?\\s?\\d))",
              comments = T)

# for loop to loop through the rows in the data frame to get the country names
for(rows in 1:nrow(tbl_endangered_sites)){

  # extract the country names using the regular expression created above
  tbl_endangered_sites$Location[rows] <- str_extract(tbl_endangered_sites$Location[rows],
                                                     pattern = regex_for_country)

  # site 32 has two country names, we will remove "*" character and 
  # replace it with a "&" to make it clear
  tbl_endangered_sites$Location[rows] <- str_replace(tbl_endangered_sites$Location[rows],
                                                     pattern = "[*]",
                                                     replacement = " &")
}

#************************ Question 3 ***************************

# To split the "Criteria" variable in to two variables,
# we will use pattern = ":" to split the data
tbl_endangered_sites$Criteria <- str_split_fixed(tbl_endangered_sites$Criteria,
                                             pattern = ":", n = 2)

# to store the data into two separate variables, we will create two empty lists
# one for storing the "Type" (cultural/natural) and "Criteria" (containing
# roman numbers)
Type <- list()
Criteria <- list()

# for loop to loop through all the rows in the data frame
for(rows in 1:nrow(tbl_endangered_sites)){
  # store the first part of data in the "Type" list that we obtained using regex
  Type[rows] <- tbl_endangered_sites$Criteria[rows,1]
  
  # store the second part of data in the "Criteria" list that we obtained using regex
  Criteria[rows] <- tbl_endangered_sites$Criteria[rows,2]
}

# we will use unlist() function to produce a vector
tbl_endangered_sites$Type <- unlist(Type)
tbl_endangered_sites$Criteria <- unlist(Criteria)

# view the column names to see the order
colnames(tbl_endangered_sites)

# we will now use the relocate() to change the column order
tbl_endangered_sites <- tbl_endangered_sites %>% 
  relocate(Type, .before = Criteria)

# view column names again to see the whether the order of the columns
colnames(tbl_endangered_sites)

#************************ Question 4 ***************************

# first we will rename the column name to "Area_(acre)" from "Areaha (acre)"
names(tbl_endangered_sites)[names(tbl_endangered_sites) == "Areaha (acre)"] <- "Area_(acre)"

# for loop to loop through the rows in the data frame (i.e. tbl_endangered_sites)
# for extracting the area
for(indx in 1:nrow(tbl_endangered_sites)){
  
  # extract only the data present inside the "()" round brackets
  tbl_endangered_sites$`Area_(acre)`[indx] <- 
    str_extract(tbl_endangered_sites$`Area_(acre)`[indx], 
                pattern = "(?<=\\().+?(?=\\))")
  
  # remove comma from the numbers
  tbl_endangered_sites$`Area_(acre)`[indx] <- gsub(pattern = ",", 
                                                   replacement = "", 
                                                   tbl_endangered_sites$`Area_(acre)`[indx])
}

# view the endangered list data
tbl_endangered_sites

#************************ Question 5 ***************************

# for loop to loop through the rows in the data frame (i.e. tbl_endangered_sites)
# to get the very last year from the "Endangered" column
for (index in 1:nrow(tbl_endangered_sites)) {
  
  # look for a hyphen "-" and replace it with an empty string
  tbl_endangered_sites$Endangered[index] <- gsub(pattern = "-", replacement = "",
                                                 tbl_endangered_sites$Endangered[index])
  
  # match and replace all the occurrences of the data before a comma "," and
  # replace it with an empty string
  tbl_endangered_sites$Endangered[index] <- gsub(pattern = ".*, ", replacement = "", 
                                                 tbl_endangered_sites$Endangered[index])
  
  # remove all the alpha numeric patterns from the variable
  tbl_endangered_sites$Endangered[index] <- gsub(pattern = "[^[:alnum:]]",
                                                 replacement = "", 
                                                 tbl_endangered_sites$Endangered[index])
}

# view the endangered list data
tbl_endangered_sites


#************************ Question 6 ***************************

# we will use the "str()" function to check if all the variable present in the
# data frame (i.e. tbl_endangered_sites) are either character vector or numeric
# vector.
str(tbl_endangered_sites)

# since we have multiple columns that has numerical data in them, we will
# convert the columns to numeric data type that can be used later on for 
# further calculations.
tbl_endangered_sites[, c(5:7)] <- sapply(tbl_endangered_sites[, c(5:7)],
                                         as.numeric)

# view the structure to verify that the columns have been converted into numeric
str(tbl_endangered_sites)

#****************************************************************************
#                         Part 3 (Data Analysis)
#****************************************************************************

#************************ Question 1 ***************************

# we will group the data by 'Type' and count the number of entries present and
# display the result in the descending order
type_of_site <- tbl_endangered_sites %>% 
  group_by(Type) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# the most common type of site in the endangered list is
print(paste("The most common type of site in the endangered list is", 
            type_of_site$Type[1]))

# for loop to print the number of sites each site type has
for(idx in 1:length(type_of_site)){
  print(paste("There are",type_of_site$count[idx], type_of_site$Type[idx],"sites."))
}

# view the data in the data frame
print(type_of_site)

## Analysis: From the above table we can clearly see that Cultural site is the 
##           most common because the Cultural site has count of 36 and the 
##           Natural site has the count of 17 only.

#************************ Question 2 ***************************

# Sites with largest and smallest area m^2

# site with the largest area in m^2
site_with_max_area <- tbl_endangered_sites %>% 
  mutate(`max_area (msqr)` = `Area_(acre)` * 4046.86) %>%  # converted to m^2
  select(Name, (`max_area (msqr)`))%>% 
  arrange(desc(`max_area (msqr)`))

print(paste(site_with_max_area$Name[1], "has the maximum area of",
            site_with_max_area$`max_area (msqr)`[1],"(msqr)"))


# site with the smallest area in m^2
site_with_min_area <- tbl_endangered_sites %>% 
  mutate(`min_area (msqr)` = `Area_(acre)` * 4046.86) %>%   # converted to m^2
  select(Name, `min_area (msqr)`)%>% 
  arrange(`min_area (msqr)`)

print(paste(site_with_min_area$Name[1], "has the minimum area of",
            site_with_min_area$`min_area (msqr)`[1],"(msqr)"))

## Analysis: The site which has the largest area (in m2) is "Air and Ténéré Natural Reserves"
##           and the site which has the smallest area (in m2) is "Site of Palmyra"

#************************ Question 3 ***************************

# frequency (in years) with which sites were put on the endangered list

ggplot(data = tbl_endangered_sites, aes(Endangered)) +                # for creating a ggplot
  geom_histogram(color = "darkblue", fill = "lightblue", breaks = seq(1980, 2025, 5)) +   # for displaying the histogram chart
  ggtitle("Frequency chart of sites in endangered list from (1980 to 2020)") + # set the plot title
  scale_x_continuous(name = "Year from (1980 - 2020)",                    # display the label for x-axis
                     breaks = seq(1980, 2025, 5)) +                       # display x-axis values with a gap interval of 5
  scale_y_continuous(name = "Number of sites put on the endangered list", # display the y-axis label
                     breaks = seq(0, 18, 2)) +                            # display y-axis values with a gap interval of 2
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5), # for changing the font size of the plot title
        axis.title.x = element_text(size = 12, hjust = 0.5),              # for changing the font size of the x-axis label
        axis.title.y = element_text(size = 12, hjust = 0.5))              # for changing the font size of the y-axis label
  

## Analysis: From histogram we can see that :
##           From 1980 to 1989 only 2  sites were added
##           From 1990 to 1999 only 8  sites were added
##           From 2000 to 2009 only 11  sites were added
##           From 2010 to 2015 only 18 sites were added
##           From 2016 to 2020 only 12 sites were added
##           From 2021 to 2025 only 1 site was added
## We can see that the maximum number of sites that were put on the endangered
## list are from 2010 to 2015

#************************ Question 4 ***************************
# Which country has most sites and how many sites does each country has

# For getting the count of the sites each country has
location_frequency <- tbl_endangered_sites %>% 
  group_by(Location) %>%                       # group by location
  summarise(Location_feq = n()) %>%            # get the count in a new column
  arrange(desc(Location_feq))                  # arrange the data in descending order

# get the index position of the data in the data frame (i.e. location_frequency)
index <- which(location_frequency$Location_feq == max(location_frequency$Location_feq))

# display the country name with maximum sites with the help of the "index" that
# we obtained from the above step
print(paste("The country with most sites is", location_frequency$Location[index]))

# display countries and their number of sites
print(paste(location_frequency$Location, "has",location_frequency$Location_feq, "no. of sites" ))

print(location_frequency, n = Inf)

## Analysis: From the above analysis we can see that
##            Syria: has 6 sites
##            Democratic Republic of the Congo, Libya: have 5 sites
##            Yemen, Iraq, Mali: have 3 sites
##            Afghanistan,Palestine : have 2 sites
##
## The maximum number of sites that were put on the endangered list are from 
## Syria.

#************************ Question 5 ***************************
# Time taken by sites to be in Endangered list.

# To get the number of years taken by each site to be put on the endangered
# list, we create a new column "years_taken" in the data frame (i.e. tbl_endangered_list)
# by subtracting the values present in the "Endangered" column with the values present
# in the "Year (WHS)" column.
tbl_endangered_sites$years_taken <- 
  tbl_endangered_sites$Endangered - tbl_endangered_sites$`Year (WHS)`

# Display the name, location: country name, 
# Year (WHS): the year the site was inscribed on the World Heritage List, 
# Endangered: the year the site appeared on the List of World Heritage in Danger
# and the newly created column years_taken
tbl_endangered_sites %>% 
  select(Name, Location, `Year (WHS)`, Endangered, years_taken)

## Analysis: Ashur and 9 other sites took no time to be in Endangered list.
##           Ancient City of Damascus, Archaeological Site of Cyrene, 
##           Archaeological Site of Leptis Magna and Archaeological Site of 
##           Sabratha took 34 years to be put on the endangered list.

#************************ Question 6 ***************************
#  frequency with which sites were put on the endangered list

# sub-setting and grouping by the year of difference each site moved to 
# endangered list from WHS list and getting the count of sites
frequency_of_sites_endangered <- as.data.frame(tbl_endangered_sites %>% 
                                                 group_by(years_taken) %>% 
                                                 summarise(Count = n()))

# view the data
frequency_of_sites_endangered

# create a plot to view the frequency with which sites were put on the endangered list
ggplot(data = frequency_of_sites_endangered,                          # data frame
       aes(x = years_taken, y = Count)) +                             # x-axis and y-axis data points
  geom_bar(stat = "identity", width = 1,                              # identity bar chart with width of 1
           color = "darkblue", fill = "lightblue") +                  # fill: to display color inside the bar and color: to display the border color of the bar 
  geom_text(aes(label = Count),     # to display the frequency count on the bars and
            vjust = -0.2) +         # adjust the vertical positioning of the count on bars
  ggtitle("Frequency with which sites were put on Endangered list") + # plot title
  labs(x = "Year Differences", y = "Frequency") +                     # set x-axis and y-axis label
  scale_x_continuous(breaks = seq(0, 34, 1)) +                        # display all the values on x-axis
  scale_y_continuous(breaks = seq(0, 9, 1)) +                         # display all the values on y-axis
  geom_vline(xintercept = 3.5, linetype="dashed", color = "red") +    # add a vertical line on x-axis
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5), # for changing the font size of the plot title
        axis.title.x = element_text(size = 11, hjust = 0.5),              # for changing the font size of the x-axis label
        axis.title.y = element_text(size = 11, hjust = 0.5))              # for changing the font size of the y-axis label

## Analysis: 
## 1. The frequency of number of sites moving to endangered list is high
##    usually within an year as 9 sites were moved from the world heritage list 
##    to the endangered list.
##
## 2. From the plot we can see that after 3 years 37 sites has been moved from 
##    the world heritage sites list to the endangered list





