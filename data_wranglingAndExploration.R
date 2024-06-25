install.packages("dplyr")
install.packages("tidyr")
install.packages("plotly")

library(dplyr)
library(tidyr)
library(plotly)

#load the dataset and save in dataframe
sal<- read.csv("salaries.csv")

#display the first 5 records of the dataset
head(sal)
#last 10
tail(sal, 10)


#CLEAN DATA 

# check for null values
missing_values <- any(is.na(sal))
missing_values

# how many nulls 
missing_values <- sum(is.na(sal))
missing_values

# which columns have missing values
missing_values <- colSums((is.na(sal)))
missing_values

# Ommit missing values and save in a data frame 
sal_cleaned <- na.omit(sal)
sal_cleaned

#check for duplicate values
duplicates_logical <- duplicated(sal)
duplicates_logical

# count how many duplicates you have 
duplicates <- sum(duplicated(sal))
duplicates

# extract duplicate rows 
duplicates <- sal[duplicates_logical, ]
duplicates

# Ommit duplicates and save in another data frame 
sal_cleaned <- sal[!duplicated(sal), ]




# EXPLORE DATA
# summary statistics

summary_value <- summary(sal)
summary_value

# changing datatype of a cloumn

#BasePay to numeric
sal$BasePay <- as.numeric(sal$BasePay)

sal$OvertimePay <- as.numeric(sal$OvertimePay)
sal$OtherPay <- as.numeric(sal$OtherPay)
sal$Benefits <- as.numeric(sal$Benefits)

#remove columns not needed
sal <- subset(sal, select = -Notes)
sal <- subset(sal, select = -Benefits)




# VISUALIZE DATA 
#scatter plot for total salary vs. Years

plot_salary_year <- plot_ly(
  data = sal, 
  x = ~Year, 
  y = ~TotalPay, 
  type = "scatter",
  marker = list(size = 10),
  color = ~JobTitle,
  text = ~paste("Job Title: ", JobTitle, "<br>Salary: $", TotalPay),
  hoverinfo = "text"
)%>%
  layout(title = "Total Salary per Year",
         xaxis = list(title = "Years"),
         yaxis = list(title = "Total Salary")
  )


plot_salary_year


# Box Plot 

sal2 <- read.csv("Salary.csv")

plot_salary_EXyear <- plot_ly(
  data = sal2, 
  x = ~yearsExperience, 
  y = ~Salary, 
  type = "scatter",
  marker = list(size = 10),
  color = ~Department,
  text = ~paste("Job Title: ", Position, "<br>Salary: $", Salary),
  hoverinfo = "text"
)%>%
  layout(title = "Total Salary per Year",
         xaxis = list(title = "Years of Experience"),
         yaxis = list(title = "Total Salary")
  )

plot_salary_EXyear




# Box plot

plot_salary_education <- plot_ly(
  data = sal2, 
  x = ~Education, 
  y = ~Salary, 
  type = "scatter",
  text = ~paste("Education Level: ", Education, "<br>Salary: $", Salary),
  hoverinfo = "text"
)%>%
  layout(title = "Salary Distribution by Education Level",
         xaxis = list(title = "Eduction"),
         yaxis = list(title = "Total Salary")
  )

plot_salary_education

# view the plot in same window
subplot(plot_salary_EXyear, plot_salary_education, nrows=2)





#MAPS
# in order to color the states plotly works with the two characters
# state code, that's why we have to join the two tables together 

states <- read.csv("states.csv")

wage_df <- read.csv("Minimum Wage Data.csv") %>%
  inner_join(states, by = c("State" = "State")) %>% # Joining tables using the state column
    select (Year, State, Code, Wage)%>% # select which columns to have in dataset
    mutate(hover = paste0(State, "\n$", Wage)) # create a new column

#Setting the font style
fontstyle = list(
  family = 'DM Sans',# font type
  size = 15, # font size
  color = 'blue'# font color
)

# setting the label style
label = list(
  bgcolor = '#e1a42d', # background color
  bordercolor = "transparent", #no border
  font = fontstyle
)

  
# visualize the Data 

wage_map <- wage_df %>% plot_ly(
  locationmode = 'USA-states', # setting the location to the USA
  frame = ~Year, #setting the slider for which column
  type = 'choropleth' # type of map by filling the regions
)%>%
  add_trace(
  locations = ~Code, # setting the states on the map
  z = ~Wage, # value to specify on map
  zmin = 0, # minium value for legend
  zmax = max(wage_df$Wage), # maxium value for the legend 
  color = ~Wage, # color according to wages
  colorscale = 'Electric', # color palette
  text = ~hover,
  hoverinfo = 'text'
  )%>%
  layout(
    geo = list(scope ='usa'), # set the scope to USA
    title = "Minium wage by state in USA \n from 1968 to 2020",
    font = fontstyle
  )%>%
  # remove the displaying mode bar at the top
  config(displayModeBar = FALSE)%>%
  # set the label style created to the tooltip
  style(hoverlabel = label)%>%
  #display the $ in the legend
  colorbar(tickprefix = '$')


wage_map


























