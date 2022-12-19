#INSTALLING PACKAGES
install.packages('quantmod')
install.packages('stringr')
install.packages('tidyr')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('tidyverse')

#LOADING PACKAGES
library(quantmod)
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)

#UPLOADING DATA
#BNP
bnp_data = getSymbols("BNP.PA", from ="2009-01-01", to="2011-12-31", auto.assign = FALSE)    
head(bnp_data)
tail(bnp_data)

bnp_prices = bnp_data[, "BNP.PA.Close"]  #We take just the close price
head(bnp_prices)
tail(bnp_prices)

#CAC40
cac40_data = getSymbols("^FCHI", from ="2009-01-01", to="2011-12-31", auto.assign = FALSE)
head(cac40_data)
tail(cac40_data)

cac40_prices = cac40_data[, "FCHI.Close"]  #We take just the close price
head(cac40_prices)
tail(cac40_prices)

#MERGING PRICES
Prices = merge(cac40_prices, bnp_prices)
head(Prices)
tail(Prices)

#CREATING DATAFRAME
#We convert our data to a dataframe, and then we convert our dates from row names to an actual column. 
#This is necessary since we will treat our dates as a variable (e.g., when plotting price changes as a function of the date).
#Now we are ready to compute the daily percent changes for each company.

combined_prices = data.frame(Prices)
combined_prices = tibble::rownames_to_column(combined_prices, "Date")
combined_prices$Date = as.Date(combined_prices$Date, format = "%Y-%m-%d")
head(combined_prices)

list_na = colnames(combined_prices)[apply(combined_prices, 2, anyNA)]
list_na

missing_average = apply(combined_prices[,colnames(combined_prices) %in% list_na],
                        2,
                        mean,
                        na.rm = TRUE)
missing_average

combined_prices_1 = combined_prices %>%
  mutate(CAC40 = ifelse(is.na(FCHI.Close), missing_average[1], FCHI.Close),
         BNP = ifelse(is.na(BNP.PA.Close), missing_average[2], BNP.PA.Close))
head(combined_prices_1)

new_prices = combined_prices_1 %>% select(-FCHI.Close, -BNP.PA.Close)
head(new_prices)


#COMPUTING DAILY PERCENT CHANGES:
#We use a for loop to iterate over the closing price columns. Within each iteration, 
#we create the name for the new column (e.g, "TM % Change" for Toyota), calculate the percent changes, 
#and assign these percent changes to the new column. Line five is particularly interesting, 
#as it shows how we can express our formula for the percent change concisely in R. 
#This brevity is made possible by the lag function, which shifts each value in a vector down by one, 
#and the fact that arithmetic operations are vectorized in R.

for (col in names(new_prices)[-1]) {
  symbol= str_sub(col, 1, -1)
  new_col_name = paste(symbol, "% Change")
  col_values = new_prices[[col]]
  new_prices[[new_col_name]] <- 100*(col_values - lag(col_values))/lag(col_values)
}
head(new_prices)

# Remove the first line row since there is NA since the percentage variation is zero,
# the 1st of January the stock is closed.
new_prices = new_prices[-1, ]
head(new_prices)

#DATA MANIPULATION:
#We select the percent change columns, filtering out the closing price columns that are no longer needed. 
#Then we remove "% Change" from our column names, leaving just the company ticker symbol for each column. 
#In the third line we re-format the data so that the company is represented as a variable 
#(rather than having a separate column for each company).

new_prices = select(new_prices, Date, `CAC40 % Change`:`BNP % Change`)
names(new_prices)[-1] = str_sub(names(new_prices)[-1], 1, -10)
names(new_prices)
new_prices = gather(new_prices, key = "Symbol", value = "% Change", CAC40:BNP)
head(new_prices)

filter(new_prices, Date == "2009-01-05")

#PLOT DATA:
#We tell ggplot that we want to use the combined_2019_df dataset for our plot. 
#We then indicate what type of plot we want (geom_line indicates a line plot).
#We also map our variables "Date" and "% Change" to the x and y coordinates, respectively. 
#In the third line we use facet_wrap to create separate plots for each company (using the variable "Symbol").
#This is why we re-formatted the data in step 6; with facet_wrap we can easily create separate plots,
#where each plot corresponds to a set of observations that have the same value within a specified column. 
#Lastly, we specify that we want the x-axis labels to be three-letter abbreviations for the month.

ggplot(data = new_prices) +
  geom_line(aes(x = Date, y = `% Change`)) +
  facet_wrap("Symbol", nrow  = 2) +
  scale_x_date(date_labels = "%b")


#COMPUTE THE STANDARD DEVIATION OF THE PERCENT CHANGES
#We group the data by company, and then we apply the standard deviation calcluation to each group. 
#We then order the data so that the companies with the highest standard deviation will be listed at the top.

new_prices %>%
  group_by(Symbol) %>%
  summarize(`Standard Deviation of % Change` = sd(`% Change`)) %>%
  arrange(desc(`Standard Deviation of % Change`))