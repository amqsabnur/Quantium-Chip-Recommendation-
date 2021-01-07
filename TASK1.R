##Fardous Sabnur
# code for Quantium Virtual Internship

#### Load required libraries
library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)
library(dplyr)
#### assign the data files to data.tables
transactionData <- fread(paste0("QVI_transaction_data.csv"))
customerData <- fread(paste0("QVI_purchase_behaviour.csv"))

## Exploratory data analysis
#### Examine transaction data
transactionData       ##gives some sample data
head(transactionData) ##gives the first 10 rows
str(transactionData)  ## gives the format for eeach column and some sample data

##### the date column is in an integer format, Convert DATE column to a date format
#### CSV and Excel integer dates begin on 30 Dec 1899
transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30")
str(transactionData$DATE) #check if it was converted successfully

#### Examine PROD_NAME
str(transactionData$PROD_NAME)

#text analysis 
#### Examine products that are not chips

productWords <- data.table(unlist(strsplit(unique(transactionData[, PROD_NAME]), " ")))
setnames(productWords, 'words')

#remove all words with digits and special characters such as '&' from our set of product words
#We can do this using `grepl()`.
# Over to you! Remove digits, and special characters, and then sort the distinct 
#words by frequency of occurrence.
#### Removing digits
productWords <- gsub('[[:digit:]]+', " ", productWords)
str(productWords)
#### Removing special characters
productWords <- gsub("[[:punct:]]", " ", productWords)
str(productWords)
#### Let's look at the most common words by counting the number of times a word appears and
#### sorting them by this frequency in order of highest to lowest frequency
sort(table(productWords), decreasing=TRUE) 

#### Remove salsa products
transactionData[, SALSA := grepl("salsa", tolower(PROD_NAME))]
transactionData <- transactionData[SALSA == FALSE, ][, SALSA := NULL]

#### Summarise the data to check for nulls and possible outliers
summary(transactionData)

#### Filter the dataset to find the outlier
filter(transactionData, transactionData$PROD_QTY == 200.000)

# Over to you! Use a filter to see what other transactions that customer made.
filter(transactionData, transactionData$LYLTY_CARD_NBR == 226000)

#### Filter out the customer based on the loyalty card number
transactionData <- subset(transactionData, LYLTY_CARD_NBR != 226000)
#### Re-examine transaction data
summary(transactionData)

#### Count the number of transactions by date
# Over to you! Create a summary of transaction count by date.
transactionData[, .N, by = DATE]

#### Create a sequence of dates and join this the count of transactions by date
allDates <- data.table(seq(as.Date("2018/07/01"), as.Date("2019/06/30"), by = "day"))
setnames(allDates, "DATE")
transactions_by_day <- merge(allDates, transactionData[, .N, by = DATE], all.x = TRUE) #.N gives frequency

#### Setting plot themes to format graphs
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

#### Plot transactions over time
ggplot(transactions_by_day, aes(x = DATE, y = N)) + geom_line() +
 labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
 scale_x_date(breaks = "1 month") +
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#### Filter to December and look at individual days
ggplot(transactions_by_day[month(DATE) ==12], aes(x = DATE, y = N)) + geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#### Pack size
#### We can work this out by taking the digits that are in PROD_NAME
transactionData[, PACK_SIZE := parse_number(PROD_NAME)]

####  pack sizes with frequency and ranked by order of size
transactionData[, .N, PACK_SIZE][order(PACK_SIZE)]

#### plot a histogram of PACK_SIZE since it is a categorical variable and not a continuous variable 
hist(transactionData[, PACK_SIZE], xlab= "Size")


# Over to you! Create a column which contains the brand of the product
#### Checking brands
transactionData$BRAND <- gsub("([A-Za-z]+).*", "\\1", transactionData$PROD_NAME)
transactionData$BRAND <- toupper(transactionData$BRAND)
transactionData[, .N, by = BRAND]
#### Clean brand names
transactionData[BRAND == "RED", BRAND := "RRD"]
transactionData[BRAND == "SNBTS", BRAND := "SUNBITES"]
transactionData[BRAND == "INFZNS", BRAND := "INFUZIONS"]
transactionData[BRAND == "WW", BRAND := "WOOLWORTHS"]
transactionData[BRAND == "SMITH", BRAND := "SMITHS"]
transactionData[BRAND == "NCC", BRAND := "NATURAL"]
transactionData[BRAND == "DORITO", BRAND := "DORITOS"]
transactionData[BRAND == "GRAIN", BRAND := "GRNWVES"]
transactionData[, .N, by = BRAND]

#### Examining customer data
str(customerData)

#### Merge transaction data to customer data
data <- merge(transactionData, customerData, all.x = TRUE)

#check if some customers were not matched on by checking for nulls.
summary(data)
fwrite(data, paste0("QVI_data.csv"))

#### Total sales by LIFESTAGE and PREMIUM_CUSTOMER
sales <- data[, .(SALES = sum(TOT_SALES)), .(LIFESTAGE, PREMIUM_CUSTOMER)]
#### Create plot
p <- ggplot(data = sales) + geom_mosaic(aes(weight = SALES, x = product(PREMIUM_CUSTOMER, LIFESTAGE),
        fill = PREMIUM_CUSTOMER)) + labs(x = "Lifestage", y = "Premium customer flag", 
        title = "Proportion of sales") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#### Plot and label with proportion of sales
p + geom_text(data = ggplot_build(p)$data[[1]], aes(x = (xmin + xmax)/2 ,
        y = (ymin + ymax)/2, label = as.character(paste(round(.wt/sum(.wt),3)*100,'%'))))

#### Number of customers by LIFESTAGE and PREMIUM_CUSTOMER
customers <- data[, .(CUSTOMERS = uniqueN(LYLTY_CARD_NBR)), .(LIFESTAGE, PREMIUM_CUSTOMER)]

#### Average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMER
p <- ggplot(data = customers) + geom_mosaic(aes(weight = CUSTOMERS, x = product(PREMIUM_CUSTOMER, LIFESTAGE),
        fill = PREMIUM_CUSTOMER)) + labs(x = "Lifestage", y = "Premium customer flag", 
        title = "Proportion of customers") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#### Plot and label with proportion of sales
p + geom_text(data = ggplot_build(p)$data[[1]], aes(x = (xmin + xmax)/2 ,
    y = (ymin + ymax)/2, label = as.character(paste(round(.wt/sum(.wt),3)*100,'%'))))
#Older families and young families in general buy more chips per customer

#### Average price per unit by LIFESTAGE and PREMIUM_CUSTOMER
# Calculate and plot the average price per unit sold 
avg_price <- data[, .(AVG = sum(PROD_QTY)/uniqueN(LYLTY_CARD_NBR)),.(LIFESTAGE, PREMIUM_CUSTOMER)]
ggplot(data = avg_price) + aes(weight = AVG, x = LIFESTAGE,
             fill = PREMIUM_CUSTOMER) + labs(x = "Lifestage", y = "Premium customer flag", 
            title = "unit per customers") + geom_bar(position = position_dodge()) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


#### Perform an independent t-test between mainstream vs premium and budget midage
pricePerUnit <- data[, price := TOT_SALES/PROD_QTY]
t.test(data[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES") &
      PREMIUM_CUSTOMER == "Mainstream", price], 
       data[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES")
      & PREMIUM_CUSTOMER != "Mainstream", price], alternative = "greater")

#The t-test results in a p-value of 2.2e-16, i.e. the unit price for mainstream,
#young and mid-age singles and couples ARE significantly higher than
#that of budget or premium, young and midage singles and couples.
