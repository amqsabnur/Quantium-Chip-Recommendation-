#title: "Quantium Virtual Internship - Retail Strategy and Analytics - Task 2"

## Load required libraries and datasets

library(data.table)
library(ggplot2)
library(tidyr)

#### assign the data files to data.tables
data <- fread(paste0("data.csv"))

#### Set themes for plots
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

#### Add a new month ID column in the data with the format yyyymm.
monthID <- data[, YEARMONTH := year(DATE)*100 + month(DATE)]

# For each store and month calculate total sales, number of customers,
#transactions per customer, chips per customer and the average price per unit.
measureOverTime <- data[, .(totSales = sum(TOT_SALES), nCustomers = uniqueN(LYLTY_CARD_NBR), 
nTxnPerCust = uniqueN(TXN_ID)/uniqueN(LYLTY_CARD_NBR), nChipsPerTxn = sum(PROD_QTY)/uniqueN(TXN_ID), 
avgPricePerUnit = sum(TOT_SALES)/sum(PROD_QTY)), by = c("STORE_NBR", "YEARMONTH")][order(STORE_NBR, YEARMONTH)]

#### Filter to the pre-trial period and stores with full observation periods
storesWithFullObs <- unique(measureOverTime[, .N, STORE_NBR][N == 12, STORE_NBR])
preTrialMeasures <- measureOverTime[YEARMONTH < 201902 & STORE_NBR %in%
storesWithFullObs, ]


#### Create a function to calculate correlation for a measure, looping through each control store.
#### inputTable as a metric table with potential comparison stores,
#### metricCol as the store metric used to calculate correlation on, 
#### storeComparison as the store number of the trial store.

calculateCorrelation <- function(inputTable, metricCol, storeComparison) {
  calcCorrTable = data.table(Store1 = numeric(), Store2 = numeric(), corr_measure = numeric())
  storeNumbers <- unique(inputTable [,STORE_NBR])
    
    for (i in storeNumbers) {
    calculatedMeasure = data.table("Store1" = storeComparison, "Store2" = i,
    "corr_measure" = cor(inputTable[STORE_NBR == storeComparison,
    eval(metricCol)], inputTable[STORE_NBR == i, eval(metricCol)]))
    
    calcCorrTable <- rbind(calcCorrTable, calculatedMeasure)
    }
  return(calcCorrTable)
}

#### Create a function to calculate a standardised magnitude distance for a measure,
#### looping through each control store

calculateMagnitudeDistance <- function(inputTable, metricCol, storeComparison) {
calcDistTable = data.table(Store1 = numeric(), Store2 = numeric(), YEARMONTH =
numeric(), measure = numeric())

storeNumbers <- unique(inputTable[, STORE_NBR])

for (i in storeNumbers) {
 calculatedMeasure = data.table("Store1" = storeComparison, "Store2" = i,
 "YEARMONTH" = inputTable[STORE_NBR == storeComparison, YEARMONTH], 
 "measure" = abs(inputTable[STORE_NBR == storeComparison, eval(metricCol)]
 - inputTable[STORE_NBR == i,eval(metricCol)]) )
 calcDistTable <- rbind(calcDistTable, calculatedMeasure)
 }

#### Standardise the magnitude distance so that the measure ranges from 0 to 1
 minMaxDist <- calcDistTable[, .(minDist = min(measure), maxDist = max(measure)),
by = c("Store1", "YEARMONTH")]
 distTable <- merge(calcDistTable, minMaxDist, by = c("Store1", "YEARMONTH"))
 distTable[, magnitudeMeasure := 1 - (measure - minDist)/(maxDist - minDist)]

 finalDistTable <- distTable[, .(mag_measure = mean(magnitudeMeasure)), by =
.(Store1, Store2)]
 return(finalDistTable)
}

#### Use the function you created to calculate correlations against store 
####77 using total sales and number of customers.

trial_store <- 77
corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totSales),trial_store)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers),trial_store )

#### Then, use the functions for calculating magnitude.
magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales),
trial_store)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures,
quote(nCustomers), trial_store)

####  Create a combined score composed of correlation and magnitude, by
####first merging the correlations table with the magnitude table.
#### Hint: A simple average on the scores would be 0.5 * corr_measure + 0.5 * mag_measure

corr_weight <- 0.5
score_nSales <- merge(corr_nSales, magnitude_nSales, by = c("Store1","Store2"))[, scoreNSales 
                              := corr_measure * corr_weight + mag_measure * (1-corr_weight)]

score_nCustomers <- merge(corr_nCustomers, magnitude_nCustomers, by =c("Store1", "Store2"))[, 
                  scoreNCust := corr_measure * corr_weight +mag_measure * (1- corr_weight)]

#### Combine scores across the drivers by first merging our sales scores and customer scores 
#### into a single table
score_Control <- merge(score_nSales,score_nCustomers , by = c("Store1", "Store2")  )
score_Control[, finalControlScore := scoreNSales * 0.5 + scoreNCust * 0.5]

#### Select control stores based on the highest matching store (closest to 1 but
#### not the store itself, i.e. the second ranked highest store)
#### sekect the most appropriate control store for trial store 77 by
#### finding the store with the highest final score.
control_store <- score_Control[Store1 == trial_store,][order(-finalControlScore)][2, Store2]
control_store

#### Visual checks on trends based on the drivers
measureOverTimeSales <- measureOverTime
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store,
            "Trial", ifelse(STORE_NBR == control_store, "Control", "Other stores"))][, 
            totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
            ][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), 
            "%Y-%m-%d")][YEARMONTH < 201903 , ]
ggplot(pastSales, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_line() + labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")

####  Conduct visual checks on customer count trends by comparing the
####trial store to the control store and other stores.

measureOverTimeCusts <- measureOverTime
pastCustomers <- measureOverTimeCusts[, Store_type := ifelse(STORE_NBR ==
                trial_store, "Trial", ifelse(STORE_NBR == control_store,
                "Control", "Other stores"))][, numberCustomers := mean(nCustomers), by =
                c("YEARMONTH", "Store_type")][, TransactionMonth := as.Date(paste(YEARMONTH %/%
                100, YEARMONTH %% 100, 1, sep = "‐"), "%Y‐%m‐%d")][YEARMONTH < 201903 , ]

ggplot(pastCustomers, aes(TransactionMonth, numberCustomers, color = Store_type)) +
                geom_line() + labs(x = "Month of operation", y = "Total number of customers", 
                title = "Totalnumber of customers by month")

#### Scale pre-trial control sales to match pre-trial trial store sales
scalingFactorForControlSales <- preTrialMeasures[STORE_NBR == trial_store &
YEARMONTH < 201902, sum(totSales)]/preTrialMeasures[STORE_NBR == control_store &
YEARMONTH < 201902, sum(totSales)]

#### Apply the scaling factor
measureOverTimeSales <- measureOverTime
scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store, ][ ,
controlSales := totSales * scalingFactorForControlSales]

####  Calculate the percentage difference between scaled control sales and trial sales
percentageDiff <- merge(scaledControlSales[, c("YEARMONTH", "controlSales")],
                  measureOverTime[STORE_NBR == trial_store, c("totSales","YEARMONTH")],
                  by = "YEARMONTH")[, percentageDiff :=abs(controlSales-totSales)/controlSales]
                 
#### As our null hypothesis is that the trial period is the same as the pre-trial period
####take the standard deviation based on the scaled percentage difference in the pre-trial period
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff])

#### Note that there are 8 months in the pre-trial period
#### hence 8 - 1 = 7 degrees of freedom
degreesOfFreedom <- 7

#### to check whether the hypothesis is statistically significant.
#### Hint: The test statistic here is (x - u)/standard deviation
percentageDiff[, tValue := (percentageDiff - 0)/stdDev
][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100,
YEARMONTH %% 100, 1, sep = "‐"), "%Y‐%m‐%d")
][YEARMONTH < 201905 & YEARMONTH > 201901, .(TransactionMonth,
tValue)]

measureOverTimeSales <- measureOverTime

#### Trial and control store total sales
####Create new variables Store_type, totSales and TransactionMonth in the data table.

pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR ==
             trial_store, "Trial",ifelse(STORE_NBR == control_store,
             "Control", "Other stores"))][, totSales := mean(totSales), 
             by = c("YEARMONTH","Store_type")][, TransactionMonth := as.Date(paste(YEARMONTH %/%
            100, YEARMONTH %% 100, 1, sep = "‐"), "%Y‐%m‐%d")][Store_type %in% c("Trial", "Control"), ]
#### Control store 95th percentile
pastSales_Controls95 <- pastSales[Store_type == "Control",
 ][, totSales := totSales * (1 + stdDev * 2)
 ][, Store_type := "Control 95th % confidence interval"]
#### Control store 5th percentile
pastSales_Controls5 <- pastSales[Store_type == "Control",
 ][, totSales := totSales * (1 - stdDev * 2)
 ][, Store_type := "Control 5th % confidence
       interval"]
trialAssessment <- rbind(pastSales, pastSales_Controls95, pastSales_Controls5)
#### Plotting these in one nice graph
ggplot(trialAssessment, aes(TransactionMonth, totSales, color = Store_type)) +
 geom_rect(data = trialAssessment[ YEARMONTH < 201905 & YEARMONTH > 201901 ,],
aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0 , ymax =
Inf, color = NULL), show.legend = FALSE) +
 geom_line() +
 labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")

####The results show that the trial in store 77 is significantly different to its
####control store in the trial period as the trial store performance lies outside the
#### 5% to 95% confidence interval of the control store in two of the three trial months.

#### Scale pre-trial control customers to match pre-trial trial store customers
#### Compute a scaling factor to align control store customer countsto our trial store.
#### Then, apply the scaling factor to control store customer counts.
#### Finally, calculate the percentage difference between scaled control store
#### customers and trial customers.

scalingFactorForControlCust <- preTrialMeasures[STORE_NBR == trial_store &
                            YEARMONTH < 201902, sum(nCustomers)]/preTrialMeasures[STORE_NBR ==
                            control_store & YEARMONTH < 201902, sum(nCustomers)]
measureOverTimeCusts <- measureOverTime
scaledControlCustomers <- measureOverTimeCusts[STORE_NBR == control_store,
                          ][ , controlCustomers := nCustomers * scalingFactorForControlCust][, 
                          Store_type:= ifelse(STORE_NBR ==trial_store, 
                          "Trial",ifelse(STORE_NBR == control_store,"Control", "Other stores"))]

percentageDiff <- merge(scaledControlCustomers[, c("YEARMONTH", "controlCustomers")],
                        measureOverTimeCusts[STORE_NBR == trial_store,
                        c("nCustomers", "YEARMONTH")], by = "YEARMONTH")[, percentageDiff :=
                        abs(controlCustomers-nCustomers)/controlCustomers]


#### As our null hypothesis is that the trial period is the same as the pre-trial
#### period, let's take the standard deviation based on the scaled percentage difference
#### in the pre-trial period
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff])
degreesOfFreedom <- 7
#### Trial and control store number of customers
pastCustomers <- measureOverTimeCusts[, nCusts := mean(nCustomers), by =
c("YEARMONTH", "Store_type")
 ][Store_type %in% c("Trial", "Control"), ]
#### Control store 95th percentile
pastCustomers_Controls95 <- pastCustomers[Store_type == "Control",
 ][, nCusts := nCusts * (1 + stdDev * 2)
 ][, Store_type := "Control 95th % confidence interval"]
#### Control store 5th percentile
pastCustomers_Controls5 <- pastCustomers[Store_type == "Control",
 ][, nCusts := nCusts * (1 - stdDev * 2)
 ][, Store_type := "Control 5th % confidence
       interval"]
trialAssessment <- rbind(pastCustomers, pastCustomers_Controls95,
pastCustomers_Controls5)

####  Plot everything into one nice graph.
#### Hint: geom_rect creates a rectangle in the plot. Use this to highlight the
#### trial period in our graph.
ggplot(trialAssessment, aes(TransactionMonth, nCusts, color = Store_type)) +
geom_rect(data = trialAssessment[ YEARMONTH < 201905 & YEARMONTH > 201901 ,],
aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0 ,
ymax = Inf, color = NULL), show.legend = FALSE) + geom_line() +
labs(x = "Month of operation", y = "Total number of customers", title = "Total
number of customers by month")

####Calculate the metrics below as we did for the first trial store.
measureOverTime <- data[, .(totSales = sum(TOT_SALES),
                            nCustomers = uniqueN(LYLTY_CARD_NBR), 
                            nTxnPerCust = uniqueN(TXN_ID)/uniqueN(LYLTY_CARD_NBR), 
                            nChipsPerTxn = sum(PROD_QTY)/uniqueN(TXN_ID), 
                            avgPricePerUnit = sum(TOT_SALES)/sum(PROD_QTY)), 
                        by = c("STORE_NBR", "YEARMONTH")][order(STORE_NBR, YEARMONTH)]

#### Use the functions we created earlier to calculate correlations
#### and magnitude for each potential control store
trial_store <- 86
corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totSales),trial_store)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store)
magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures,quote(totSales), trial_store)
magnitude_nCustomers <-calculateMagnitudeDistance(preTrialMeasures,quote(nCustomers), trial_store)

#### Now, create a combined score composed of correlation and magnitude
corr_weight <- 0.5
score_nSales <- merge(corr_nSales, magnitude_nSales, by = c("Store1","Store2"))[, scoreNSales 
                := corr_measure * corr_weight + mag_measure * (1-corr_weight)]

score_nCustomers <- merge(corr_nCustomers, magnitude_nCustomers, by =c("Store1", "Store2"))[, 
                 scoreNCust := corr_measure * corr_weight +mag_measure * (1- corr_weight)]

#### Combine scores across the drivers by first merging our sales scores and customer scores 
#### into a single table
score_Control <- merge(score_nSales,score_nCustomers , by = c("Store1", "Store2")  )
score_Control[, finalControlScore := scoreNSales * 0.5 + scoreNCust * 0.5]

#### Select control stores based on the highest matching store (closest to 1 but
#### not the store itself, i.e. the second ranked highest store)
#### sekect the most appropriate control store for trial store 77 by
#### finding the store with the highest final score.
control_store <- score_Control[Store1 == trial_store,][order(-finalControlScore)][2, Store2]
control_store

####Looks like store 155 will be a control store for trial store 86.
#### Conduct visual checks on trends based on the drivers
measureOverTimeSales <- measureOverTime
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store,
             "Trial", ifelse(STORE_NBR == control_store, "Control", "Other stores"))][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
              ][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), 
               "%Y-%m-%d")][YEARMONTH < 201903 , ]
ggplot(pastSales, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_line() + labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")

####  Conduct visual checks on customer count trends by comparing the
####trial store to the control store and other stores.

measureOverTimeCusts <- measureOverTime
pastCustomers <- measureOverTimeCusts[, Store_type := ifelse(STORE_NBR ==
                trial_store, "Trial", ifelse(STORE_NBR == control_store,
                "Control", "Other stores"))][, numberCustomers := mean(nCustomers), by =
                c("YEARMONTH", "Store_type")][, TransactionMonth := as.Date(paste(YEARMONTH %/%
                100, YEARMONTH %% 100, 1, sep = "‐"), "%Y‐%m‐%d")
                ][YEARMONTH < 201903 , ]     

ggplot(pastCustomers, aes(TransactionMonth, numberCustomers, color = Store_type)) +
  geom_line() + labs(x = "Month of operation", y = "Total number of customers", 
                     title = "Totalnumber of customers by month")

#### Scale pre-trial control sales to match pre-trial trial store sales
scalingFactorForControlSales <- preTrialMeasures[STORE_NBR == trial_store &
YEARMONTH < 201902, sum(totSales)]/preTrialMeasures[STORE_NBR == control_store &
YEARMONTH < 201902, sum(totSales)]
#### Apply the scaling factor
measureOverTimeSales <- measureOverTime
scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store, ][ ,
controlSales := totSales * scalingFactorForControlSales]

#### Calculate the percentage difference between scaled control sales and trial sales
#### Hint: When calculating percentage difference, remember to use absolute difference

percentageDiff <- merge(scaledControlSales[, c("YEARMONTH", "controlSales")],
                 measureOverTime[STORE_NBR == trial_store, c("totSales","YEARMONTH")],
                 by = "YEARMONTH")[, percentageDiff :=abs(controlSales-totSales)/controlSales]

#### As our null hypothesis is that the trial period is the same as the pre-trial period
####take the standard deviation based on the scaled percentage difference in the pre-trial period
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff])
degreesOfFreedom <- 7
measureOverTimeSales <- measureOverTime
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR ==
            trial_store, "Trial",ifelse(STORE_NBR == control_store,
            "Control", "Other stores"))][, totSales := mean(totSales), 
            by = c("YEARMONTH","Store_type")][, TransactionMonth := as.Date(paste(YEARMONTH %/%                                                                                                                                                                                         100, YEARMONTH %% 100, 1, sep = "‐"), "%Y‐%m‐%d")][Store_type %in% c("Trial", "Control"), ]
#### Control store 95th percentile
pastSales_Controls95 <- pastSales[Store_type == "Control",
][, totSales := totSales * (1 + stdDev * 2)
][, Store_type := "Control 95th % confidence interval"]
#### Control store 5th percentile
pastSales_Controls5 <- pastSales[Store_type == "Control",
][, totSales := totSales * (1 - stdDev * 2)
][, Store_type := "Control 5th % confidence
       interval"]
trialAssessment <- rbind(pastSales, pastSales_Controls95, pastSales_Controls5)
#### Plotting these in one nice graph
ggplot(trialAssessment, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_rect(data = trialAssessment[ YEARMONTH < 201905 & YEARMONTH > 201901 ,],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0 , ymax =
                  Inf, color = NULL), show.legend = FALSE) +
  geom_line() +
  labs(x = "Month of operation", y = "Total Total number of customers", 
       title = "Total number of customers by month")

## Trial store 88
measureOverTime <- data[, .(totSales = sum(TOT_SALES),
                            nCustomers = uniqueN(LYLTY_CARD_NBR), 
                            nTxnPerCust = uniqueN(TXN_ID)/uniqueN(LYLTY_CARD_NBR), 
                            nChipsPerTxn = sum(PROD_QTY)/uniqueN(TXN_ID), 
                            avgPricePerUnit = sum(TOT_SALES)/sum(PROD_QTY)), 
                        by = c("STORE_NBR", "YEARMONTH")][order(STORE_NBR, YEARMONTH)]

#### Use the functions from earlier to calculate the correlation of the sales and
#### number of customers of each potential control store to the trial store
trial_store <- 88
corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totSales),trial_store)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store)
magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures,quote(totSales), trial_store)
magnitude_nCustomers <-calculateMagnitudeDistance(preTrialMeasures,quote(nCustomers), trial_store)

#### Now, create a combined score composed of correlation and magnitude
corr_weight <- 0.5
score_nSales <- merge(corr_nSales, magnitude_nSales, by = c("Store1","Store2"))[, scoreNSales 
                                                                                := corr_measure * corr_weight + mag_measure * (1-corr_weight)]

score_nCustomers <- merge(corr_nCustomers, magnitude_nCustomers, by =c("Store1", "Store2"))[, 
                                                                                            scoreNCust := corr_measure * corr_weight +mag_measure * (1- corr_weight)]

#### Combine scores across the drivers by first merging our sales scores and customer scores 
#### into a single table
score_Control <- merge(score_nSales,score_nCustomers , by = c("Store1", "Store2")  )
score_Control[, finalControlScore := scoreNSales * 0.5 + scoreNCust * 0.5]

#### Select control stores based on the highest matching store (closest to 1 but
#### not the store itself, i.e. the second ranked highest store)
#### sekect the most appropriate control store for trial store 77 by
#### finding the store with the highest final score.
control_store <- score_Control[Store1 == trial_store,][order(-finalControlScore)][2, Store2]
control_store

#### We've now found store 237 to be a suitable control store for trial store 88.
#### Conduct visual checks on trends based on the drivers
measureOverTimeSales <- measureOverTime
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR == trial_store,
                                                         "Trial", ifelse(STORE_NBR == control_store, "Control", "Other stores"))][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
                                                         ][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), 
                                                                                         "%Y-%m-%d")][YEARMONTH < 201903 , ]
ggplot(pastSales, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_line() + labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")

####  Conduct visual checks on customer count trends by comparing the
####trial store to the control store and other stores.

measureOverTimeCusts <- measureOverTime
pastCustomers <- measureOverTimeCusts[, Store_type := ifelse(STORE_NBR ==
                                                               trial_store, "Trial", ifelse(STORE_NBR == control_store,
                                                                                            "Control", "Other stores"))][, numberCustomers := mean(nCustomers), by =
                                                                                                                           c("YEARMONTH", "Store_type")][, TransactionMonth := as.Date(paste(YEARMONTH %/%
                                                                                                                                                                                               100, YEARMONTH %% 100, 1, sep = "‐"), "%Y‐%m‐%d")
                                                                                                                           ][YEARMONTH < 201903 , ]     

ggplot(pastCustomers, aes(TransactionMonth, numberCustomers, color = Store_type)) +
  geom_line() + labs(x = "Month of operation", y = "Total number of customers", 
                     title = "Totalnumber of customers by month")

#### Scale pre-trial control sales to match pre-trial trial store sales
scalingFactorForControlSales <- preTrialMeasures[STORE_NBR == trial_store &
                                                   YEARMONTH < 201902, sum(totSales)]/preTrialMeasures[STORE_NBR == control_store &
                                                                                                         YEARMONTH < 201902, sum(totSales)]
#### Apply the scaling factor
measureOverTimeSales <- measureOverTime
scaledControlSales <- measureOverTimeSales[STORE_NBR == control_store, ][ ,
                                                                          controlSales := totSales * scalingFactorForControlSales]

#### Calculate the percentage difference between scaled control sales and trial sales
#### Hint: When calculating percentage difference, remember to use absolute difference

percentageDiff <- merge(scaledControlSales[, c("YEARMONTH", "controlSales")],
                        measureOverTime[STORE_NBR == trial_store, c("totSales","YEARMONTH")],
                        by = "YEARMONTH")[, percentageDiff :=abs(controlSales-totSales)/controlSales]

#### As our null hypothesis is that the trial period is the same as the pre-trial period
####take the standard deviation based on the scaled percentage difference in the pre-trial period
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff])
degreesOfFreedom <- 7
measureOverTimeSales <- measureOverTime
pastSales <- measureOverTimeSales[, Store_type := ifelse(STORE_NBR ==
                                                           trial_store, "Trial",ifelse(STORE_NBR == control_store,
                                                                                       "Control", "Other stores"))][, totSales := mean(totSales), 
                                                                                                                    by = c("YEARMONTH","Store_type")][, TransactionMonth := as.Date(paste(YEARMONTH %/%                                                                                                                                                                                         100, YEARMONTH %% 100, 1, sep = "‐"), "%Y‐%m‐%d")][Store_type %in% c("Trial", "Control"), ]
#### Control store 95th percentile
pastSales_Controls95 <- pastSales[Store_type == "Control",
][, totSales := totSales * (1 + stdDev * 2)
][, Store_type := "Control 95th % confidence interval"]
#### Control store 5th percentile
pastSales_Controls5 <- pastSales[Store_type == "Control",
][, totSales := totSales * (1 - stdDev * 2)
][, Store_type := "Control 5th % confidence
       interval"]
trialAssessment <- rbind(pastSales, pastSales_Controls95, pastSales_Controls5)
#### Plotting these in one nice graph
ggplot(trialAssessment, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_rect(data = trialAssessment[ YEARMONTH < 201905 & YEARMONTH > 201901 ,],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0 , ymax =
                  Inf, color = NULL), show.legend = FALSE) +
  geom_line() +
  labs(x = "Month of operation", y = "Total Total number of customers", 
       title = "Total number of customers by month")

####The results show that the trial in store 88 is significantly different to its
####control store in the trial period as the trial store performance lies outside of
####the 5% to 95% confidence interval of the control store in two of the three trial months.

#### This would be a repeat of the steps before for total sales
#### Scale pre-trial control store customers to match pre-trial trial store customers
scalingFactorForControlCust <- preTrialMeasures[STORE_NBR == trial_store &
                                                  YEARMONTH < 201902, sum(nCustomers)]/preTrialMeasures[STORE_NBR ==
                                                                                                          control_store & YEARMONTH < 201902, sum(nCustomers)]
measureOverTimeCusts <- measureOverTime
scaledControlCustomers <- measureOverTimeCusts[STORE_NBR == control_store,
][ , controlCustomers := nCustomers * scalingFactorForControlCust][, 
                                                                   Store_type:= ifelse(STORE_NBR ==trial_store, 
                                                                                       "Trial",ifelse(STORE_NBR == control_store,"Control", "Other stores"))]

percentageDiff <- merge(scaledControlCustomers[, c("YEARMONTH", "controlCustomers")],
                        measureOverTimeCusts[STORE_NBR == trial_store,
                                             c("nCustomers", "YEARMONTH")], by = "YEARMONTH")[, percentageDiff :=
                                                                                                abs(controlCustomers-nCustomers)/controlCustomers]


#### As our null hypothesis is that the trial period is the same as the pre-trial
#### period, let's take the standard deviation based on the scaled percentage difference
#### in the pre-trial period
stdDev <- sd(percentageDiff[YEARMONTH < 201902 , percentageDiff])
degreesOfFreedom <- 7
#### Trial and control store number of customers
pastCustomers <- measureOverTimeCusts[, nCusts := mean(nCustomers), by =
                                        c("YEARMONTH", "Store_type")
][Store_type %in% c("Trial", "Control"), ]
#### Control store 95th percentile
pastCustomers_Controls95 <- pastCustomers[Store_type == "Control",
][, nCusts := nCusts * (1 + stdDev * 2)
][, Store_type := "Control 95th % confidence interval"]
#### Control store 5th percentile
pastCustomers_Controls5 <- pastCustomers[Store_type == "Control",
][, nCusts := nCusts * (1 - stdDev * 2)
][, Store_type := "Control 5th % confidence
       interval"]
trialAssessment <- rbind(pastCustomers, pastCustomers_Controls95,
                         pastCustomers_Controls5)

####  Plot everything into one nice graph.
#### Hint: geom_rect creates a rectangle in the plot. Use this to highlight the
#### trial period in our graph.
ggplot(trialAssessment, aes(TransactionMonth, nCusts, color = Store_type)) +
  geom_rect(data = trialAssessment[ YEARMONTH < 201905 & YEARMONTH > 201901 ,],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0 ,
                ymax = Inf, color = NULL), show.legend = FALSE) + geom_line() +
  labs(x = "Month of operation", y = "Total number of customers", title = "Total
number of customers by month")

###Total number of customers in the trial period for the trial store is significantly
###higher than the control store for two out of three months, which indicates a
###positive trial effect.

## Conclusion
##### We've found control stores 233, 155, 237 for trial stores 77, 86 and 88 respectively.
#### The results for trial stores 77 and 88 during the trial period show a significant
#### difference in at least two of the three trial months but this is not the case for
#### trial store 86. We can check with the client if the implementation of the trial was
#### different in trial store 86 but overall, the trial shows a significant increase in
#### sales. Now that we have finished our analysis, we can prepare our presentation to
#### the Category Manager.
