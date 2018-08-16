
knitr::opts_chunk$set(message = FALSE)
packages <- c('useful', 'coefplot', 'xgboost', 'here', 'magrittr', 'dygraphs', 'dplyr', 'RMySQL', 'caret', 'purrr', 'randomForest', 'rpart', 'neuralnet', 'tictoc', 'tinytex', 'DT', 'partykit', 'rpart.plot','rattle')
purrr::walk(packages, library, character.only = TRUE)
#lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

#dev
#mychannel <- dbConnect(MySQL(), user="bentley", pass="dave41", host="127.0.0.1")


#dev personal computer
mychannel <- dbConnect(MySQL(), user="root", pass="dave41", host="127.0.0.1")

#NY Server Prod
#mychannel <- dbConnect(MySQL(), user="root", pass="dave41", host="127.0.0.1")

#Google Prod
#mychannel <- dbConnect(MySQL(), user="bentley", pass="dave41", host="104.154.153.225")

#source('RMySQL_Update.R')

query <- function(...) dbGetQuery(mychannel, ...)
date_today <- Sys.Date() - 76

date_1week <- date_today + 7


#list_whse <- list(2,3,6,7,9)
list_whse <- list(7)
for(i in list_whse){
  var_whse <- i

var_build <- 1

sqlquery <- paste("SELECT 
    workday_workday AS WORKDAY,
    workday_weekofmon AS MONTHWEEK,
    workday_weekday AS WEEKDAY,
    workday_dayofmon AS MONTHDAY,
    workday_month AS MONTH,
    YEAR(workday_date) AS YEAR,
    workday_befvac AS BEFVAC,
    workday_aftvac AS AFTVAC,
    workday_befchrist AS BEFCHR,
    workday_aftchrist AS AFTCHR,
    CASE
        WHEN item_sell IS NULL THEN 0
        ELSE item_sell
    END AS WHSLINES
FROM
    slotting.workdayofweek
        LEFT JOIN
    slotting.itemforecast ON item_date = workday_date
WHERE
    (item_whse = 7 OR item_whse IS NULL)
        AND (workday_befvac + workday_aftvac + workday_befchrist + workday_aftchrist) = 0
        AND workday_date BETWEEN '2017-05-08' AND '2018-06-04'
ORDER BY workday_date", sep = "")
data <- query(sqlquery)


set.seed(22)
trainIndex <- createDataPartition(data$WHSLINES,
                                  p = .75,
                                  list = FALSE,
                                  times = 1)

dataTrain <- data[ trainIndex,]
dataTest  <- data[-trainIndex,]

data_formula_boxes <- WHSLINES ~ WORKDAY + MONTHWEEK + WEEKDAY + MONTHDAY + MONTH + YEAR + BEFVAC + AFTVAC + BEFCHR + AFTCHR


#boxes data training
dataX_Train_box <- build.x(data_formula_boxes, data=dataTrain,
                           contrasts=FALSE,
                           sparse=TRUE)
dataY_Train_box <- build.y(data_formula_boxes,data=dataTrain)

dataX_Test_box <- build.x(data_formula_boxes, data=dataTest,
                          contrasts=FALSE,
                          sparse=TRUE)
dataY_Test_box <- build.y(data_formula_boxes,data=dataTest)

xgTrain_box <- xgb.DMatrix(data=dataX_Train_box,
                           label=dataY_Train_box)

xgVal_box <- xgb.DMatrix(data=dataX_Test_box,
                         label=dataY_Test_box)

model.xgb <- xgb.train(data=xgTrain_box, objective='reg:linear', eval_metric='rmse', booster='gbtree', watchlist = list(train=xgTrain_box, validate=xgVal_box),
                       early_stopping_rounds=100, nrounds = 100000, num_parallel_tree=20, print_every_n = 20, nthread=8,eta = .01, max_depth = 5)

dataTest.xgb <- build.x(data_formula_boxes, data=dataTest, contrasts = FALSE, sparse = TRUE)

prediction.xgb <- predict(model.xgb, newdata = dataTest.xgb)



dataTest$forecastlines <- predict(model.xgb,newdata = dataTest.xgb)
dataTest$delta <- dataTest$forecastlines - dataTest$WHSLINES
dataTest$error_rate <- (dataTest$forecastlines - dataTest$WHSLINES)/dataTest$WHSLINES
dataTest$error_rate_abs <- abs((dataTest$forecastlines - dataTest$WHSLINES)/dataTest$WHSLINES)
currentDate <- Sys.Date()
csvFileName <- paste("forecast_whselines_0",var_whse,"_",currentDate,".csv",sep="")
write.csv(dataTest, file=csvFileName)


sqlquery <- paste("SELECT 
                  workday_workday AS WORKDAY,
                  workday_weekofmon AS MONTHWEEK,
                  workday_weekday AS WEEKDAY,
                  workday_dayofmon AS MONTHDAY,
                  workday_month AS MONTH,
                  YEAR(workday_date) AS YEAR,
                  workday_befvac AS BEFVAC,
                  workday_aftvac AS AFTVAC,
                  workday_befchrist AS BEFCHR,
                  workday_aftchrist AS AFTCHR,
                  0 as WHSLINES
                  FROM
                  slotting.workdayofweek
                  WHERE
                  workday_date between '",date_today,"' AND '",date_1week,"'
                  ORDER BY workday_date", sep = "")
preddata <- query(sqlquery)

data_new_lines <- build.x(data_formula_boxes, data=preddata, contrasts = FALSE, sparse = TRUE)


sqlquery <- paste("SELECT 
                    ",var_whse,",workday_date
                  FROM
                  slotting.workdayofweek
                  WHERE
                  workday_date BETWEEN  '",date_today,"' AND '",date_1week,"'
                  ORDER BY workday_date", sep = "")
forecast_insert <- query(sqlquery)


  
forecast_insert$lines <- predict(model.xgb,newdata = data_new_lines)
forecast_insert$seed02 <- 0
forecast_insert$seed03 <- 0
rmysql_update(mychannel, forecast_insert, 'printvis.forecast_whselines', verbose = FALSE)
}
lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)


