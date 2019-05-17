
knitr::opts_chunk$set(message = FALSE)
packages <- c('useful', 'coefplot', 'xgboost', 'here', 'magrittr', 'dygraphs', 'dplyr', 'RMySQL', 'caret', 'purrr', 'randomForest', 'rpart', 'neuralnet', 'tictoc', 'tinytex', 'DT', 'partykit', 'rpart.plot','rattle')
purrr::walk(packages, library, character.only = TRUE)
#lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
source('connections.R')

source('RMySQL_Update.R')
var_whse <- 'GB0001'
query <- function(...) dbGetQuery(mychannel, ...)
date_today <- Sys.Date()
date_today <- date_today
date_1week <- date_today + 7

list_tier <- list('FLOW', 'BIN', 'PALL')
for(i in list_tier){
  var_tier <- i

sqlquery <- paste("SELECT 
                      workday_date AS PRED_DATE,
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
                  COUNT(ITEM) AS WHSLINES
                  FROM
                   gillingham.workdayofweek
                  JOIN
                    gillingham.gill_raw ON PICKDATE = workday_date
                  JOIN
                    gillingham.slotmaster ON slotmaster_loc = LOCATION
                  LEFT JOIN gillingham.fcast_dateexcl on exclude_date = workday_date
                  WHERE
                  workday_date <= '2018-12-31'
                  AND slotmaster_tier = '",var_tier,"' 
                  and (workday_befvac + workday_aftvac + workday_befchrist + workday_aftchrist) = 0
                  and exclude_date is null
                  GROUP BY workday_date", sep = "")
data <- query(sqlquery)


set.seed(21)
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
                       early_stopping_rounds=100, nrounds = 100000, num_parallel_tree=20, print_every_n = 20, nthread=8,eta = .01, max_depth = 8)

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
                  gillingham.workdayofweek
                  WHERE
                  workday_date between '2019-01-01' AND '2019-01-31'
                  and (workday_befvac + workday_aftvac + workday_befchrist + workday_aftchrist) = 0
                  ORDER BY workday_date", sep = "")
preddata <- query(sqlquery)

data_new_lines <- build.x(data_formula_boxes, data=preddata, contrasts = FALSE, sparse = TRUE)


sqlquery <- paste("SELECT 
                    0,workday_date, '",var_tier,"' 
                  FROM
                  gillingham.workdayofweek
                  WHERE
                  workday_date BETWEEN '2019-01-01' AND '2019-01-31'
                  and (workday_befvac + workday_aftvac + workday_befchrist + workday_aftchrist) = 0
                  ORDER BY workday_date", sep = "")
forecast_insert <- query(sqlquery)


  
forecast_insert$lines <- predict(model.xgb,newdata = data_new_lines)
forecast_insert$seed02 <- 0
forecast_insert$seed03 <- 0
rmysql_update(mychannel, forecast_insert, 'gillingham.forecast_whselines', verbose = FALSE)
}

lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)


