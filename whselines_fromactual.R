knitr::opts_chunk$set(message = FALSE)
packages <- c('useful', 'coefplot', 'xgboost', 'here', 'magrittr', 'dygraphs', 'dplyr', 'RMySQL', 'caret', 'purrr', 'randomForest', 'rpart', 'neuralnet', 'tictoc', 'tinytex', 'DT', 'partykit', 'rpart.plot','rattle')
purrr::walk(packages, library, character.only = TRUE)
#lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
#dev
mychannel <- dbConnect(MySQL(), user="bentley", pass="dave41", host="127.0.0.1")

#NY Server Prod
#mychannel <- dbConnect(MySQL(), user="root", pass="dave41", host="127.0.0.1")

#Google Prod
#mychannel <- dbConnect(MySQL(), user="bentley", pass="dave41", host="104.154.153.225")

query <- function(...) dbGetQuery(mychannel, ...)

var_whse <- 7
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
                  lines_lines AS WHSLINES
                  FROM
                  printvis.whslinesshipped
                  JOIN
                  printvis.workdayofweek ON lines_date = workday_date
                      LEFT JOIN
                  printvis.whslines_outlierdates ON outlier_whse = whse
                  AND outlier_date = lines_date
                  WHERE
                  whse =  ",var_whse," 
                  and (workday_befvac + workday_aftvac + workday_befchrist + workday_aftchrist) = 0
                  and YEAR(workday_date) >= 2015
                  AND outlier_date IS NULL", sep = "")
data <- query(sqlquery)


set.seed(220)
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
                       early_stopping_rounds=100, nrounds = 100000, num_parallel_tree=20, print_every_n = 20, nthread=8,eta = .1, max_depth = 5)

dataTest.xgb <- build.x(data_formula_boxes, data=dataTest, contrasts = FALSE, sparse = TRUE)

prediction.xgb <- predict(model.xgb, newdata = dataTest.xgb)



dataTest$forecastlines <- predict(model.xgb,newdata = dataTest.xgb)
dataTest$delta <- dataTest$forecastlines - dataTest$WHSLINES
dataTest$error_rate <- (dataTest$forecastlines - dataTest$WHSLINES)/dataTest$WHSLINES
dataTest$error_rate_abs <- abs((dataTest$forecastlines - dataTest$WHSLINES)/dataTest$WHSLINES)
currentDate <- Sys.Date()
csvFileName <- paste("forecast_whselines_0",var_whse,"_",currentDate,".csv",sep="")
write.csv(dataTest, file=csvFileName)
lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
