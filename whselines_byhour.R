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
source('RMySQL_Update.R')
query <- function(...) dbGetQuery(mychannel, ...)
date_today <- Sys.Date()
date_1week <- date_today + 7
list_whse <- list(2,3,6,7,9)
list_equipment <- list('BLUEBIN', 'DOGPOUND','FLOWRACK','FULLPALLET', 'OTHER')
var_build <- 1

for(i in list_whse){
  var_whse <- i

  for(e in list_equipment){
    var_equip <- e
  
sqlquery <- paste("SELECT 
    CASE
        WHEN loosevol_availhour < 6 THEN 6
        WHEN loosevol_availhour > 16 THEN 16
        ELSE loosevol_availhour
    END AS HOUR,
    workday_workday AS WORKDAY,
    workday_weekofmon AS MONTHWEEK,
    workday_weekday AS WEEKDAY,
    workday_dayofmon AS MONTHDAY,
    workday_month AS MONTH,
    YEAR(loosevol_availdate) AS YEAR,
    workday_befvac AS BEFVAC,
    workday_aftvac AS AFTVAC,
    workday_befchrist AS BEFCHR,
    workday_aftchrist AS AFTCHR,
    SUM(loosevol_lines) AS WHSLINES
FROM
    printvis.hist_loosevol_summary
        JOIN
    printvis.workdayofweek ON loosevol_availdate = workday_date
        LEFT JOIN
    printvis.loosevol_outliers on loosevol_whse = looseout_whse and loosevol_availdate = looseout_date and looseout_equip = loosevol_equip and looseout_hour = CASE
        WHEN loosevol_availhour < 6 THEN 6
                  WHEN loosevol_availhour > 16 THEN 16
                  ELSE loosevol_availhour
                  END
WHERE
    loosevol_whse =  ",var_whse," and loosevol_availhour between 6 and 16
        AND loosevol_equip = '",var_equip,"' and loosevol_availdate <  '",date_today,"'
        and looseout_hour is null
GROUP BY loosevol_availdate , CASE
    WHEN loosevol_availhour < 6 THEN 6
    WHEN loosevol_availhour > 16 THEN 16
    ELSE loosevol_availhour
END
ORDER BY loosevol_availdate , HOUR", sep = "")
data <- query(sqlquery)


set.seed(222)
trainIndex <- createDataPartition(data$WHSLINES,
                                  p = .75,
                                  list = FALSE,
                                  times = 1)

dataTrain <- data[ trainIndex,]
dataTest  <- data[-trainIndex,]

data_formula_boxes <- WHSLINES ~ HOUR + WORKDAY + MONTHWEEK + WEEKDAY + MONTHDAY + MONTH + YEAR + BEFVAC + AFTVAC + BEFCHR + AFTCHR


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
                       early_stopping_rounds=100, nrounds = 100000, num_parallel_tree=20, print_every_n = 20, nthread=8,eta = .1, max_depth = 7)

data_new_test <- build.x(data_formula_boxes, data=dataTest, contrasts = FALSE, sparse = TRUE)
dataTest$LINES_PRED <- predict(model.xgb,newdata = data_new_test)
dataTest$PRED_DIF <- abs(dataTest$LINES_PRED - dataTest$WHSLINES)


sqlquery <- paste("SELECT 
                    hour_hour as HOUR,
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
                    0 AS WHSLINES
                  FROM
                    printvis.workdayofweek
                  JOIN
                    printvis.forecasthours
                  WHERE
                    workday_date BETWEEN '",date_today,"' AND '",date_1week,"'
                    AND hour_hour BETWEEN 6 AND 16
                  ORDER BY workday_date, hour_hour", sep = "")
preddata <- query(sqlquery)
#need to build preddata as build.x
data_new <- build.x(data_formula_boxes, data=preddata, contrasts = FALSE, sparse = TRUE)
preddata$WHSLINES <- predict(model.xgb,newdata = data_new)


#sql pull to insert into printvis.loosevol_forecast

sqlquery <- paste("SELECT 
                      ",var_whse,", workday_date,'",var_equip,"', hour_hour AS HOUR
                  FROM
                      printvis.workdayofweek
                          JOIN
                      printvis.forecasthours
                  WHERE
                      workday_date BETWEEN  '",date_today,"' AND '",date_1week,"'
                      AND hour_hour BETWEEN 6 AND 16
                  ORDER BY workday_date , hour_hour", sep = "")
forecast_insert <- query(sqlquery)



forecast_insert$lines <- predict(model.xgb,newdata = data_new)
forecast_insert$cube <- 0
rmysql_update(mychannel, forecast_insert, 'printvis.loosevol_forecast', verbose = FALSE)


} #end of equipment list loop

} #end of whse list loop





currentDate <- Sys.Date()
csvFileName <- paste("line_pred_whse",var_whse,"_",currentDate,".csv",sep="")
write.csv(preddata, file=csvFileName)
lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)

