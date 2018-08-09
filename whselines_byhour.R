knitr::opts_chunk$set(message = FALSE)
packages <- c('useful', 'coefplot', 'xgboost', 'here', 'magrittr', 'dygraphs', 'dplyr', 'RMySQL', 'caret', 'purrr', 'randomForest', 'rpart', 'neuralnet', 'tictoc', 'tinytex', 'DT', 'partykit', 'rpart.plot','rattle')
purrr::walk(packages, library, character.only = TRUE)
#lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
#dev
#mychannel <- dbConnect(MySQL(), user="bentley", pass="dave41", host="127.0.0.1")

#NY Server Prod
mychannel <- dbConnect(MySQL(), user="root", pass="dave41", host="127.0.0.1")

#Google Prod
#mychannel <- dbConnect(MySQL(), user="bentley", pass="dave41", host="104.154.153.225")

query <- function(...) dbGetQuery(mychannel, ...)

var_whse <- 2
var_build <- 1

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
WHERE
    loosevol_whse =  ",var_whse," and loosevol_availhour between 7 and 16
        AND loosevol_equip = 'FLOWRACK'
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

dataTest.xgb <- build.x(data_formula_boxes, data=dataTest, contrasts = FALSE, sparse = TRUE)

prediction.xgb <- predict(model.xgb, newdata = dataTest.xgb)
plot(dataTest$BOXES,prediction.xgb,col='blue',main='Real vs predicted Boosted Forest',pch=18, cex=0.7)
abline(0,1,lwd=2)