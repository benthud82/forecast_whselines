




knitr::opts_chunk$set(message = FALSE)
packages <-
  c(
    'useful',
    'coefplot',
    'xgboost',
    'here',
    'magrittr',
    'dygraphs',
    'dplyr',
    'RMySQL',
    'caret',
    'purrr',
    'randomForest',
    'rpart',
    'neuralnet',
    'tictoc',
    'tinytex',
    'DT',
    'partykit',
    'rpart.plot',
    'rattle'
  )
purrr::walk(packages, library, character.only = TRUE)
#lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
source('connections.R')

source('RMySQL_Update.R')
var_whse <- 'GB0001'
query <- function(...)
  dbGetQuery(mychannel, ...)
date_today <- Sys.Date()
date_today <- '2019-03-31'

trainstart_date <- '2017-01-01'
trainend_date <- '2019-07-31'

predstart_date <- '2019-08-01'
predend_date <- '2019-08-31'

list_tier <- list('%','FLOW', 'BIN', 'PALL')
#list_tier <- list('PALL')

for (i in list_tier) {
  var_tier <- i
  
  
  
  sqlquery <- paste(
    "SELECT
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
    sum(linesgrouped_lines) AS WHSLINES
    FROM
    gillingham.workdayofweek
    JOIN
    gillingham.fcast_linesgrouped ON linesgrouped_date = workday_date
    LEFT JOIN gillingham.fcast_dateexcl on exclude_date = workday_date
    WHERE
    workday_date  between '",
    trainstart_date,
    "' and '",
    trainend_date,
    "'
    and (workday_befvac + workday_aftvac + workday_befchrist + workday_aftchrist) = 0
    and exclude_date is null
    and linesgrouped_tier LIKE '",
    var_tier,
    "'
    GROUP BY workday_date",
    sep = ""
  )
  data <- query(sqlquery)
  for (s in 1:12) {
    set.seed(s)
    trainIndex <- createDataPartition(data$WHSLINES,
                                      p = .75,
                                      list = FALSE,
                                      times = 1)
    
    dataTrain <- data[trainIndex,]
    dataTest  <- data[-trainIndex,]
    
    data_formula_boxes <-
      WHSLINES ~ WORKDAY + MONTHWEEK + WEEKDAY + MONTHDAY + MONTH + YEAR + BEFVAC + AFTVAC + BEFCHR + AFTCHR
    
    
    #boxes data training
    dataX_Train_box <- build.x(
      data_formula_boxes,
      data = dataTrain,
      contrasts = FALSE,
      sparse = TRUE
    )
    dataY_Train_box <- build.y(data_formula_boxes, data = dataTrain)
    
    dataX_Test_box <- build.x(
      data_formula_boxes,
      data = dataTest,
      contrasts = FALSE,
      sparse = TRUE
    )
    dataY_Test_box <- build.y(data_formula_boxes, data = dataTest)
    
    xgTrain_box <- xgb.DMatrix(data = dataX_Train_box,
                               label = dataY_Train_box)
    
    xgVal_box <- xgb.DMatrix(data = dataX_Test_box,
                             label = dataY_Test_box)
    
    model.xgb <-
      xgb.train(
        data = xgTrain_box,
        objective = 'reg:linear',
        eval_metric = 'rmse',
        booster = 'gbtree',
        watchlist = list(train = xgTrain_box, validate = xgVal_box),
        early_stopping_rounds = 100,
        nrounds = 1000,
        num_parallel_tree = 20,
        print_every_n = 20,
        nthread = 16,
        eta = .1,
        max_depth = 8
      )
    
    dataTest.xgb <-
      build.x(
        data_formula_boxes,
        data = dataTest,
        contrasts = FALSE,
        sparse = TRUE
      )
    
    prediction.xgb <- predict(model.xgb, newdata = dataTest.xgb)
    
    
    
    dataTest$forecastlines <-
      predict(model.xgb, newdata = dataTest.xgb)
    dataTest$delta <- dataTest$forecastlines - dataTest$WHSLINES
    dataTest$error_rate <-
      (dataTest$forecastlines - dataTest$WHSLINES) / dataTest$WHSLINES
    dataTest$error_rate_abs <-
      abs((dataTest$forecastlines - dataTest$WHSLINES) / dataTest$WHSLINES)
    currentDate <- Sys.Date()
    csvFileName <-
      paste("forecast_whselines_0",
            var_whse,
            "_",
            currentDate,
            ".csv",
            sep = "")
    write.csv(dataTest, file = csvFileName)
    
    
    sqlquery <- paste(
      "SELECT
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
      LEFT JOIN gillingham.fcast_dateexcl on exclude_date = workday_date
      WHERE
      workday_date between '",
      predstart_date,
      "' and '",
      predend_date,
      "'
      and (workday_befvac + workday_aftvac + workday_befchrist + workday_aftchrist) = 0
      and exclude_date is null
      ORDER BY workday_date",
      sep = ""
    )
    preddata <- query(sqlquery)
    
    data_new_lines <-
      build.x(
        data_formula_boxes,
        data = preddata,
        contrasts = FALSE,
        sparse = TRUE
      )
    
    if (var_tier == '%') {
      tierinsert = 'ALL'
    } else{
      tierinsert =var_tier
    }
    
    
    sqlquery <- paste(
      "SELECT
      0,workday_date, ",
      s,
      ", '",
      tierinsert,
      "'
      FROM
      gillingham.workdayofweek
      LEFT JOIN gillingham.fcast_dateexcl on exclude_date = workday_date
      WHERE
      workday_date between '",
      predstart_date,
      "' and '",
      predend_date,
      "'
      and (workday_befvac + workday_aftvac + workday_befchrist + workday_aftchrist) = 0
      and exclude_date is null
      ORDER BY workday_date",
      sep = ""
    )
    forecast_insert <- query(sqlquery)
    
    
    
    forecast_insert$lines <-
      predict(model.xgb, newdata = data_new_lines)
    forecast_insert$seed02 <- 0
    forecast_insert$seed03 <- 0
    rmysql_update(mychannel,
                  forecast_insert,
                  'gillingham.forecast_whselines_tier',
                  verbose = FALSE)
  }
}
lapply(dbListConnections(dbDriver(drv = "MySQL")), dbDisconnect)
