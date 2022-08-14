
knitr::opts_chunk$set(message = FALSE)
packages <-
  c('RMySQL')
purrr::walk(packages, library, character.only = TRUE)


source('connections.R')
source('RMySQL_Update.R')

query <- function(...)
  dbGetQuery(mychannel, ...)

trainstart_date <- '2019-10-01'
trainend_date <- '2019-10-31'

#Generate list of dates

sqlquery <- paste(
  "SELECT
  workday_date
  FROM
  gillingham.workdayofweek
  LEFT JOIN gillingham.fcast_dateexcl on exclude_date = workday_date
  WHERE
  workday_date BETWEEN '",
  trainstart_date,
  "' and '",
  trainend_date,
  "'
  and (workday_befvac + workday_aftvac + workday_befchrist + workday_aftchrist) = 0
  and exclude_date is null
  ORDER BY workday_date",
  sep = ""
)
data_dates <- query(sqlquery)

list_tier <- list('ALL', 'FLOW', 'BIN', 'PALL')

for (i in list_tier) {
  var_tier <- i
  
  for (i in 1:nrow(data_dates)) {
    var_date <- (data_dates[i,])

    
    
    sqlquery <- paste(
      "SELECT
      fcast_lines
      FROM
      gillingham.forecast_whselines_tier
      WHERE
      fcast_date = '",
      var_date,
      "'
      AND fcast_tier =  '",
      var_tier,
      "'",
      sep = ""
    )
    data_lines <- query(sqlquery)
    
    testdata <- t.test(data_lines)
    ci_mean_named <- testdata$estimate
    ci_mean <- as.numeric(ci_mean_named)
    
    ci_std <- sapply(data_lines, sd)
    #using 3 std because such a tight sd
    ci_meanplus2std <- ci_mean + (3 * ci_std)  
    ci_meanminus2std <- ci_mean - (3 * ci_std)

    
    
    
    ci_insert <-
      data.frame(var_date,
                 var_tier,
                 ci_mean,
                 ci_meanminus2std,
                 ci_meanplus2std)
    
    
    rmysql_update(mychannel,
                  ci_insert,
                  'gillingham.fcast_ci',
                  verbose = FALSE)
    
  }
}
lapply(dbListConnections(dbDriver(drv = "MySQL")), dbDisconnect)