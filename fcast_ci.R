


knitr::opts_chunk$set(message = FALSE)
packages <-
  c('RMySQL')
purrr::walk(packages, library, character.only = TRUE)


source('connections.R')
source('RMySQL_Update.R')

query <- function(...)
  dbGetQuery(mychannel, ...)


#Generate list of dates

sqlquery <- paste(
  "SELECT
  workday_date
  FROM
  gillingham.workdayofweek
  LEFT JOIN gillingham.fcast_dateexcl on exclude_date = workday_date
  WHERE
  workday_date BETWEEN '2019-05-01' AND '2019-05-31'
  and (workday_befvac + workday_aftvac + workday_befchrist + workday_aftchrist) = 0
  and exclude_date is null
  ORDER BY workday_date",
  sep = ""
)
data_dates <- query(sqlquery)

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
    AND fcast_tier = '%'",
    sep = ""
  )
  data_lines <- query(sqlquery)
  
  testdata <- t.test(data_lines)
  ci_mean_named <- testdata$estimate
  ci_mean <- as.numeric(ci_mean_named)
  
  ci_std <- sapply(data_lines, sd)
  ci_meanplus2std <- ci_mean + (2 * ci_std)
  ci_meanminus2std <- ci_mean - (2 * ci_std)
  
  
  
  ci_insert <-
    data.frame(var_date, 'ALL', ci_mean, ci_meanminus2std, ci_meanplus2std)
  
  
  rmysql_update(mychannel,
                ci_insert,
                'gillingham.fcast_ci',
                verbose = FALSE)
  
}
lapply(dbListConnections(dbDriver(drv = "MySQL")), dbDisconnect)