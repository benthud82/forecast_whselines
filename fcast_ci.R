


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
    AND fcast_tier = 'FLOW'",
    sep = ""
  )
  data_lines <- query(sqlquery)
  
  testdata <- t.test(data_lines)
  ci_mean_named <- testdata$estimate
  ci_mean <- as.numeric(ci_mean_named)
  ci_lowbound <- testdata$conf.int[1]
  ci_upbound <- testdata$conf.int[2]


  
  ci_insert <-
    data.frame(var_date, 'FLOW', ci_mean, ci_lowbound, ci_upbound)
  
  rmysql_update(mychannel,
                ci_insert,
                'gillingham.fcast_ci',
                verbose = FALSE)
  
}
lapply(dbListConnections(dbDriver(drv = "MySQL")), dbDisconnect)