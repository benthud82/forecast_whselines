

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
  WHERE
  workday_date BETWEEN '2019-05-01' AND '2019-05-31'
  ORDER BY workday_date",
  sep = ""
)
data_dates <- query(sqlquery)

for (i in 1:nrow(data_dates)) {
  var_date <- (data_dates[i, ])
  
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
  # #  ci_insert <- list()
  # ci_insert <- as.data.frame(
  #   #date = as.Date(character()),
  #   #tier = character(),
  #   #mean = integer(),
  #   #lower = integer(),
  #   #upper = integer()
  # )
  # ci_insert$date <- var_date
  # ci_insert$tier <- 'FLOW'
  # ci_insert$mean <- ci_mean
  # ci_insert$lower <- ci_lowbound
  # ci_insert$upper <- ci_upbound
  
  ci_insert <- data.frame(var_date, 'FLOW', ci_mean, ci_lowbound, ci_upbound)
  
  rmysql_update(mychannel,
                ci_insert,
                'gillingham.fcast_ci',
                verbose = FALSE)
  
}
lapply(dbListConnections(dbDriver(drv = "MySQL")), dbDisconnect)