



source('connections.R')
source('RMySQL_Update.R')

query <- function(...)
  dbGetQuery(mychannel, ...)

sqlquery <- paste(
  "SELECT
    fcast_lines
  FROM
    gillingham.forecast_whselines
  WHERE
    fcast_date = '2019-01-03'
    AND fcast_tier = 'BIN'",
  sep = ""
)
data_lines <- query(sqlquery)

testdata<-t.test(data_lines)
ci_mean_named <- testdata$estimate
ci_mean <- as.numeric(ci_mean_named)
ci_lowbound <- testdata$conf.int[1]
ci_upbound <- testdata$conf.int[2]

ci_insert <-

rmysql_update(mychannel,
              forecast_insert,
              'gillingham.forecast_whselines',
              verbose = FALSE)