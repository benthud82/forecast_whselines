#detect outliers at the whse, date, hour, equipment level

#knitr::opts_chunk$set(message = FALSE)
#packages <- c('useful', 'coefplot', 'xgboost', 'here', 'magrittr', 'dygraphs', 'dplyr', 'RMySQL', 'caret', 'purrr', 'randomForest', 'rpart', 'neuralnet', 'tictoc', 'tinytex', 'DT', 'partykit', 'rpart.plot','rattle')
#purrr::walk(packages, library, character.only = TRUE)
#lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
#dev
mychannel <- dbConnect(MySQL(), user="bentley", pass="dave41", host="127.0.0.1")

#NY Server Prod
#mychannel <- dbConnect(MySQL(), user="root", pass="dave41", host="127.0.0.1")

#Google Prod
#mychannel <- dbConnect(MySQL(), user="bentley", pass="dave41", host="104.154.153.225")

query <- function(...) dbGetQuery(mychannel, ...)

var_whse <- 6
var_build <- 1

sqlquery <- paste("SELECT 
                      CASE
                        WHEN loosevol_availhour < 6 THEN 6
                        WHEN loosevol_availhour > 16 THEN 16
                      ELSE loosevol_availhour
                      END AS HOUR,
                      SUM(loosevol_lines) AS WHSLINES
                  FROM
                      printvis.hist_loosevol_summary
                  JOIN
                      printvis.workdayofweek ON loosevol_availdate = workday_date
                  WHERE
                      loosevol_whse =  ",var_whse," and loosevol_availhour between 6 and 16
                      AND loosevol_equip = 'BLUEBIN' and loosevol_availdate <= '2019-01-17'
                      AND CASE
                            WHEN loosevol_availhour < 6 THEN 6
                            WHEN loosevol_availhour > 16 THEN 16
                            ELSE loosevol_availhour
                          END = 10
                  GROUP BY loosevol_availdate, 
                      CASE
                        WHEN loosevol_availhour < 6 THEN 6
                        WHEN loosevol_availhour > 16 THEN 16
                      ELSE loosevol_availhour
                      END
                  ORDER BY loosevol_availdate , HOUR", sep = "")
data <- query(sqlquery)

outlier_values <- boxplot.stats(data$WHSLINES)$out
