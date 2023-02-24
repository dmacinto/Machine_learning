#Question 1
library(dplyr)
library(caret)
library(glmnet)

#Question 2
STCdata_A<-read.csv('travelData.csv')
STCdata_A<-STCdata_A[,-1]

str(STCdata_A)

n_distinct(STCdata_A$From.Grade, na.rm = FALSE) ## n_distinct is a function from dplyr package

STCdata_A <- mutate_at(STCdata_A, vars(From.Grade), as.factor)

str( STCdata_A$From.Grade )


( unique.per.column <- sapply( dplyr::select_if(STCdata_A, is.numeric), n_distinct ) )

( column.names.to.factor <- names(unique.per.column)[unique.per.column < 15] )


STCdata_A <- mutate_at(STCdata_A, column.names.to.factor, as.factor)

date.columns = c('Departure.Date', 'Return.Date', 'Deposit.Date', 'Early.RPL', 'Latest.RPL',
                 'Initial.System.Date', 'FirstMeeting', 'LastMeeting')
STCdata_A <- mutate_at(STCdata_A, date.columns, function(x) as.Date(x, format = "%m/%d/%Y"))

STCdata_A <- mutate_if(STCdata_A, is.character, as.factor)


str(STCdata_A)


#Question 3

sapply(STCdata_A, function(x) sum(is.na(x)))
