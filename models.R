# Load the systemfit library to create the SEM model
# Load the vars library to be able to use VAR() and the predict() function
# for VAR models. The zoo library is used for its time series functions
require("systemfit")
require("zoo")
require("vars")

# Some properties of the model have to be predefined.
# model.lag.max: the maximum lag between all steps.
# t.threshold: threshhold value for the t-statistics in the iterative
# exlusion of uncorrelated variables.
model.lag.max <- 30
model.lag.avg <- 7
t.threshold   <- 2

# Data is read from a CSV file. The data consists of daily aggregates of
# quantities per process step. In this example there are three process steps:
# SO: sales order; GS: goods shipped; IS: invoice sent
data.raw <- read.csv( file="Data/Sales-Quantities.csv", sep=";", 
                      header=TRUE, colClasses=c('Date', 'numeric', 'numeric', 'numeric') )

# Missing dates in the provided CSV files are filled by merging an empty
# data frame containing all dates with the provided data. Missing dates
# between the first and last day of the provided data is filled with zeros.
data.empty <- data.frame(
  Date=seq.Date(from=as.Date( head(sort( data.raw[,1] ), 1 ) ),
                to=as.Date( tail( sort(data.raw[,1]), 1) ), by="1 day") )
data.merged <- merge( data.empty, data.raw, by = c("Date"), all.x=TRUE,
                      all.y=FALSE )
data.merged[ is.na(data.merged) ] <- 0

# Select observations for the training and validation subsets
data.training <- data.merged[ 1:200, ]
data.validation <- data.merged[  201:nrow(data.merged), ]

# The SEM is modeled by using the systemfit function
model.sem.formulas <- as.list( NULL )
for ( col in seq( ncol( data.training ) ) ) {
  sem.formula = paste("data.training$",dimnames( data.training )[[2]][col], " ~ ")
  for ( col.minor in seq( ncol( data.training ) ) ) {
    if( col.minor != col ){
      sem.formula <- paste( sem.formula, "+ data.training$",
                            dimnames( data.training )[[2]][col.minor] )
    }
  }
  model.sem.formulas = c( model.sem.formulas, as.formula( sem.formula ) )
}
model.sem <- systemfit( model.sem.formulas, method = "OLS" )

model.lrm <- lm()


# A multipe time series object is created by using the ts function on the merged
# data frame. This mts object can be used by the vars package for modeling.
data.tseries <- ts( data = data.training[, 2:4] )

# The VAR is modeled by using the VAR function from the vars package based on
# the mts object. A maximum lag can be provided and since trend and constant terms
# should not be included in the model, type is set to none. In this case the model
# contains all of the variables restricted by lag.max (30) in this example.
model.var <- VAR( data.tseries, p=1, lag.max=model.lag.max, type="none" )

# The VAR model is restricted further to exclude all weakly correlated variables
# from the model.
model.var.restricted <- restrict( model.var, thresh=t.threshold, method = "ser" )

# Serveral built-in functions can be used to present the resulting models.
summary( model.sem )
summary( model.var )
summary( model.var.restricted )