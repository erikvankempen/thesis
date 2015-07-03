# Determine number of detected anomalies prior to error injection
data.test <- data.validation
threshold <- 10
injection.size <- 5

# First predictions are calculated
model.sem.predictions <- predict( model.sem, interval = 'confidence',
	level = 0.01)
model.var.predictions <- predict( model.var,
	n.ahead = nrow(data.test), ci = 0.01 )
model.rvar.predictions <- predict( model.var.restricted,
	n.ahead = nrow(data.test), ci = 0.01 )
	
# Count number of detected anomalies prior to error injection
model.sem.pre.error.counter <- 0
model.var.pre.error.counter <- 0
model.rvar.pre.error.counter <- 0

for ( i in 1:nrow(data.test) ) {
	if( abs( data.test[i] -
		model.sem.predictions[i] ) > threshold ) {
			model.sem.pre.error.counter <- model.sem.pre.error.counter + 1
	}
	
	# VAR test
	if( abs( data.test[i] -
		model.var.predictions[i] ) > threshold ) {
			model.var.pre.error.counter <- model.var.pre.error.counter + 1
	}
	
	# RVAR test
	if( abs( data.test[i] -
		model.rvar.predictions[i] ) > threshold ) {
			model.rvar.pre.error.counter <- model.rvar.pre.error.counter + 1
	}
}

print( "Number of detected anomalies prior to injection:" )
print( "SEM: ", model.sem.pre.error.avg )
print( "VAR: ", model.var.pre.error.avg )
print( "RVAR: ", model.rvar.pre.error.avg )

# Determine number of detected anomalies after error injection
# Set counters
model.sem.error.counter <- 0
model.var.error.counter <- 0
model.rvar.error.counter <- 0

for ( i in 1:1000 ) {
	# Select 5 random samples
	sample.selection <- sample( seq( 1, nrow( data.test ) ), injection.size )
	
	# Set selected observations to 0
	data.test[sample.selection] <- 0
	
	# Count number of Type II errors (false negatives)
	for ( j in 1:injection.size ) {
		# SEM test
		if( abs( data.test[sample.selection[j]] -
			model.sem.predictions[sample.selection[j]] ) > threshold ) {
				model.sem.error.counter <- model.sem.error.counter + 1
		}
		
		# VAR test
		if( abs( data.test[sample.selection[j]] -
			model.var.predictions[sample.selection[j]] ) > threshold ) {
				model.var.error.counter <- model.var.error.counter + 1
		}
		
		# RVAR test
		if( abs( data.test[sample.selection[j]] -
			model.rvar.predictions[sample.selection[j]] ) > threshold ) {
				model.rvar.error.counter <- model.rvar.error.counter + 1
		}
	}
}

model.sem.error.avg <- model.sem.error.counter / 1000
model.var.error.avg <- model.var.error.counter / 1000
model.rvar.error.avg <- model.rvar.error.counter / 1000

print( "Averege number of Type II errors after injection:" )
print( "SEM: ", model.sem.error.avg )
print( "VAR: ", model.var.error.avg )
print( "RVAR: ", model.rvar.error.avg )
