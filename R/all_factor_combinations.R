library(data.table)

all_combinations <- function(
	factor.names=c("V1", "V2"), 
	factor.values=list(
		V1=c("A", "B"),
		V2=c(1, 2)
	),
	wide.form=FALSE
) {
	if (length(factor.names) != length(factor.values)) {
		print("names and values lists must be of same length!")
		return(FALSE)
	}
	
	lengths <- sapply(factor.values, function(v) length(v))
	indexes <- sapply(factor.values, function(v) 1)
	
	ret=data.table()
	cycle.length <- prod(lengths)
	cur.combin <- 0
	
	while (cur.combin < cycle.length) {
		cur.values <- mapply(function(Vs, i) Vs[i], factor.values, indexes)
		cur.combin <- cur.combin + 1
		ret <- rbind(ret, data.table(
			cur.combin=cur.combin, 
			factor.name=factor.names, 
			factor.value=cur.values 
		))
		
		# Next value(s)
		flag <- TRUE
		for (i in 1:length(indexes)) {
			if (flag == TRUE) {
				indexes[i] <- indexes[i] + 1
				flag <- (indexes[i] > lengths[i])
				if (flag == TRUE) {indexes[i] <- 1}
			}				 
		}
	}
	
	if (wide.form==TRUE) {
		return(
			dcast(ret, cur.combin ~ factor.name, value.var=c("factor.value")) # Wide form
		)
	}
	
	return(ret) # Long form
	
}

all_combinations()

all_combinations(wide.form=TRUE)
