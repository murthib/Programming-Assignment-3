helpfn <- function(data, outcome, num){
ListOfHospitals <- data[, 2][order(outcome, data[, 2])[num]]
ListOfHospitals
}
rankall <- function(outcome, num = "best") {

#-----------------------------------------------------
# Logic
#1.  check inputs, throw error if inputs are not proper
#2.  open outcome-of-care-measures
#3. Loop through all records and build 
#4. sort
#5. return hospital name
#-----------------------------------------------------

	setwd("C:/Users/murthib/Downloads/3 Big Data/Coursera/Course 2 R Programming/Week 4/rprog-data-ProgAssignment3-data")
	data <- read.csv("outcome-of-care-measures.csv", header = TRUE,stringsAsFactors=FALSE, colClasses = "character")

	reason <- c("heart attack", "heart failure", "pneumonia")
	StateNames_arr <- sort(unique(data$State))
	arr_len <- length(StateNames_arr)
	ListOfHospitals <- rep("", arr_len)
	if(!outcome %in% reason){
		stop("invalid outcome")
	} 
	else 
	{
		for(i in 1:arr_len)
		{
			target <- data[data$State == StateNames_arr[i], ]
			if(outcome == "heart attack")
			{
				attack <- as.numeric(target[, 11])   
				len <- dim(target[!is.na(attack),])[1]
				if(num == "best")
				{
					ListOfHospitals[i] <- helpfn(target, attack, 1)
				} 
				else if(num == "worst")
				{
					ListOfHospitals[i] <- helpfn(target, attack, len)
				} 	
				else if(num > len)
				{
					ListOfHospitals[i] <- NA
				} 
				else
				{
					ListOfHospitals[i] <- helpfn(target, attack, num)
				}          
			}
			else if(outcome == "heart failure" )
			{
			# Attention here!
				failure <- as.numeric(target[, 17])   
				len <- dim(target[!is.na(failure),])[1]
				if(num == "best")
				{
					ListOfHospitals[i] <- helpfn(target, failure, 1)
				} else if(num == "worst")
				{
					ListOfHospitals[i] <- helpfn(target, failure, len)
				} else if(num > len)
				{
					ListOfHospitals[i] <- NA
				} else
				{
					ListOfHospitals[i] <- helpfn(target, failure, num)
				} 
			}
		else
		{
			pneumonia <- as.numeric(target[, 23])
			len <- dim(target[!is.na(pneumonia),])[1]
			if(num == "best")
			{
				ListOfHospitals[i] <- helpfn(target, pneumonia, 1)
			} else if(num == "worst")
			{
				ListOfHospitals[i] <- helpfn(target, pneumonia, len)
			} else if(num > len)
			{
				ListOfHospitals[i] <- NA
			} else
			{
				ListOfHospitals[i] <- helpfn(target, pneumonia, num)   
			} 
		}  
	}
	df <- data.frame(ListOfHospitals = ListOfHospitals, state = StateNames_arr)
	df
	}
}