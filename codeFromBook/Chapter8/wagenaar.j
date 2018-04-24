model { 
# Priors: all uniform 
p ~ dbeta(1,1) 
q ~ dbeta(1,1) 
c ~ dbeta(1,1) 

# Data: multinomial as a function of predicted probabilities  
consistent[1:4]   ~ dmulti(predprob[1,1:4], Nsubj[1]) 
inconsistent[1:4] ~ dmulti(predprob[2,1:4], Nsubj[2]) 
neutral[1:4]      ~ dmulti(predprob[3,1:4], Nsubj[3]) 

#Predictions for all three conditions 
#Row numbers refer to Table X.1
# Consistent condition 
predprob[1,1] <- (1 + p + q - p*q + 4 * p*c)/6 #Row 1
predprob[1,2] <- (1 + p + q - p*q - 2 * p*c)/3 #Row 2
predprob[1,3] <- (1 - p - q + p*q)/6           #Row 3 
predprob[1,4] <- (1 - p - q + p*q)/3           #Row 4
#  Inconsistent condition 
predprob[2,1] <- (1 + p - q + p*q + 4 * p*c)/6 #Row 5
predprob[2,2] <- (1 + p - q + p*q - 2 * p*c)/3 #Row 6
predprob[2,3] <- (1 - p + q - p*q)/6           #Row 7
predprob[2,4] <- (1 - p + q - p*q)/3           #Row 8
# Neutral condition
predprob[3,1] <- (1 + p + 4 * p*c)/6           #Row 9
predprob[3,2] <- (1 + p - 2 * p*c)/3           #Row 10
predprob[3,3] <- (1 - p)/6 				       #Row 11
predprob[3,4] <- (1 - p)/3                     #Row 12  
}     