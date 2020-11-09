# This code is based on the situation described at
# start of the file Bayesian Networks.pdf

# The code simulates 10000 different patients.
# At each iteration of the for loop it decides
# whether a patient has the virus or not and
# whether the patient has a positive test or not.
# 
# The code uses two vectors, pos and virus
# pos represents the outcomes of the blood test
# virus represents whether the patient has the virus or not

pos=0
virus=0
for(i in c(1:10000))
{
	if(runif(1) < 0.15)
	{
		virus[i]=1
		if(runif(1) < 0.95) pos[i] = 1 else pos[i] =0
	}
	else
	{
		virus[i]=0
		if(runif(1) < 0.02) pos[i] = 1 else pos[i] =0
	}
}

# the probability of getting a positive test is
sum(pos)/10000

# the probability of having the virus given a positive test is
sum(pos & virus)/sum(pos)

# the probability of having the virus given a negative test is
sum(!pos & virus)/sum(!pos)




