###################################################################################################################
# Name          : ManjeetPanda_Aug_IATST_Lab.R                                                                    #
# Date Modified : 03 September 2017                                                                               #
# Author        : Manjeet Panda                                                                                   #
###################################################################################################################

#Set working Directory
setwd("C:/Users/sg0949286/Documents/R/WorkSpace/LabAssignment")

###################################################################################################################
# Question NUmber 1
# Let X1, X2, X3, X4, X5 be independent U (0, 1) random variables. Let X = X1 + X2 + X3 and Y = X3 + X4 + X5. 
# Use the runif() function to simulate 1000 trials of each of these variables. Use these to estimate Cov (X, Y).
###################################################################################################################

# Step 1: Delcare Sample Size
N = 1000

# Step 2: Use runif to generate independent random variables
X1 = runif(N)
X2 = runif(N)
X3 = runif(N)
X4 = runif(N)
X5 = runif(N)

# Step 3: Declare and define X and Y
X = X1 + X2 + X3
Y = X3 + X4 + X5

# Step 4: Calculate covariance of X and Y
cov(X,Y)

###################################################################################################################
# Question NUmber 2
# The random variable X takes values -1, 0, 1 with probabilities 1/8, 2/8, 5/8 respectively.
# (a) Compute E(X).
# (b) Give the pmf of Y = X^2 and use it to compute E(Y).
# (c) Instead, compute E(X^2) directly from an extended table.
# (d) Compute Var(X).
###################################################################################################################

# Create a List of Random Variables with values -1,0,1 with the given probabilities
x = c(-1,0,1)
prob_of_x = c(0.125, 0.25, 0.625)

# Part a: Compute E(X) = mean
e_of_x = sum(x*prob_of_x)

# Part b: Compute PMF Y = X^2, and then compute E(Y)
y = unique(x^2)  #Get a vector y which has unique values of x squared
prob_of_y = c(rep(0,length(y))) #Initialize the probability vector for y
#Run the following nested loop to get the probability of each y value
for (i in 1:length(x))
  for (j in 1:length(y))
{
  if (x[i]^2 == y[j])
  {
    prob_of_y[j] = prob_of_y[j] + prob_of_x[i]
  }
}
#Now calculate E(y)
e_of_y = sum(y*prob_of_y)

# Part c: Calcaulate E(X^2) directly from an extended table,
#         which essentially means take the sum of all elements of X multiplied with the corresponding probability
e_of_sq_of_x = sum((x^2)*prob_of_x)

# Part d: Calculate the variance of x
sq_of_e_of_x = e_of_x^2
var_of_x = e_of_sq_of_x - sq_of_e_of_x


#Print all results
cat(" X      : " , x, "\n",
    "Y = X^2: " , y, "\n",
    "Prob(Y): ", prob_of_y, "\n",
    "E(Y)   : ", e_of_y, "\n",
    "Prob(X):", prob_of_x, "\n",
    "E(X^2) : ", e_of_sq_of_x, "\n",
    "E(X)^2 : ", sq_of_e_of_x, "\n",
    "var(x) : ", var_of_x)
