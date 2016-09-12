#################################################################################################
## OZKAN EMRE OZDEMIR                                                                           #
## HOMEWORK 5 : Facebook Edges Homework (Lecture 5)                                                           #
## 05/05/16                                                                                     #
## Class:  Methods for Data Analysis                                                            #
##                                                                                              #
#################################################################################################
## Clear objects from Memory :
rm(list=ls())
##Clear Console:
cat("\014")

# Get the libraries
library(igraph)

# Setup working directory
setwd('~/DataAnalysis/5_HypothesisTesting_ConfidenceIntervals')

# Get the data and check it
fb_data = read.csv('facebook_edge_list.csv', stringsAsFactors = FALSE)
head(fb_data)

fb_degree_list = as.numeric(table(fb_data$Source))

#########################################################################
# Part I : Compute the mean degree and shows a plot of the histogram of degrees.
# Verify this with Gephi.
#########################################################################

## Calculate the mean degree
## This will give us the information on avarage FB links to itself 
degree_mean = mean(fb_degree_list)
degree_mean # degre_mean is obtaines as 18.0262
            # which means that on average Facebook links to itself 18 times
            # This value is equivalent to the Gephi weighted degree value 18.026


## histogram of the degree list, we will compare this with GP
fb_hist = hist(fb_degree_list,breaks=50, freq=FALSE)

# Test Power law of fb_degree_list distribution
pow_fb_fit = power.law.fit(fb_degree_list)
pow_fb_fit  # KS.p = 0.9992188 is high enough to confirm that the test cannot reject that
            # the fb_degree_list data could have been drawn from the fitted power-law distribution.

#########################################################################
# Part II : Perform a K-S test for the following:
#- Test if the distribution of degrees is Poisson. (reuse K-S code)
#- Test if the distribution of degrees is a Power Law. (igraph)
#########################################################################

# Create k-s statistic function
ks_stat = function(x_min,x_max, dist_a, dist_b){
  x_seq = seq(x_min,x_max,len=1000)
  y_cdf1 = sapply(x_seq, function(x){
    sum(dist_a<x)/length(dist_a)
  })
  y_cdf2 = sapply(x_seq, function(x){
    sum(dist_b<x)/length(dist_b)
  })
  k_s_stat = max(abs(y_cdf1-y_cdf2))
  return(k_s_stat)
}

min_p = 1
max_p = 534

degree_poisson = dpois(fb_degree_list, degree_mean)

degree_ks = ks_stat(min_p, max_p, fb_degree_list, degree_poisson)


k_s_simulate = function(mean_lambda, length_vec){
  # Simulate two same poisson distributions with the same mean/length
  dist1 = rpois(length_vec, mean_lambda)
  dist2 = rpois(length_vec, mean_lambda)
  # Get k-s distance
  return(ks_stat(1, length_vec, dist1, dist2))
}

# Now that we can get 1 k-s stat under the null, let's get 1000 of them.
k_s_distribution = sapply(1:1000, function(x){
  k_s_simulate(degree_mean, 534)
})

hist(k_s_distribution,breaks=10, freq=FALSE)
# Now we just have to sum up how many are equal to or bigger than 1.
# Note we can never get larger than 1).  Since none are larger than 1,
# we say our p-value is at most 1 / 1,000 = 0.001.  So we reject the null.

summation = 0
for (i in 1:length(k_s_distribution)){
        if (k_s_distribution[i] >= 1){
        summation = summation + 1     
        }
        pvalue = summation/length(k_s_distribution)
}
pvalue # it is confirmed that there isn't any number of value equal or larger than one 

# Test Power law of k-s distribution
pow_ks_fit = power.law.fit(k_s_distribution)
pow_ks_fit  # KS.p = 0.008063914 is small
            # According to the test description (?power.law.fit) small p-values (less than 0.05) indicate that the test rejected the hypothesis 
            # that the original data could have been drawn from the fitted power-law distribution.
            # We can also observe the similar behavior from k_s_distribution histogram profile
            # Therefore, the k-s distribution cannot be accepted as a Power-Law

#################################       End     #################################