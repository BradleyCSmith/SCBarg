################################################################################
###
###
###   Data management
###   Author: Brad Smith
###
###
###   Purpose: Cleans and formats data for Supreme Court project
###
###   Outputs: Creates four objects, listed below
###            k: a data frame containing information on SS ideal points
###            d: a matrix containing ideal points for each case
###            x: column vector containing case outcomes
###            id: matrix containing corresponding id values for elements in d
###            
###
################################################################################

rm(list = ls())

#First we'll do SS cases
d <- read.csv("Data/AllIdealPoints.csv")
s <- d[d$Type == "SS",]
s <- s[complete.cases(s),]

#Load justice information
k <- read.csv("Data/SSJusticetable.csv")

#Eliminate court 2, as we can't uniquely identify Jackson and Minton (same IP)
s <- s[s$Court!=2,]

# Create an object where each row is the ideal points of the justices for each
# case, sorted in ascending order
d <- matrix(NA, nrow(s), 9)
for(i in 1:nrow(s)){
  d[i,] <- as.numeric(sort(s[i,c("J1", "J2", "J3", "J4", 
                      "J5", "J6", "J7", "J8", "J9")]))
}

# Pull out outcomes
x <- as.vector(s$Opinion)

#Create an object where we assign each justice a unique ID and associate those 
#with entries in d
length(unique(c(d))) # Check to see how many unique values
k <- k[k$Median.Posterior != 0.17,]
length(unique(j$Median.Posterior))  # Once we've removed duplicate justices, 
                                    # check num of unique (they're both 29)
k$id <- c(1:nrow(k))

#create the desired object
id <- matrix(NA, nrow(d), ncol(d))
for(i in 1:nrow(id)){
  for(j in 1:ncol(id)){
    id[i,j] <- k$id[which(k$Median.Posterior == d[i,j])]
  }
}

# Clear the workspace and save output 
rm(list = setdiff(ls(), c("d", "id", "k", "x")))
save.image("~/Google Drive/Research/Supreme Court Bargaining/Data/stan_ss_data.RData")
