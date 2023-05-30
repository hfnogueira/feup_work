# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
#                       carenR algorithm application
#                       by:  hugonogueira
#                       at:  Fri May 12 13:47:05 2023  
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  


# libs --------------------------------------------------------------------------------

library(carenR)
library(tidyverse)


# read dataset ------------------------------------------------------------------------

df.rdd <- read.csv(file = 'data/dis_rule_dataset_rdd.csv') %>%  select (-Year)

glimpse(df.rdd)

head(df.rdd)

# test --------------------------------------------------------------------------------

# Build distribution rule set
drs<-caren(df.rdd, Dist=TRUE, POI='Wine_mhl')

drs

# View rules
plot.drs(drs)

# View next rules
plot.drs(drs,iterate=TRUE)











# examples ----------------------------------------------------------------------------

# Attribute-Value (UCI like) dataset
data(mushrooms)

head(mushrooms)

# build rule set, should take less than 1 minute
rls<-caren(mushrooms,class='class',min.sup=0.1,imp=0.001)

# view first 10 rules
ar.pp(rls[1:10,])

# view 10 rules randomly chosen
ar.pp(rls[sample(1:nrow(rls),10),])


# view 10 rules with highest support
ar.pp(rls[order(rls$Sup,decreasing=TRUE)[1:10],])

# Two Column Datasets 
# Load dataset with basket (two column) format
data(greengrocers)

head(greengrocers)

# Build rule set
rls<-caren(greengrocers, Bas=TRUE)

# view first rule
rls[1,]

# Distribution Rules
# Load dataset with one numerical Property of Interest (MPG)
data(auto.mpg.discr)

head(auto.mpg.discr)

# Build distribution rule set
drs<-caren(auto.mpg.discr, Dist=TRUE, POI='MPG')

# View rules
plot.drs(drs)

# View next rules
plot.drs(drs,iterate=TRUE)

mushrooms$class = as.numeric(mushrooms$class)

drs<-caren(mushrooms, Dist=TRUE, POI='class')
plot.drs(drs)
plot.drs(drs,iterate=TRUE)

for (i in 1:ncol(auto.mpg.discr)) {
  
  print(class(auto.mpg.discr[,i]))
}


# Example data frame
df <- data.frame(x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

# Discretize x into intervals of unequal width
df$interval <- cut(df$x, breaks = c(0, 3, 7, Inf))

# View the result
df



# Example data frame
df <- data.frame(x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

# Discretize x into 3 intervals of equal width
df$interval <- cut(df$x, breaks = 3)

# View the result
df

