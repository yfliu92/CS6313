# (a) 
# Read College.csv to R, 
# Make sure the project2.R is in the same directory with College.csv

college = read.csv("./College.csv")

# (b) 
# Create a row.column with the names of each university recorded

rownames(college) = college[, 1]
fix(college)

# Eleminate the first column in the data where names are stored

college = college[, -1]
fix(college)

# (c)
# (i) Produce a numerical summary of the variables in the college data set

summary(college)

# (ii)

pairs(college[,1:10], main="College[, 1:10]")

# (iii)
# Outstate vs Private

plot(college$Private, college$Outstate)

