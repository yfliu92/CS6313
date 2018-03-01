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
plot(college$Private, college$Outstate, main="Outstate vs Private")

# (iv)

Elite = rep("No",nrow(college))
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)

summary(Elite)
plot(Elite, college$Outstate, main="Outstate vs Elite")

# (v)
# Use hist() function to generate some histgrams

par(mfrow=c(2,3))
hist(college$Apps)
hist(college$Accept)
hist(college$Enroll)
hist(college$perc.alumni, col = 2)
hist(college$S.F.Ratio, col = 3, breaks = 10)
hist(college$Books)

# (vi)

par(mfrow=c(1,1))

plot(college$Apps, college$F.Undergrad)
# Colleges with high applications tend to have a large number of full-time undergraduates

plot(college$Accept, college$Enroll)
# Colleges which accept more students, the number of new studetns enroll tends to larger

plot(college$Outstate, college$Expend)
# Colleges with high out-of-state tuition tend to have a high instructional expenditure per student
