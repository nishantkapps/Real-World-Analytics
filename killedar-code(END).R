###############################################################################
# You can use this template to draft the script for your Assessment 2 of SIT718.
###############################################################################
# save your code as "name-code.R" (where ''name'' is replaced 
# with your surname or first name).
###############################################################################
install.packages("lpSolve")
install.packages("lpSolveAPI")
library(lpSolve)
library(lpSolveAPI)
###############################################################################
#------------------------------------------------------------------------------
# Question 1 - Food factory beverage mixing
#------------------------------------------------------------------------------
###############################################################################
# Set working directory
setwd('D:/MDS-Deakin/SIG718 - Real World Analytics/Mid-Term Assignment')

beverage = make.lp(0,2)

lp.control(beverage, sense="minimize")

set.objfn(beverage, c(4,12))

add.constraint(beverage, c(1,1), ">=", 100)
add.constraint(beverage, c(2,7), "<=", 600)
add.constraint(beverage, c(6,4), ">=", 500)
add.constraint(beverage, c(4,8), ">=", 500)

solve(beverage)

get.objective(beverage)
get.variables(beverage)

