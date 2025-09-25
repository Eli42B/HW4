#ZOO 800
#Homework Week 4
#Submission instructions
#Submit a single URL to a public GitHub repository on Canvas. Solutions to
#Objectives 1-3 should be included in a single R script.

#Problem
#Antarctic researchers think that smaller penguins are more susceptible to climate change, but they need some help summarizing their data.The penguin data set they have sent you can be found in the package: "palmerpenguins" and the data frame object is called “penguins”. Install the package and read in the dataset.

# Loading packages
install.packages("palmerpenguins") #if you try to load 'penguins' you will get an error, it now goes by palmerpenguins 

#Loading libraries 
library(palmerpenguins)
library(dplyr)
library(tibble)

#############################################
# OBJECTIVE 1 
#############################################

#Create a function to convert a continuous variable into a binary variable (e.g., high/low, yes/no, 1/0, etc.). The function should allow the user to specify the breakpoint by which the data are divided in two, as well as specify the labels that both groups are assigned

d = penguins #call the data 
#d = na.omit(d) # remove hte NAs, they will mess us up 
hist(d$body_mass_g) #what's a reasonable breakpoint? Let's check via a histogram
d = as_tibble(d) #turning it into a tibble, because tibbles are better 

break_value = 4500 #based on our histogram, let's set our break_value to 4500 

#We will need to learn subsetting
#https://adv-r.hadley.nz/subsetting.html


#Create a function that takes in two values: the data and the break value
binary = function(data, break_value) { 
  for (mass in data$body_mass_g) {
    if(is.na(mass)) {
       next #skips stuff 
    } else if (mass >= break_value) {
      mass = 'large'
    } else {
      mass = 'small'
    }
  }
}


#Create a function that takes in two values: the data and the break value

body_mass_g = d$body_mass_g

binary = function(data, break_value) { 
  for (mass in body_mass_g) {
    if(is.na(mass)) {
      next #skips stuff 
    } else if (mass >= break_value) {
      mass = 'large'
    } else {
      mass = 'small'
    }
  }
}


#example 

t = 0 
return_value <- function(t) {
  return(1000/((((1000-1)/1)*(2.71828^(-0.1*1*t)))+1))
} #create a function 

for (date in dates) {
  values <- c(values, return_value(t))
  t <- t + 1
} #loop the function with dates 

#Save in a dataframe 
d = data.frame(dates, values)
print(d)




for (date in dates) {
  values <- c(values, return_value(t))
  t <- t + 1
} #loop the function with dates 


result = list()
for (i in seq_along(dog_names)) {
  result[[i]] = paste("Good dog:", dog_names[i])
  print(result)
}


binary = function(data, break_value) {
  for (i in seq_along(data$body_mass_g)) {
    if(is.na(data$body_mass_g[[i]])) {
      next 
    } else if (data$body_mass_g[[i]] > break_value) {
      out[[i]] = "fat"
    } else {
    out[[i]] = "small"
    }
  }
} 
binary(d, 4500)
out

binary = function(data, break_value) { 
  for (mass in data$body_mass_g) {
    if(is.na(mass)) {
      next #skips stuff 
    } else if (mass >= break_value) {
      mass = 'large'
    } else {
      mass = 'small'
    }
  }
}


binary(d, break_value)
#OK this works but it doesn't share what the mass is outside of the function 

#d$body_mass_g[1]
weight_vector = vector()
binary = function(data, break_value) {
  for (i in 1:length(data)) {
    weight = data$body_mass_g[i]
    i + 1 
    weight_vector = c(weight_vector, weight)
  }
}

binary(d, 4500)
weight_vector

multi.size.at.age = function(linf, k, t0, t){
  
  lt = linf[1] * (1 - exp(-k[1] * (t[[1]] - t0[1]))) # calculate lengths for first fish
  
  plot(t[[1]], lt, type = "l",
       xlim = c(0, max(unlist(t))), 
       ylim = c(0, max(linf)))                       # plot growth for first fish
  
  for(i in 2:length(linf)){                          # calculate lengths for all subsequent fish and add to figure
    
    lt = linf[i] * (1 - exp(-k[i] * (t[[i]] - t0[i])))
    
    lines(t[[i]], lt)
    
  }
  
}