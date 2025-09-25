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

# A Create a function to convert a continuous variable into a binary variable(e.g., high/low, yes/no, 1/0, etc.). The function should allow the user to specify the breakpoint by which the data are divided in two, as well as specify the labels that both groups are assigned

binary = function(x) {
  ifelse(x<4000,"small","big")
}
binary(4)
binary(6000)

#Part B: Use your function to convert body mass into a binary variable of ‘small’and ‘large’ penguins

d = penguins #call the data 
#d = na.omit(d) # remove hte NAs, they will mess us up 
hist(d$body_mass_g) #what's a reasonable breakpoint? Let's check via a histogram
d = as_tibble(d) #turning it into a tibble, because tibbles are better 

binary(d$body_mass_g) #yep it works 
binary1 = binary(d$body_mass_g) #saving it as a vector
d$class_size = binary1 #bind it to the dataframe 

#######################################
# OBJECTIVE 2 
#######################################

# Nathan did tihs!!! THis is not our work he helped us thank you Nathan! 

continuous_to_categorical = function(values, breakpoints, category_labls) {
  #this assumes the breakpoints and label cateogiry vectaors are ordered, and that breakpoints is 1 element smaller than label_cateogry
  converted_values = character(length(values)) #this is our storage vector
  #go through each value 
  #seq in R makes hte range, range gives you the min and max 
  for (value_number in seq(length(values))) {
    if (is.na(values[value_number] <- NA 
    next
  }
  for (breakpoint_number in seq(length(breakpoints))) {
    if values[value_number] = breakpoints[breakpoint_number]) {
      converted_values[value_number] = category_labels[breakpoint_number]
      #stop the  loop
      break
    }
  }

continuous_to_categorical <- function(values, breakpoints, category_labels) {
  # This assumes the breakpoints and label_categories vectors are ordered,
  # and that breakpoints is 1 element smaller than label_categories.
  # This is our storage vector
  converted_values <- character(length(values))
  # Go through each value (it would be nice if this was vectorizable)
  # Seq in R makes the range, range gives you the min and max
  for (value_number in seq(length(values))) {
    # NA will crash the function unless I do this
    if (is.na(values[value_number])) {
      converted_values[value_number] <- NA
      # Continues with the next thing in the loop
      next
    }
    # Otherwise, evaluate if it's less than or equal to each breakpoint at a time
    for (breakpoint_number in seq(length(breakpoints))) {
      # If value is less than breakpoint we assign label and stop looping
      if (values[value_number] <= breakpoints[breakpoint_number]) {
        # Assign the label, which is the corresponding index in label_categories EXCEPT for the last one
        converted_values[value_number] <- category_labels[breakpoint_number]
        # Stop the loop
        break
      }
    }
    # If the label wasn't assigned yet after we hit the last breakpoint it's the last label by default
    if (converted_values[value_number] == "") {
      # NEGATIVE INDEXING DROPS THE ELEMENT IN R IT DOES NOT ACCESS THE LAST
      converted_values[value_number] <- tail(category_labels, n=1)
    }
  }
  return(converted_values)
}

# Test omg it works!!!!
continuous_to_categorical(c(1:10), breakpoints=c(2, 4, 6, 8), category_labels=c("below 2", "below 4", "below 6", "below 8", "above 8"))
# Second test with NA things
continuous_to_categorical(c(2, NA, 3), breakpoints=c(2.5), category_labels=c("small", "large"))

# Question 2b - small, medium, and large penguins
# Look at a range again
range(penguins$body_mass_g, na.rm=TRUE)
# I'll make the cutoffs 3900 and 5100
penguins$body_mass_sml <- continuous_to_categorical(penguins$body_mass_g, c(3900, 5100), c("small", "medium", "large"))   

  
  
  
#Example 3: multiple if then statements

binary = function(x) {
  if (x < 2000) {
    print("small")
  } else if (x >= 2000 & x < 5000) {
    print("medium")
  } else print("large")
}

binary(100)
binary(2500)
binary(6000)

#The problem is it cannot handle a list or a vector 

binary = function(data) {
  for (i in data) {
    if(is.na(data$body_mass_g[i])) {
      print("NA")
    } else print("not NA")
  }
}


binary(d)  

storage = vector()

binary = function(data) {
  for (i in 1:length(data$body_mass_g)) {
    if(is.na(data$body_mass_g[i])) {
      storage[i] = NA
      storage = c(storage, storage[i])
    } else if (data$body_mass_g[i] < 2000) {
      storage[i] = "small"
      storage = c(storage, storage[i])
    } else if (data$body_mass_g[i] >=2000 & data$body_mass_g[i] < 5000) {
      storage[i] = "medium"
      storage = c(storage, storage[i])
    } else storage[i] = "large"
    storage = c(storage, storage[i])
  }
}

binary(d)

#hold for now 
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


d$body_mass_g[1]

x = c(100, 4500, 9000)
binary(x)

#this is if you expand it from ifelse to if and hten a separate else statement 
binary = function(x) { 
  if (x > 0) {
    print("small")
  } else {
    print("big")
  }
}


#We will need to learn subsetting
#https://adv-r.hadley.nz/subsetting.html

#4.5 hrs later screw it we're using dplyr :(  
d2 = d %>% 
  mutate(mass = if_else(body_mass_g < 4000, "small","big"))


#Create a function that takes in two values: the data and the break value

split_size = function(d) { 
  for(mass in d$body_mass_g){
    size = ifelse(mass > 4000, "big","small")
    print(size)}
}

#this prints but it won't turn it into a vector 

binary = vector() 
for (i in seq(length(x))) { 
  if(is.na(x[i])) {
    binary[i] = NA 
    else if(x[i] > 25) {
      binary[i] = "big"
    } else binary[i] = "small"
  }
  
  
split_size(d)
mass = split_size(d)
d$split_size = split_size(d)

split = vector() 
split_size = function(d) { 
  for(mass in d$body_mass_g){
    size = ifelse(mass > 4000, "big","small")
    split = c(size)}
}


#############################33
binary = vector() 
for (i in seq(length(x))) {
  if(is.na(x[i])) {
    binary[i] = NA
  }else if(x[i] > 25) { 
    binary[i] = "big"
  } else binary[i] = "small"
}

#hold for now 
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
binary(d, 4000) 

#Create a function that takes in two values: the data and the break value

body_mass_g = d$body_mass_g

mass = vector() 
binary = function(data, break_value) { 
  for (i in seq_length(data) {
    if(is.na(mass)) {
      next #skips stuff 
    } else if (data$body_mass_g >= break_value) {
      mass = 'large'
    } else {
       = 'small'
    }
  }
}

#Mini example without a function 
x = c(1,2,3,NA,60)
mass = ifelse(x>25, "big","small")
mass
mass = ifelse(d$body_mass_g > 4000, "big","small")
mass
d$mass = mass

#TA example 
binary = vector() 
for (i in seq(length(x))) { 
  if(is.na(x[i])) {
    binary[i] = NA 
else if(x[i] > 25) {
    binary[i] = "big"
  } else binary[i] = "small"
}

################ THIS WORKS DO NOT TOUCH ################
binary = vector()
for (i in seq(length(x))) {
  if(is.na(x[i])) {
    binary[i] = NA
  }else if(x[i] > 25) { 
    binary[i] = "big"
  } else binary[i] = "small"
}
binary
###################################################################

#[] means the location not hte item itself 
#vectors the [] refers to the actual value 
#for vectors, dataframes, and arrays only need one set of sq brackets
#for lists, [] means the locatoin and [[]] means the stuff inside 

x = c(1,2,3,NA)
y = c(2,5,6,9)
listy = list(x,y)
# to get the first element in the list you need hte double brakets, to get the delicious goodies inside oyu need single brackets 

listy[[1]] #gives you the first full thing in hte list 
listy[[1]][1] #gives you the first goody in list one, you nneed to chain them, so go to the first list, and then take the first element
vectory = c(x,y)
vectory[[3]] #vectors don't care about double square brackets? maybe 

#why is this like this? because lists can contain entire dataframes and other stuff 

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