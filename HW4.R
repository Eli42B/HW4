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

break_value = 4500 #based on our histogram, let's set our break_value to 4500 
binary(d$body_mass_g) #yep it works 
binary1 = binary(d$body_mass_g) #saving it as a vector
d$class_size = binary1 #bind it to the dataframe 

#######################################
# OBJECTIVE 2 
#######################################

# Nathan joined our group!!

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

  
 