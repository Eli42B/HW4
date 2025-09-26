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
library(tidyverse)

#############################################
# OBJECTIVE 1 
#############################################

#Create a function to convert a continuous variable into a binary variable (e.g., high/low, yes/no, 1/0, etc.). The function should allow the user to specify the breakpoint by which the data are divided in two, as well as specify the labels that both groups are assigned
#make a very basic function to divide sizes into either big or small

#Part a - create the function
size_class <- function(data, break_point, size1, size2) {
  ifelse(data > break_point, size2, size1)
}

#Part b - Apply function to the palmer penguin dataset
penguins$class <- size_class(data = penguins$body_mass_g, break_point = 4000, size1 = "small",  size2 = "large")


#######################################
# OBJECTIVE 2 
#######################################

# Nathan joined our group!!
# Part 2a
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


##################
# OBJECTIVE 3
##################

#Part a

#we first need to subset our penguins df by species

adelie_df <- subset(penguins, species == "Adelie") #df for Adelie penguins
quantile(adelie_df$body_mass_g, na.rm = TRUE) #specify na.rm = T to not get thrown errors
#looks like small penguins are less than the 50% quantile, so <3700

gentoo_df <- subset(penguins, species == "Gentoo")
quantile(gentoo_df$body_mass_g, na.rm = TRUE)
#small penguins < 5000

chinstrap_df <- subset(penguins, species == "Chinstrap")
quantile(chinstrap_df$body_mass_g, na.rm = TRUE)
#small penguins < 3700, very close to adelie in size it seems

# Question 3b - update our function to account for species differences
continuous_to_categorical_by_species <- function(dataframe, species_vector, species_specific_breakpoints, category_labels) {
  # This assumes the species and breakpoint species order match, 
  # and that the breakpoints and label_categories vectors are ordered,
  # and that each breakpoints vector is 1 element smaller than label_categories.
  # This also assumes our dataframe will have values and species in the "body_mass_g" and "species" columns
  # We'll store the results in a new column called body_mass_cat and return the updated dataframe
  
  # We will be subsetting the dataframe and will need to join them together after proccessing
  dataframes_storage <- list()
  # Iterate through the species first and foremost
  for (species_number in seq(length(species_vector))) {
    # First isolate the rows with the species in question ([rows, cols])
    relevant_entries <- dataframe[dataframe$species == species_vector[species_number],]
    # Get the breakpoints - R does not support vectors in vectors so species_specific_breakpoints must be a list
    species_breakpoints <- species_specific_breakpoints[[species_number]]
    # Make storage vector for our current species
    converted_values <- character(nrow(relevant_entries))
    # Iterate through each of the values for the current species
    relevant_values <- relevant_entries$body_mass_g
    for (value_number in seq(length(relevant_values))) {
      # Deal with NA again
      if (is.na(relevant_values[value_number])) {
        converted_values[value_number] <- NA
        # Continue with the loop
        next
      }
      # Otherwise evaluate if it's less than or equal to each breakpoint at a time
      for (breakpoint_number in seq(length(species_breakpoints))) {
        # If value is less than breakpoint we assign label and stop looping
        if (relevant_values[value_number] <= species_breakpoints[breakpoint_number]) {
          # Assign label 
          converted_values[value_number] <- category_labels[breakpoint_number]
          # Get out of the for loop
          break
        }
      }
      # If label wasn't assigned yet after we hit the last breakpoint it's the last label
      if (converted_values[value_number] == "") {
        # Assign as the last label
        converted_values[value_number] <- tail(category_labels, n=1)
      }
    }
    # Add the converted values to the relevant entries
    relevant_entries$body_mass_sml_by_species <- converted_values
    # Store the finished subset for later joining
    dataframes_storage[[species_number]] <- relevant_entries
  }
  # Join the subsetted dataframes back together
  updated_dataframe <- do.call(rbind, dataframes_storage)
  # End by returning the new dataframe
  return(updated_dataframe)
}

# Question 3c - apply this to penguins holy moly it actually works after like 20 tries
size_categories_by_species_penguins <- continuous_to_categorical_by_species(dataframe=penguins,
                                                                            species_vector=c("Adelie", "Gentoo", "Chinstrap"),
                                                                            species_specific_breakpoints=list(c(3450, 3900), c(4800, 5350), c(3583, 3833)),
                                                                            category_labels=c("small", "medium", "large"))

##################
# OBJECTIVE 4
##################

# Question 4 - make a box plot
continuous_to_categorical_by_species_boxplot <- function(dataframe, species_vector, species_specific_breakpoints, category_labels) {
  # This assumes the species and breakpoint species order match, 
  # and that the breakpoints and label_categories vectors are ordered,
  # and that each breakpoints vector is 1 element smaller than label_categories.
  # This also assumes our dataframe will have values and species in the "body_mass_g" and "species" columns
  # We'll store the results in a new column called body_mass_cat and return the updated dataframe
  
  # We will be subsetting the dataframe and will need to join them together after proccessing
  dataframes_storage <- list()
  # Iterate through the species first and foremost
  for (species_number in seq(length(species_vector))) {
    # First isolate the rows with the species in question ([rows, cols])
    relevant_entries <- dataframe[dataframe$species == species_vector[species_number],]
    # Get the breakpoints - R does not support vectors in vectors so species_specific_breakpoints must be a list
    species_breakpoints <- species_specific_breakpoints[[species_number]]
    # Make storage vector for our current species
    converted_values <- character(nrow(relevant_entries))
    # Iterate through each of the values for the current species
    relevant_values <- relevant_entries$body_mass_g
    for (value_number in seq(length(relevant_values))) {
      # Deal with NA again
      if (is.na(relevant_values[value_number])) {
        converted_values[value_number] <- NA
        # Continue with the loop
        next
      }
      # Otherwise evaluate if it's less than or equal to each breakpoint at a time
      for (breakpoint_number in seq(length(species_breakpoints))) {
        # If value is less than breakpoint we assign label and stop looping
        if (relevant_values[value_number] <= species_breakpoints[breakpoint_number]) {
          # Assign label 
          converted_values[value_number] <- category_labels[breakpoint_number]
          # Get out of the for loop
          break
        }
      }
      # If label wasn't assigned yet after we hit the last breakpoint it's the last label
      if (converted_values[value_number] == "") {
        # Assign as the last label
        converted_values[value_number] <- tail(category_labels, n=1)
      }
    }
    # Add the converted values to the relevant entries
    relevant_entries$body_mass_sml_by_species <- converted_values
    # Store the finished subset for later joining
    dataframes_storage[[species_number]] <- relevant_entries
  }
  # Join the subsetted dataframes back together
  updated_dataframe <- do.call(rbind, dataframes_storage)
  # End with making a boxplot
  boxplot(body_mass_g ~ species + body_mass_sml_by_species, 
          data=updated_dataframe, names=c("", "Large", "", "", "Medium", "", "", "Small", ""),
          col=c("blue", "green", "yellow"), main="Penguin body masses by size categories and species",
          cex.lab=1, cex.axis=1, xlab="Body size categories", ylab="Body mass (g)")
  legend("topright", fill=c("blue", "green", "yellow"), 
         legend=c("Adelie", "Gentoo", "Chinstrap"))
}

# Run it with penguins
size_categories_by_species_penguins <- continuous_to_categorical_by_species_boxplot(dataframe=penguins,
                                                                                    species_vector=c("Adelie", "Gentoo", "Chinstrap"),
                                                                                    species_specific_breakpoints=list(c(3450, 3900), c(4800, 5350), c(3583, 3833)),
                                                                                    category_labels=c("small", "medium", "large"))


# Another way to create the boxplot (moving the legend) 
#Note: you need to have still changed the penguins dataframe and bound a new column with the correct classes on it for this to work 

#library(tidyverse)
p = ggplot(penguins, aes(x = body_mass_sml, y = body_mass_g, fill = species)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of Penguin Sizes by Species", x = "Size", y = "Body Mass (g)") +
  theme_classic()

p + scale_x_discrete(labels = c("large" = "Large", "medium" = "Medium", "small" = "Small"))
#p is storing it, ggplot calls the tidyverse library, penguins is our data, we set our x and y, and we 'fill' ie group by species. Ggplot works in layers, so then we add on to this that we want it to be a boxplot (geom_boxplot()), and then we can add personalized labels (labs) and specificy a theme (I did classic, there is also a colorblind friendly version). 
#Then if we want to personalize teh x axis labels we call the graph again and add on the scale_x_discrete function 

##################
# OBJECTIVE 5
##################

# Question 5 - the most hands-off version possible with no breakpoint specifications

# No breakpoints needed because we are automatically determining them inside
# Breakpoints determiner is where we have already-established groups with unique breakpoints 
# Like penguin species (each species gets a set of breakpoints)
# Values to assign group by is the continuous variable
# We'll store the results in a new column called assigned_category and return the updated dataframe
ok_final_function <- function(dataframe, breakpoints_determiner_colname,
                              values_to_assign_group_by_colname, category_labels) {
  # First make the storage column
  dataframe$assigned_category <- ""
  # We will be subsetting the dataframe and will need to join them together after proccessing
  dataframes_storage <- list()
  # First we need to see how many breakpoints we will have
  how_many_breakpoints <- length(category_labels) - 1
  # We will start by establishing the breakpoints for each group
  # Turns out R takes the variable name after $ literally so we need to pull the column with [[]]  
  for (breakpoint_determiner in unique(dataframe[[breakpoints_determiner_colname]])) {
    # Take out the observations in the group to see the distribution
    relevant_values <- dataframe[(dataframe[[breakpoints_determiner_colname]] == breakpoint_determiner), values_to_assign_group_by_colname][[values_to_assign_group_by_colname]]
    # this works too: relevant_values <- subset(dataframe, breakpoints_determiner_colname == breakpoint_determiner)
    # Use quantile to determine our breakpoints (it looks messy but gets rid of the 0th and 100th quantile)
    # This is a double apparently
    breakpoints_for_this_group <- quantile(relevant_values, na.rm=TRUE,
                                           probs=seq(from=(1/(how_many_breakpoints + 1)), 
                                                     to=(how_many_breakpoints/(how_many_breakpoints + 1)), 
                                                     by=(1/(how_many_breakpoints + 1))))
    # Make storage vector for our current species
    converted_values <- character(length(relevant_values))
    # Iterate through each of the values for the current species
    for (value_number in seq(length(relevant_values))) {
      # Deal with NA values
      if (is.na(relevant_values[value_number])) {
        converted_values[value_number] <- NA
        # Continue with the loop
        next
      }
      # Otherwise evaluate if it's less than or equal to each breakpoint at a time
      for (breakpoint_number in seq(length(breakpoints_for_this_group))) {
        # If value is less than breakpoint we assign label and stop looping
        if (as.numeric(relevant_values[value_number]) <= breakpoints_for_this_group[breakpoint_number]) {
          # Assign label 
          converted_values[value_number] <- category_labels[breakpoint_number]
          # Get out of the for loop
          break
        }
      }
      # If label wasn't assigned yet after we hit the last breakpoint it's the last label
      if (converted_values[value_number] == "") {
        # Assign as the last label
        converted_values[value_number] <- tail(category_labels, n=1)
      }
    }
    # Add the converted values to the relevant entries
    dataframe[(dataframe[[breakpoints_determiner_colname]] == breakpoint_determiner), ]$assigned_category <- converted_values
  }
  return(dataframe)
}

# Testing with 5 categories
with_5_categories <- ok_final_function(penguins, "species", "body_mass_g", c("Extra Small", "Small", "Medium", "Large", "Extra Large"))
# Box plot for fun
boxplot(body_mass_g ~ species + assigned_category, 
        data=with_5_categories, names=c("", "XL", "", "", "L", "", "", "M", "", "", "S", "", "", "XS", ""),
        col=c("blue", "green", "yellow"), main="Penguin body masses by size categories and species",
        cex.lab=1, cex.axis=1, xlab="Body size categories", ylab="Body mass (g)")
legend("topright", fill=c("blue", "green", "yellow"), 
       legend=c("Adelie", "Gentoo", "Chinstrap"))
