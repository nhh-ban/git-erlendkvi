#### Assignment 5 - Git - BAN400 ####

library(tidyverse)

## Problem 2

# Storing the raw data file as lines
galaxies_lines <- readLines(con = "suites_dw_Table1.txt")

# Identifying the line number L in which the separator line is
L <- grep("-+-", galaxies_lines, value = FALSE)

# Storing the variable description in a text file
cat(readLines("suites_dw_Table1.txt", n = L-2),
    file = "var_description.txt",
    sep = "\n")

# Extracting variable names
var_names <- 
  str_split(string = galaxies_lines[L-1], pattern = "\\|") %>% 
  unlist() %>% 
  str_trim()

# Removing separators and replacing with commas
comma_separated_values <- 
  galaxies_lines[L+1:length(galaxies_lines)] %>% 
  gsub("\\|", ",", .) %>% 
  gsub(" ", "", .)

# Adding the variable names
comma_separated_values_with_names <- 
  c(paste(var_names, collapse = ","),
    comma_separated_values) 

# Creating a local .csv file
cat(comma_separated_values_with_names, 
    sep = "\n",
    file = "galaxies.csv")

# Reading the final data frame
galaxies <- read_csv("galaxies.csv")

## Problem 3

# Plotting the diameter and mass of all galaxies in a histogram
galaxies %>% 
  ggplot(aes(x = log_lk, y = log(a_26))) +
  geom_point(alpha = .5) +
  xlab("Stellar Mass (Solar Units, log)") +
  ylab("Linear Diameter (kpc, log)") +
  ggtitle("Diameter and Mass of Galaxies")

# The reason for this possible under-representation may be that the smaller
# galaxies are harder to locate, for example due to low luminosity, large 
# distance, etc. 

## Problem 4

# Storing the raw data file as lines
velocity_lines <- readLines(con = "UCNG_Table4.txt")

# Extracting variable names
var_names_velo <- 
  str_split(string = velocity_lines[1], pattern = "\\|") %>% 
  unlist() %>% 
  str_trim()

# Removing separators and replacing with commas
csv_velo <- 
  velocity_lines[3:length(velocity_lines)] %>% 
  gsub("\\|", ",", .) %>% 
  gsub(" ", "", .)

# Adding the variable names
csv_with_names <- 
  c(paste(var_names_velo, collapse = ","),
    csv_velo) 

# Creating a local .csv file
cat(csv_with_names, 
    sep = "\n",
    file = "velocity.csv")

# Reading the final data frame
velocity <- read_csv("velocity.csv")

# Joining the two .csv files together
joint <- galaxies %>% 
  inner_join(velocity, join_by(name))

# Plotting velocity and distance of each galaxy
joint %>% 
  ggplot(aes(x = cz, y = D)) +
  geom_point(alpha = .5) +
  xlab("Velocity") +
  ylab("Diameter") +
  ggtitle("Velocity and Diameter of Galaxies")

# From what we observe in the plot above, it seems like Hubble's observation was
# correct. We do, in fact, see that the further a galaxy is away from us, the higher
# the speed the with which it travels is.

# Estimating Hubble's constant using our data set
joint %>% 
  mutate(H = cz / D) %>% 
  summarise(H_estimate = mean(H))

# Using our data set, we get an estimated value of Hubble's constant of 85.1 
# (km/s)/Mpc. The estimate is a little higher than the "official" approximate 
# value of 70 (km/s)/Mpc.


  