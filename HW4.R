# Simulating the number of matches of however many individuals' birthdays
simulate_birthday_matches <- function(num_people = 23) {
  # Sets the seed for generating a random number 
  set.seed(314159)
  
  # Generates random birthdays for however many individuals
  birthdays <- sample(1:365, num_people, replace = TRUE)
  
  # Counts the number of unique birthdays, or days with at least 1 match
  num_matches <- length(birthdays) - length(unique(birthdays))
  
  # Returns the number of matches
  return(num_matches)
}

# Run simulations for different numbers of people (10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
num_people <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
num_simulations <- 100000 # Number of simulations for each number of people
results <- numeric(length(num_people)) # Stored results with vector

# Loop through different numbers of people and simulate matches
# I used the following website to help with setting up the loop with vector. "https://englelab.gatech.edu/useRguide/intermediate-r.html"
for (i in seq_along(num_people)) {
  matches <- replicate(num_simulations, simulate_birthday_matches(num_people[i]))
  results[i] <- mean(matches) # Stores the average number of matches
}

# Summary statistics
summary(results)
results
# Created my bar plot with the results. I used the following link to help create my plot. "https://r-graph-gallery.com/209-the-options-of-barplot.html"
barplot(results, names.arg = num_people, xlab = "Number of People",
        ylab = "Average Number of Matches",
        main = "Distribution of Birthday Matches",
        col = "steelblue", border = NA)

# The probability of having at least one pair of people with the same birthday
# greatly increases as the number of people in a group increases.
# Right skewed distribution
# Distribution might be varied a little bit in each simulation because 
# the randomness factor, but overall it is shown that there is a higher probability of getting matches 
# as the number of people increases
