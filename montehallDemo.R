#make a function to simulate one game

resample <- function(x,...) x[sample.int(length(x),...)]

simulate_mh_one <- function(user_door, # door that the player picked
                            switch, # if they want to switch after picking
                            n_doors = 3 # the number of doors that we can pick from
                            
){
  
  # this will make a vector of doors counting from 1 to 3
  doors <- seq_len(n_doors)
  
  # the door that will have a car behind it will be a random number
  car_door <- sample(doors, 1)
  
  #the host can only open a door that the user did not pick and
  #that the car is not behind
  doors_to_open_choices <- doors[-c(user_door, car_door)]
  
  doors_to_open <- resample(x = doors_to_open_choices,
                            size = n_doors - 2,
                            replace = TRUE)  # Bootstrap sampling implies replacement
  
  
  
  door_that_can_be_switched_to <- doors[-c(doors_to_open, user_door)]
  
  if(switch)
  {
    user_door <- door_that_can_be_switched_to
  }
  # Let's see if the user wins after the option to switch
  win <- user_door == car_door
  return(win)
}

output <- c()
for(i in 1:1000)
{
  output[i] <- simulate_mh_one(user_door = sample(1:3,1),switch = FALSE, n_doors = 3)
}

#count up all the true values and divide them by the length of the output

length(output[output == TRUE])/length(output)

mean(output)

barplot(output)

win_proportion <- mean(output)

# Create a bar plot

barplot(table(output),
        
        col = c("red", "green"),
        names.arg = c("Loss", "Win"),
        main = "Monty Hall Simulation Results",
        xlab = "Outcome",
        ylab = "Games Played")

# Add the win proportion as text

text(x = 1.5, y = max(table(output)),
     labels = paste("Win Proportion: ", round(win_proportion, 2)),
     pos = 3)