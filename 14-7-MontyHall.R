# 
# In the 1970s, there was a game show called “Let’s Make a Deal” and Monty Hall was
# the host. At some point in the game, contestants were asked to pick one of three
# doors. Behind one door there was a prize. The other doors had a goat behind them
# to show the contestant they had lost. After the contestant picked a door, before
# revealing whether the chosen door contained a prize, Monty Hall would open one of
# the two remaining doors and show the contestant there was no prize behind that
# door. Then he would ask “Do you want to switch doors?” What would you do?

# We can use probability to show that if you stick with the original door choice,
# your chances of winning a prize remain 1 in 3. However, if you switch to the
# other door, your chances of winning double to 2 in 3! This seems counterintuitive.
# Many people incorrectly think both chances are 1 in 2 since you
# are choosing between 2 options. Below we use a Monte Carlo simulation to see which
# strategy is better. Note that this code is written longer than it should be
# for pedagogical purposes.
B <- 10000
stick_strategy <- function() {
    # create doors
    doors <- as.character(1:3)
    # create the behind_door_ objects
    behind_door <- sample(c('goat', 'goat', 'car'))
    # identify the winning door
    prize <- doors[behind_door == 'car']
    # make a first pick
    pick <- sample(doors, 1)
    # identify show door (not the pick and not the price)
    show <- doors[!((doors == prize) | (doors == pick))] %>% sample(1)
    # choose the first picked door
    choose <- pick
    # compare to prize door
    choose == prize
}

switch_strategy <- function() {
    # create doors
    doors <- as.character(1:3)
    # identify the winning door
    behind_door <- sample(c('goat', 'goat', 'car'))
    # identify the winning door
    prize <- doors[behind_door == 'car']
    # make a first pick
    pick <- sample(doors, 1)
    # identify show door (not the pick and not the price)
    # if two are available, pick one
    show <- doors[!((doors == prize) | (doors == pick))] %>% sample(1)
    # get the switching door. not the one shown and not the picked
    switch <- doors[!(doors == show | doors == pick)]
    # choose the switch door
    choose = switch
    # compare to prize door
    choose == prize
}

replicate(B, stick_strategy()) %>% 
    mean()

replicate(B, switch_strategy()) %>% 
    mean()

# The Monte Carlo estimate confirms the 2/3 calculation.
# This helps us gain some insight by showing that we are removing
# a door, show, that is definitely not a winner from our choices.
# We also see that unless we get it right when we first pick,
# you win: 1 - 1/3 = 2/3.



