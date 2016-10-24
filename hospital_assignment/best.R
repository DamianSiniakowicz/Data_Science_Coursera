### step 0 : read in the table
### step 1 : store the valid state and outcome values
### step 2 : check that state and outcome arguments passed to the function are valid
### step 3 : get a dataframe that only has observations of hospitals from the state passed into the function
### step 4 : remove all columns except the hospital name and death rate for the outcome passed into the function
### step 5 : remove all observations with NA's
### step 6 : Order the dataframe by deathrate, tiebreak by name
### step 7 : get and return the best hospital
## How do I check if an element appears in a vector

best <- function(state, outcome){
    
    # step 0
    all_outcomes <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    # step 1
    valid_state_names <- names(table(outcomes$State))
    valid_outcome_columns <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    valid_outcome_names <- names(valid_outcome_columns)
    
    # step 2
    if(length(valid_state_names[valid_state_names == state]) == 0) stop("invalid state")
    if(length(valid_outcome_names[valid_outcome_names == outcome]) == 0) stop("invalid outcome")
    
    # step 3
    the_state <- all_outcomes[all_outcomes$State==state,]
    
    # step 4
    outcome_index <- valid_outcome_columns[[outcome]]
    outcome_name_col <- the_state[,c(2, outcome_index)]

    # step 5
    outcome_name_col[,2] <- as.numeric(outcome_name_col[,2])
    outcome_name_col <- outcome_name_col[complete.cases(outcome_name_col),]
    
    # step 6
    the_order <- order(outcome_name_col[,2],outcome_name_col[,1])
    ordered_hospitals <- outcome_name_col[the_order,]
    
    
    the_best <- ordered_hospitals[1,1]
    the_best
}