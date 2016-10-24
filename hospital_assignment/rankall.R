# step 0 : import the dataset, and check that the outcome argument is valid
# step 1 : assign the outcome column number to a name, and convert the outcome column to a numeric
# step 2 : remove all observations which have NA for the outcome of interest
# step 3 : order the data frame by outcome, tiebreak by state
# step 4 : remove all columns except name and state
# step 5 : split by state
# step 6 : extract the rankth hosptial in each split
# step 7 : stitch a dataframe back together


rankall <- function(outcome,rank){
    
    # step 0
    all_outcomes <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    valid_outcome_columns <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    valid_outcome_names <- names(valid_outcome_columns)
    if(length(valid_outcome_names[valid_outcome_names == outcome]) == 0) stop("invalid outcome")    

    # step 1
    outcome_col_num <- valid_outcome_columns[[outcome]]
    all_outcomes[ , outcome_col_num] <- as.numeric(all_outcomes[ , outcome_col_num])
    
    # step 2
    no_nas_vec <- !is.na(all_outcomes[ , outcome_col_num])
    no_na_outcomes <- all_outcomes[no_nas_vec,]
    
    # step 3
    outcome_ordering <- order(no_na_outcomes[,outcome_col_num], no_na_outcomes$Hospital.Name)
    ordered_hospitals <- no_na_outcomes[outcome_ordering,]
    
    # step 4
    ordered_hospitals <- ordered_hospitals[,c(2,7)]
    
    # step 5
    OH_split_by_state <- split(ordered_hospitals, ordered_hospitals$State)
    
    # step 6
    get_nth_name <- function(df,rnk=rank){
            num_entries <- nrow(df)
            if(rnk == "best") return(df[[1,1]])
            if(rnk == "worst") return(df[[num_entries,1]])
            if(rnk > num_entries) return(NA)
            else return(df[[rnk,1]])
    }
    get_nth_state <- function(df,rnk=rank){
            num_entries <- nrow(df)
            if(rnk == "best") return(df[[1,2]])
            if(rnk == "worst") return(df[[num_entries,2]])
            if(rnk > num_entries) return(NA)
            else return(df[[rnk,2]])    
    }
    name_vector <- sapply(OH_split_by_state, get_nth_name)
    state_vector <- sapply(OH_split_by_state, get_nth_state)
    
    # step 7
    new_df <- data.frame(name_vector, state_vector)
    names(new_df) <- c("Hospital.Name","State")
    new_df
}