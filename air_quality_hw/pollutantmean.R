get_string_id <- function(id){
    #re-write the number
    if(id >= 100){
        string_id <- as.character(id)
    }
    else if(id >= 10){
        string_id <- paste("0", as.character(id), sep = "")
    }
    else{
        string_id <- paste("00", as.character(id), sep = "")
        
    }
    string_id
}

get_table <- function(directory,id_string){
    # gets a table give its id number as a string
    table <- read.csv(paste(directory,"/",id_string,".csv",sep=""))
    table
}

get_pollution_vec <- function(a_table, pollutant){
    # returns a numeric vector with all NA's removed
    has_na_vector <- a_table[pollutant]
    clean_vector <- has_na_vector[!is.na(has_na_vector)]
    clean_vector
}


pollutantmean <- function(directory,pollutant,id=1:332){
    # step 1 : get all the data frames indicated by id into a list
#   # step 1.1 : write a function that turns id's into characters
    # step 1.2 : write a function that gets a table given an id #
    # step 1.3 : write a function that gets the column of interest out of a table
        pollution_vector <- vector(mode="numeric")
        
        for(id_number in id){
                id_string <- get_string_id(id_number)
                one_table <- get_table(directory,id_string)
                one_pol_vec <- get_pollution_vec(one_table,pollutant)
                pollution_vector <- c(pollution_vector,one_pol_vec)
        }
        mean(pollution_vector)
}