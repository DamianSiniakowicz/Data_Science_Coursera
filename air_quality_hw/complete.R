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

get_num_cc <- function(table){
    num_cc <- sum(complete.cases(table))
    num_cc
}

complete <- function(directory, id=1:332){
    # returns a data frame telling how many complete entries are in each file
    
    id_vec <- vector(mode="numeric")
    nobs <- vector(mode="numeric")
    
    for(a_id in id){
          id_vec <- c(id_vec,a_id)
          
          id_string <- get_string_id(a_id)
          table <- get_table("specdata",id_string)
          
          current_nobs <- get_num_cc(table)
          nobs <- c(nobs,current_nobs)
                    }
    id <- id_vec
    complete_data_frame <- as.data.frame(cbind(id,nobs))
    complete_data_frame
    }

# Error: object 'id_vec' not found
# wtf