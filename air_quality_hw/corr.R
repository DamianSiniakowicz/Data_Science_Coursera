get_table <- function(directory,file_name){
    # gets a table give its id number as a string
    table <- read.csv(paste(directory,"/",file_name,sep=""))
    table
}

get_num_cc <- function(table){
    num_cc <- sum(complete.cases(table))
    num_cc
}

corr <- function(directory, threshold = 0){
      
    num_of_files <- length(dir(directory))
    all_files <- dir(directory)
    correlation_vector <- vector(mode = "numeric")
    count <- 0
    for(a_file in all_files){
            count <- count + 1
            print(count)
            a_table <- get_table(directory,a_file)
            num_cc <- get_num_cc(a_table)
            
            if(num_cc > threshold){
                    table_minus_na <- a_table[complete.cases(a_table),]
                    the_pltn_corr <- cor(table_minus_na[["sulfate"]],table_minus_na[["nitrate"]])
                    correlation_vector <- c(correlation_vector,the_pltn_corr)
            }
            
    }
    correlation_vector
    
}