#Keyword analysis

## Goal
#        *To plot % of total damage done by storms whose EVTYPE includes the string "X"*
#        for population damage, just focus on deaths
## Steps
        
        ### step 1
                    # Get the cumulative property/crop damage by 
                    # cumulative_damage <- with(stormz, sum(PROPDMG + CROPDMG))
        ### step 2
                    # Create a data frame of storms sorted by EVTYPE
                    # stormz_by_type <- group_by(stormz, EVTYPE)
        ### step 3
                    # Summarize each group by its percentage of cumulative damage, remove entries whose metric value is 0 
                    # sum(PROPDMG + CROPDMG) / cumulative_damage
        ### step 4
                    # get a vector of key-words appearing in historically impactful storm types
                            # step 4.1
# Goal : 
# Three Objects
# a character vector where each string is a unique keywords
# a list of vectors where each vector is a single string; the EVTYPE
# a list of n-long vectors where n is the number of keywords in the corresponding EVTYPE 

# Start:
# storm_names: a character vector of EVTYPE strings

# Step 1:
# make lower case
# replace all punctuation with spaces
# convert to a list of length(num_key_words) vectors
# remove all white_space from the vectors
# get a vector of unique keywords, at this point            
        ### step 5
                    # Use the storm key words stored in the vector as the names of a list whose values are integer vectors which correspond to the indices of entries in the stormz dataframe whose EVTYPEs contain the keyword
        ### step 6
                    # replace the list's integer vectors representing indices with the (total damage caused by storms of the type indicated by the indices / cumulative damage caused by all storms)
        ### step 7
                    # convert the named list of length 1 vectors into a dataframe
        ### step 8
                    # make a bar plot whose categories are keywords, and values are the proportion of total damage done by storms whose name contains the keyword 

# later you can decided if and how to facet this data by year

# 


standardize_variants <- function(a_vector){
    # will standardize all keywords in a_vector
    variant_list <- list()
    standard_list <- 
}



# replacing variants
        # write a function that replaces variants with a standed
            # store variants in one list and replacement in another
        # apply over each vector in the list
