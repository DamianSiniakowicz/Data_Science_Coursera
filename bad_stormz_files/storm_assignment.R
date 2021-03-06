# Most badass kinds of storms
#============================================================================

### Synopsis
# The independent variable was storm type, which was coded by EVTYPE
# The dependent variables were negative effects, coded by FATALITIES, INJURIES, PROPDMG, CROPDMG
# The data were grouped by storm type, and the mean of each negative effect was taken.
# The metrics were chosen: one for economic effects, the other for human effects.
# The data were arranged into two lists by the different metrics. 
# The three most destructive storms by each metric were prioritized for research into preventetive measures


## download file and packages and load into data frame
library(plyr)
library(dplyr)
library(ggplot2)

if(!(exists("stormz"))){
       download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",destfile = "stormz.bz2")
        stormz <- read.csv("stormz.bz2")
}


### Data Processing

## Econ Section

# step 1
cumulative_damage <- sum(stormz$PROPDMG + stormz$CROPDMG)

# step 2
stormz_by_type <- group_by(stormz, EVTYPE)

# step 3
stormz_damage <- summarize(stormz_by_type, relative_damage = sum(PROPDMG + CROPDMG)/cumulative_damage) %>%
                   filter(relative_damage > 0)
# step 4
storm_names <- as.character(stormz_damage[["EVTYPE"]])
library(stringi)
# make lower case and remove punctuation
lower_case_names <- stri_trans_tolower(storm_names)
lower_case_names <- gsub(pattern = "[[:punct:]]", replacement = " ", x = lower_case_names)
# create a list of vectors, where each vector represents one EVTYPE's key words
word_list <- stri_split_boundaries(lower_case_names)
# remove whitespace from each vector
new_word_list <- lapply(X = word_list, FUN = function(subVec) gsub(pattern="[[:space:]]", replacement = "", x = subVec))



# also get rid of all elements containing numbers


# this is the EVTYPE list where each vector contains 1 character string per word
EV_split <- lapply(X=new_word_list, FUN = function(subVec) subVec[!grepl(pattern="[[:digit:]]",x=subVec)]) 


# this is the EVTYPE list where each vector contains 1 character string
EV_merged <- lapply(X=EV_split, FUN = function(subVec) paste(subVec,collapse=" "))


# construct a keywords vector
words_draft <- stri_extract_all_words(EV_merged, simplify=TRUE)
dim(words_draft) <- NULL
words_draft <- stri_unique(words_draft)

### YEAH!

# step 1 : make thesaurus
thesaurus <- list(
                    list(c("cool","chill","windcold"), "cold"),
                    list(c("flooding","floods","flashflood","Flood"), "flood"),
                    list(c("freezing"), "freeze"),
                    list(c("hailstorm","windhail"), "hail"),
                    list(c("icy"), "ice"),
                    list(c("landslides"), "landslide"),
                    list(c("ligntning","lighting","light"),"lightning"),
                    list(c("mud","mudslides"),"mudslide"),
                    list(c("thunderstormind","thunderstorms","thuderstorm", "thundeerstorm", "thunderestorm","thunderstorms","thunderstormw","thunderstormwinds","thunderstrom","thundertorm","thunerstorm","thundersnow"), "thunderstorm"),
                    list(c("wildfires","fires"),"wildfire"),
                    list(c("wins","winds","squall","squalls","turbulence"),"wind"),
                    list(c("wintry"),"winter"),
                    list(c("trees"),"tree"),
                    list(c("snowfall","sleet","mix","precipitation"),"snow"),
                    list(c("rainfall","rains","drizzle","rainstorm"),"rain"),
                    list(c("tornadoes","torndao"),"tornado"),
                    list(c("tide","surge","seas","surf","swells"),"ocean"),
                    list(c("stream","river","floes","currents"),"rivers")
                )

#step 1.5 : define a standardize_name function
standardize_name <- function(event_name, variant_vector, standard_string){
                for(Variant in variant_vector){
                        #print(Variant)
                        if(grepl(Variant,event_name)){
                                event_name <- gsub(Variant,standard_string,event_name)
                                #print(event_name)
                                break
                        }
                }
                event_name
        }

# step 2 : Replace variants with standard
for(x in seq_along(EV_merged)){
                event <- EV_merged[[x]]
                
                for(entry in thesaurus){
                        variants <- entry[[1]]
                        standard <- entry[[2]]
                        event <- standardize_name(event,variants,standard)
                        EV_merged[x] <- event
                }
}


# step 3 : recreate keywords vector and EVTYPE list of n-long vectors
key_words <- stri_extract_all_words(EV_merged, simplify=TRUE)
dim(key_words) <- NULL
key_words <- stri_unique(key_words)

EV_split <- stri_split_boundaries(EV_merged)

# step 3.5 : make a, retrieve damage, function,
retrieve_damage <- function(Index){
        DAMAGE <- stormz_damage[[Index,"relative_damage"]]
        DAMAGE
}

# step 4 : create word-damage data frame and plot it
damage_vector <- numeric()
for(key_word in key_words){
        damage <- 0
        for(indeX in seq_along(EV_split)){
                Event <- EV_split[[indeX]]
                if(key_word %in% Event){
                        damage <- damage + retrieve_damage(indeX)
                }
        }
        damage_vector <- c(damage_vector,damage)
}

word_damage <- data.frame(word=key_words,damage=damage_vector)
word_damage <- filter(word_damage, damage >= .05)
word_damage <- arrange(word_damage, desc(damage))
trouble_makers <- word_damage[["damage"]]
names(trouble_makers) <- word_damage[["word"]]
trouble_df <- data.frame(word=names(trouble_makers),fraction_of_damage=trouble_makers)


#####################################################################################

## Death Section : our metric for human toll was death, injuries were excluded

# step 1
total_deaths <- sum(stormz$FATALITIES, na.rm = TRUE)

# step 2
stormz_by_type <- group_by(stormz, EVTYPE)

# step 3
stormz_death <- summarize(stormz_by_type, relative_deadliness = sum(FATALITIES)/total_deaths) %>%
    filter(relative_deadliness > 0)

# step 4
storm_names_2 <- as.character(stormz_death[["EVTYPE"]])
library(stringi)
# make lower case and remove punctuation
lower_case_names_2 <- stri_trans_tolower(storm_names_2)
lower_case_names_2 <- gsub(pattern = "[[:punct:]]", replacement = " ", x = lower_case_names_2)
# create a list of vectors, where each vector represents one EVTYPE's key words
word_list_2 <- stri_split_boundaries(lower_case_names_2)
# remove whitespace from each vector
new_word_list_2 <- lapply(X = word_list_2, FUN = function(subVec) gsub(pattern="[[:space:]]", replacement = "", x = subVec))

# also get rid of all elements containing numbers

# this is the EVTYPE list where each vector contains 1 character string per word
EV_split_2 <- lapply(X=new_word_list_2, FUN = function(subVec) subVec[!grepl(pattern="[[:digit:]]",x=subVec)]) 


# this is the EVTYPE list where each vector contains 1 character string
EV_merged_2 <- lapply(X=EV_split_2, FUN = function(subVec) paste(subVec,collapse=" "))


# construct a keywords vector
words_draft_2 <- stri_extract_all_words(EV_merged_2, simplify=TRUE)
dim(words_draft_2) <- NULL
words_draft_2 <- stri_unique(words_draft_2)

# step 1 : make thesaurus
thesaurus_2 <- list(
        list(c("ash","dust","volcanic"),"bad air"),
        list(c("winds","windcold","gusty","blowing"),"wind"),
        list(c("dry"),"drought"),
        list(c("freeze","frost"),"ice"),
        list(c("wildfire"),"fire"),
        list(c("windhail"),"hail"),
        list(c("lightningning"),"lightning"),
        list(c("landslump","landslide","mudslideslide","mudslideslides"),"mudslide"),
        list(c("whirlwind","gustnado","funnel","torndao"),"tornado")
)


# step 2 : Replace variants with standard
for(x in seq_along(EV_merged_2)){
    event_2 <- EV_merged_2[[x]]
    
    for(entry in thesaurus_2){
        variants <- entry[[1]]
        standard <- entry[[2]]
        event_2 <- standardize_name(event_2,variants,standard)
        EV_merged_2[x] <- event_2
    }
}


# step 3 : recreate keywords vector and EVTYPE list of n-long vectors
key_words_2 <- stri_extract_all_words(EV_merged_2, simplify=TRUE)
dim(key_words_2) <- NULL
key_words_2 <- stri_unique(key_words_2)

EV_split_2 <- stri_split_boundaries(EV_merged_2)

# step 3.5 : make a, retrieve death, function,
retrieve_death <- function(INDEX){
    DEATH <- stormz_death[[INDEX,"relative_deadliness"]]
    DEATH
}

# step 4 : create word-damage data frame and plot it
death_vector <- numeric()
for(key_word in key_words_2){
    death <- 0
    for(indexx in seq_along(EV_split_2)){
        EvenT <- EV_split_2[[indexx]]
        if(key_word %in% EvenT){
            death <- death + retrieve_death(indexx)
        }
    }
    death_vector <- c(death_vector,death)
}

word_death <- data.frame(word=key_words_2,death=death_vector)
word_death <- filter(word_death, death >= .05)
word_death <- arrange(word_death, desc(death))
killers <- word_death[["death"]]
names(killers) <- word_death[["word"]]
killer_df <- data.frame(word = names(killers),fraction_of_deaths = killers)
trouble_df <- data.frame(word=names(trouble_makers),fraction_of_damage=trouble_makers)

### Results

# first i got this sweet function for putting more than 1 plot on a page
# Source for the following function
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}


plot_1 <- ggplot(aes(x=word,y=fraction_of_deaths,fill=word),data=killer_df) + geom_bar(stat="identity") + geom_text(aes(label=paste(round(fraction_of_deaths,2)*100,"%"),vjust=1)) + ggtitle("% of deaths caused by events whose names include a certain word") + theme(plot.title = element_text(size=0))
plot_2 <- ggplot(aes(x=word,y=fraction_of_damage,fill=word),data=trouble_df) + geom_bar(stat="identity") + geom_text(aes(label=paste(round(fraction_of_damage,2)*100,"%"),vjust=1)) + ggtitle("% of total damage caused by events whose names include a certain word") + theme(plot.title = element_text(size=9))
multiplot(plot_1,plot_2,cols=2)



