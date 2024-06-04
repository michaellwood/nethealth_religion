####################################################################################################
################################## Loading Packages/Data ###########################################
####################################################################################################


#### all of my working directions (for the most part)
setwd("C:/Users/ethan/Dropbox/Ensembles")

### loading in initial data set to extract the psych scales
vertex_traits <- read.csv("vertex_traits_not3.csv",
                          sep = ",", na.strings = c("", " ", "NA"), header = TRUE)

####################################################################################################
################################### SUBSETTING DATA ################################################
####################################################################################################

#### creating a subset of vertex_traits (ego leve) for each wave
# put in row slot in matrix notation to remove all T3 subjects !(vertex_traits$tier_id == "T3")
vertex_traits1 <- vertex_traits[,c(1,9,12,15,18,35,37,39)] #### e.g. CESD_ego_1, BAI_ego_1 etc.
vertex_traits2 <- vertex_traits[,c(1,10,13,16,19,36,38,40)] #### e.g. CESD_ego_2, BAI_ego_2 etc.

####################################################################################################
################################ Creating initial lists ############################################
####################################################################################################

#### Creating lists of files in the folder so I can loop through each one
#--- lists are faster to loop through
files1 <- list.files(path="C:/Users/ethan/Dropbox/Ensembles/wave1", 
                    pattern = "*.csv", full.names=T, recursive=F)

files2 <- list.files(path="C:/Users/ethan/Dropbox/Ensembles/wave2", 
                      pattern = "*.csv", full.names=T, recursive=F)

#### Need to work with one network at a time in ram, loads in each file from filelist as list of data frames
#--- again, lists are much faster to iterate/loop through
network.list1 <- lapply(files1, read.table, sep = ",", header = T)

network.list2 <- lapply(files2, read.table, sep = ",", header = T)

####################################################################################################
########## Function that calculates average value on psych scale for each ego's alter ##############
####################################################################################################

#### my function (and consequently script in general) is dependent on igraph functions
library(igraph)

#### this function calculates the average value for a particular variable for each ego network
#--- within the whole network
alter_avg <- function(network, #### this function works with one attr_name at a time (just use lapply with list of ALL var names)
                      attr_name = "CESD_ego_1",
                      vertex_traits = vertex_traits){
  
  network <- graph_from_data_frame(network, directed = FALSE,
                                   vertices = vertex_traits)
  
  network <- make_ego_graph(network, order = 1)
  
  alter_mean <- vector('numeric')
  for (i in 1:length(network)){ 
    vector_mean <- vertex_attr(network[[i]], attr_name)
    vector_mean <- vector_mean[1:(length(vector_mean))] #####*** Double check this part Mike
    alter_mean[i] <- mean(vector_mean, na.rm = TRUE)
  }
  return(alter_mean)
}

####################################################################################################
########################### Using function for each simulation #####################################
####################################################################################################

#$%$^$%^$%^$%#$%^$%^$%^$%^$%^
######________________------- This is where I need to create the different categories of lists to run the function on
#----------- ie., psych cor mat vs personality corr mat (with w1 and w2)
#### creating list of variable names so I can use lapply in for loop
psych.list1 <- as.list(c("selsa_rom_ego_1", "selsa_fam_ego_1", "selsa_soc_ego_1", "selsa_ego_1",
                         "STAIState_ego_1", "CESD_ego_1", "BAI_ego_1"))
psych.list2 <- as.list(c("selsa_rom_ego_2", "selsa_fam_ego_2", "selsa_soc_ego_2", "selsa_ego_2",
                         "STAIState_ego_2", "CESD_ego_2", "BAI_ego_2"))

#### this code calculates everything as one list, but I kept it separated by wave
#psych.alter <- c(lapply(psych.list, function (x) alter_avg(network = network.list1[[1]], attr_name = x, vertex_traits = vertex_traits)),
#                 lapply(psych.list, function (x) alter_avg(network = network.list2[[1]], attr_name = x, vertex_traits = vertex_traits)))

#### creating empty lists to add each simulated corr matrix to
#--- need to make an empty list OUTSIDE for loop
cor1.list <- list()
cor2.list <- list()

for(i in 1:length(files1)){ #### for loop which calculates alter average for each ego on every psych scale for each simulated network
  ### length(list) instead of 500 -- abstracting

#### looping through each 'variable'
#--- each of these rows loops through each variable (psych scale) for each iteration of the simulated networks
  psych.alterw1 <- lapply(psych.list1, function (x) alter_avg(network = network.list1[[i]], attr_name = x, vertex_traits = vertex_traits1))
  psych.alterw2 <- lapply(psych.list2, function (x) alter_avg(network = network.list2[[i]], attr_name = x, vertex_traits = vertex_traits2))

#### converting list to data frame (couldn't figure out intuitive way to name lists for labels on corr matrices)
#--- which also leads to inconveniences later when plotting/table/graphing, traded efficiency for clarity
#--- for optimal calculations should loop through lists not data frames but still takes under 2 min....
  psych.df1 <- as.data.frame(psych.alterw1)
  psych.df2 <- as.data.frame(psych.alterw2)

#### renaming each column (trust me they were nasty)
  colnames(psych.df1)[1] <- "selsa_rom_alter_1"
  colnames(psych.df1)[2] <- "selsa_fam_alter_1"
  colnames(psych.df1)[3] <- "selsa_soc_alter_1"
  colnames(psych.df1)[4] <- "selsa_alter_1"
  colnames(psych.df1)[5] <- "STAIState_alter_1"
  colnames(psych.df1)[6] <- "CESD_alter_1"
  colnames(psych.df1)[7] <- "BAI_alter_1"

  colnames(psych.df2)[1] <- "selsa_rom_alter_2"
  colnames(psych.df2)[2] <- "selsa_fam_alter_2"
  colnames(psych.df2)[3] <- "selsa_soc_alter_2"
  colnames(psych.df2)[4] <- "selsa_alter_2"
  colnames(psych.df2)[5] <- "STAIState_alter_2"
  colnames(psych.df2)[6] <- "CESD_alter_2"
  colnames(psych.df2)[7] <- "BAI_alter_2"


#### merging datasets for each wave
  cor1 <- cbind(vertex_traits1, psych.df1) #### merging ego level var with alter average for w1
  cor2 <- cbind(vertex_traits2, psych.df2) #### merging ego level var with alter average for w2

  cor1.list[[i]] <- cor(cor1[,2:15], use = "complete.obs") #### storing each wave 1 corr mat for each simulated data set
  cor2.list[[i]] <- cor(cor2[,2:15], use = "complete.obs") #### storing each wave 2 corr mat for each simulated data set

} #### end loop --- should have list of cor matrices for each simulated network for both waves 1 and 2 (sep lists for each wave)

rm(cor1, cor2, psych.df1, psych.df2, vertex_traits1, vertex_traits2, files1, files2, network.list1, network.list2,
   psych.alterw1, psych.alterw2, psych.list1, psych.list2)
####################################################################################################
############################## Getting average corr's + Plots ######################################
####################################################################################################

#### loading corrplot --- let's us make pretty corr plots 
library(corrplot)

cor1.avg <- Reduce('+', cor1.list)/1000
cor2.avg <- Reduce('+', cor2.list)/1000

#### getting corr info from original/real data
#### Need the alter variables

vertex_traits1 <- as.data.frame(vertex_traits[,c(9,12,15,18,35,37,39,72,75,78,81,98,100,102)])
vertex_traits2 <- as.data.frame(vertex_traits[,c(10,13,16,19,36,38,40,73,76,79,82,99,101,103)])

cor.w1 <- cor(vertex_traits1, use = "complete.obs")
cor.w2 <- cor(vertex_traits2, use = "complete.obs")

rm(vertex_traits1, vertex_traits2)

#### writing each corr matrix for each simulated network of fall semester to txt file
sink("corr_mat_w1.txt")

print(cor1.list)

#### ends the file
sink()

#### writing each corr matrix for each simulated network of spring semester to txt file
sink("corr_mat_w2.txt")

print(cor2.list)

#### ends the file
sink()

#### writing averge correlation matrices to text files
sink("corr_mat_avg.txt")

print("########## WAVE 1 SIM ##########")
print(cor1.avg)
print(" ")
print("########## WAVE 1 REAL ##########")
print(cor.w1)
print(" ")
print("############################")
print("############################")
print("############################")
print(" ")
print("########## WAVE 2 SIM ##########")
print(cor2.avg)
print(" ")
print("########## WAVE 2 REAL ##########")
print(cor.w2)

sink()

####################################################################################################
############################## Getting average corr's + Plots ######################################
####################################################################################################

#apply(simplify2array(cor1.list), 1:2, sd)  # (Notes for ETHAN)---- need to learn what the '1:2' argument means
                                           # seems like it it means to take all rows and columns (but why not specify 
                                           # that in normal matrix notation?)
                                           # basically, need to get better with ARRAYS across all languages
                                           # yeah.... when dealing with arrays essentially dealing explicitly with dimensions
                                           # the 1:2 prob means take the sd across both dimensions (if it was 5d then 1:5)

cor.z.w1 <- (cor1.avg - cor.w1)/apply(simplify2array(cor1.list), 1:2, sd)
cor.z.w2 <- (cor2.avg - cor.w2)/apply(simplify2array(cor2.list), 1:2, sd)

sink("corr_zscore.txt")

print("####################################")
print("########### W1 Z-Scores ############")
print("####################################")
cor.z.w1
print(" ")
print("####################################")
print("####################################")
print(" ")
print("####################################")
print("########### W1 Z-Scores ############")
print("####################################")
cor.z.w2

sink()


#### creating corr plots for avg among all simulated networks for each semester

cor1.avg[c(),c()]
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor1.avg, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor2.avg, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor.w1, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor.w2, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)
