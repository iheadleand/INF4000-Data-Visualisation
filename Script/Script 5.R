###################
#Data Visualization
###################


##############
#Libraries
############
library(tidyverse)
library(stringr)
library(gridExtra)
library(FSA)
library(GGally)


#############
#Import Data
############
song_metadata <-read_delim("songs.csv")
song_acoustic_features <-read_delim("acoustic_features.csv")
song_popularity <-read_delim("song_pop.csv")
artist_metadata <-read_delim("artists.csv")



##############
#Data Cleaning
##############

#Song Metadata
artists <-song_metadata %>%  
  select(artists) #Selects just artist column 

artists <- artists <-sub("\\:.*", "", song_metadata$artists) #Pulls out artist id
artists <-gsub(".*'(.+)'.*", "\\1", artists) #removes quotations of artist id
artists <-as.data.frame(artists) #Turns it into a dataframe

song_metadata <-cbind(artists, song_metadata) #binds the cleaned artist id to the song_metadata

song_metadata <-song_metadata[, -5] #removes unwanted variable old artist id 
song_metadata <-song_metadata[, -4] #removes unwanted variable billboard
colnames(song_metadata) <- c("artist_id", "song_id", "song_name", 
                             "Popularity", "Explicit", "Song_Type") #Renames columns 

#Artist Metadata 
artist_metadata <-artist_metadata %>% 
  select(artist_id, name, main_genre) #Removes unwanted artist meta data leaving just id and genre, name is left to check later merging

artist_metadata <-artist_metadata %>%  
  filter(main_genre != "-") #Removes rows with missing data for artist main genre 
colnames(artist_metadata) <- c("artist_id", "Artist_Name", "Genre") #Renames columns

#Acoustic Features 
song_acoustic_features <-na.omit(song_acoustic_features) #Removes single row with missing data 

#Song Popularity 
song_popularity <- song_popularity %>% 
  arrange(song_id, year) %>%  
  filter(duplicated(song_id) == FALSE) #Removes duplicated song ids removing of duplicated parirs the one with the  later year release date 



########################
#Producing Total Dataset
########################
total_data <-merge(song_metadata, song_acoustic_features, by= "song_id")
total_data <-merge(total_data, song_popularity, by= "song_id")
total_data <-merge(total_data, artist_metadata, by= "artist_id") #Merges all datasets used 

total_data <-total_data %>%  
  select(-c(song_name, Artist_Name, artist_id)) #Removes unwanted variables 

year_total <-total_data %>%  
  select(year, song_id) #Pulls out year and song id to produce decades column


total_data <- total_data %>%  
  mutate(Decade_of_Release =substr(year, 1, 3)) #Creates new column with first three numbers of each of the songs release year

total_data$Decade_of_Release <-paste0(total_data$Decade_of_Release, "0") #Adds 0 onto Deacde of release column to make full year

total_data$Decade_of_Release <-as.numeric(total_data$Decade_of_Release) #Changes variable to numeric



###############################
#Data Prep For Frequencies Plot
###############################

genre_counts <-total_data %>% 
  group_by(Genre) %>%  
  summarise(count= n()) #Produces a dataframe showing how many songs they are for each of the genres in the dataset
nrow(genre_counts) #here are 479 differTent genres represented in the final dataset


top_ten <- total_data %>% 
  filter(Genre== "album rock"|
           Genre== "adult standards"|
           Genre== "dance pop" |
           Genre== "contemporary country" |
           Genre== "classic soul" |
           Genre== "brill building pop" |
           Genre== "disco" |
           Genre== "karaoke" | 
           Genre== "bubblegum pop" |
           Genre== "alternative metal") #Creates data set showing only songs in top ten most represented genres
nrow(total_data)- nrow(top_ten) #Excludes 8134 rows from original data 

top_five <-total_data  %>%  
  filter(Genre== "album rock"|
           Genre== "adult standards"|
           Genre== "dance pop" |
           Genre== "contemporary country" |
           Genre== "classic soul") #Creates data set showing only songs in the top five most represented genres
nrow(total_data)- nrow(top_five) #Excludes 10453 rows from original data 


top_three <-total_data  %>%  
  filter(Genre== "album rock"|
           Genre== "adult standards"|
           Genre== "dance pop")

top_five$Genre <-factor(top_five$Genre, 
                        levels= names(sort(table(top_five$Genre), 
                                           decreasing = TRUE)))
top_ten$Genre <-factor(top_ten$Genre, 
                       levels= names(sort(table(top_ten$Genre), 
                                          decreasing = TRUE))) #Orders both of the data frames by frequency of genres to order visualizations 
Adult_Standards_only <- total_data %>%  
  filter(Genre== "adult standards")
Album_Rock_Only <- total_data %>%  
  filter(Genre== "album rock")
Dance_Pop_Only <-total_data %>%  
  filter(Genre== "dance pop")





#######################
#Plot 1: Frequency Plot 
#######################
frequency_plot <-ggplot(top_ten, aes(Genre))+
  geom_bar(colour= "black", aes(fill= Genre))+ #Set color here later to fit colour scheme 
  geom_text(stat = "count", aes(label= after_stat(count)), vjust = -1)+ #This adds frequency counts on top of bars
  theme_minimal()+
  labs(x= "Genre", 
       y= "Frequency", 
       title= "Number of Songs in the Ten Genres Most Represented in the Dataset")+
  theme(axis.text.x = element_text(angle = 65, hjust= 1))+ #Rotates labels for bars so they don't overlap
  theme(plot.title = element_text(hjust = 0.5))+ #Centers Plot Title
  scale_x_discrete(labels= c("Album Rock", "Adult Standards", 
                             "Dance Pop", "Contemporary Country", 
                             "Classic Soul", "Brill Building Pop", 
                             "Disco", "Karaoke", "Bubblegum Pop",
                             "Alternative Metal"))+
  scale_fill_manual(values= c("album rock"= "#21908c", 
                              "adult standards"= "#440154",
                              "dance pop"= "#FDE725", 
                              "contemporary country"= "darkgrey", 
                              "classic soul"= "darkgrey", 
                              "brill building pop" = "darkgrey", 
                              "disco"= "darkgrey", 
                              "karaoke"= "darkgrey", 
                              "bubblegum pop"= "darkgrey", 
                              "alternative metal"= "darkgrey"))+
  theme(legend.position = "none")


#################################
#Plot 2: Understanding Popularity  
#################################
popularity_measures_plot <-ggplot(top_three, aes(x= year_end_score, y= Popularity))+
  geom_point(aes(col= Genre, shape = is_pop))+
  theme_minimal()+
  scale_shape_discrete(name= "Is Hit Song?", 
                       labels= c("Not Hit Song", 
                                 "Hit Song"))+
  labs(x= "Year End Score", 
       y= "Popularity Score", 
       title= "Song Popularity Score against Song Year End Score by Genre and Whether Classed as a Hit")+
  scale_color_manual(values= c("adult standards" = "#440154", 
                               "album rock" = "#21908c", 
                               "dance pop"= "#fde725"), 
                     labels= c("Adult Standards", "Album Rock", "Dance Pop"))



##################
#Plot 3: Box Plots 
##################

kruskal.test(top_three$Popularity~ top_three$Genre, 
             data= top_three) 
dunnTest(top_three$Popularity~ top_three$Genre,
         data = top_three, 
         method = "bonferroni") # All plots are significantly different 

df1 <-data.frame(a= c(1, 1:3, 3), b= c(93, 95, 95, 95, 93))

boxplot_plot <-ggplot(top_three, aes(Genre, Popularity))+
  geom_boxplot(aes(fill= Genre))+
  theme_minimal()+
  theme(legend.position = "none")+
  scale_x_discrete(labels= c("Adult Standards", "Album Rock", "Dance Pop"))+
  labs(x= "Genre", 
       y= "Popularity Score", 
       title= "Popularity Scores of Songs from Different Genres", 
       caption = "* = Singificant difference between popularity scores of genres at 95% significance level")+
  scale_y_continuous(limits = c(0, 100))+
  geom_line(data = df1, aes(x= a, y= b))+
  annotate("text", x = 2, y = 97, label = "*", size = 8)+
  scale_fill_manual(values= c("adult standards" = "#440154", 
                              "album rock" = "#21908c", 
                              "dance pop"= "#FDE725"), 
                    labels= c("Adult Standards", "Album Rock", "Dance Pop"))


#############################
#Plot 4: Parallel Coordinates
#############################

top_3_popular <- top_three %>% 
  filter(Popularity >= 39)

parallel_corord_plot <-ggparcoord(top_3_popular, columns= c(10, 11, 12, 13, 14, 15 ,16, 17, 18), 
                                  groupColumn= 21)+
  theme(axis.text.x = element_text(angle = 55, hjust= 1),
        panel.background = element_blank())+
  geom_vline(xintercept = c(1, 2, 3, 4, 5, 6, 7, 8, 9))+
  labs(x= "Audio Variables", y= "Scaled Values", 
       title= "Parallel Coordinates of Popular Song Audio Data")+
  scale_x_discrete(labels= c("Acousticness", "Danceability", "Energy", 
                             "Instrumentalness", "Liveness", "Loudness", 
                             "Speechiness", "Valence", "Tempo"))+
  scale_color_manual(values= c("adult standards" = "#440154", 
                               "album rock" = "#21908c", 
                               "dance pop"= "#FDE725"), 
                     labels= c("Adult Standards", "Album Rock", "Dance Pop"))
parallel_corord_plot


############
#Plot 5: PCA
############
pca <-prcomp(top_3_popular[, c(6, 10:18)], scale= TRUE)
pca_df <-data.frame(
  Genre= top_3_popular$Genre, 
  PC1= pca$x[, 1], 
  PC2= pca$x[, 2])
pca_plot <-ggplot(pca_df, aes(PC1, PC2))+
  geom_point(aes(colour= Genre), size= 2)+
  theme_minimal()+
  scale_color_manual(values= c("adult standards" = "#440154", 
                               "album rock" = "#21908c", 
                               "dance pop"= "#FDE725"), 
                     labels= c("Adult Standards", "Album Rock", "Dance Pop"))+
  labs(x= "PC1", y= "PC2", 
       title= "PCA of Popular Song Audio Features")


pca_plot
