##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(gridExtra)


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId], title = as.character(title), genres = as.character(genres))

# if using R 4.0 or later:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId), title = as.character(title),genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% semi_join(edx, by = "movieId") %>% semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


#RMSE function 
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


##########################################################
# methods/analysis
##########################################################

## Data Exploration and Cleaning

### Overview

#The goal is to predict how many stars a user will give a specific movie
#Let's first explore the data
head(edx)

#dimension of the data, there are 9000055 ratings and 6 columns consisting of "userId",
#"movieId","rating", "timestamp","title" and "genres"  
dim(edx)
names(edx)

#69878 unique users and  10677 different movies
edx %>% summarize(n_users = n_distinct(userId), n_movies = n_distinct(movieId))

#we also need to remember that some movies get rated more often than others and some users rate more than others
#how often some movies are rated
edx %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")

#how often some user rate movies
edx %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  ggtitle("Users")

#movies with the most ranking 
edx %>% group_by(title) %>% summarize(count = n()) %>% arrange(desc(count))edx %>% group_by(title) %>% summarize(count = n()) %>% arrange(desc(count)) %>% slice(1:10)

#Most given ratings
edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>% arrange(desc(count))


### Genres Observations

#797 distinct genres, we can see that one movie can be of multiple genres, we can also observe
# that those genres are written in alphabetical order and not by most relevant which means that 
#we cannot simply pick the first genre listed as the main genre of a particular movie.
edx %>% summarize(n_genre =n_distinct(genres))
head(unique(edx$genres))

#popular genres , there are 20 distinct genres listed and 797 different combinations of genres
popular_genres <- edx %>% group_by(genres) %>% summarize(count = n()) %>% arrange(desc(count)) 
different_genres <- popular_genres %>% select(genres) %>% separate_rows(genres, sep = "\\|")%>% unique()
number_different_genres <- nrow(different_genres)
different_combination <- edx %>% summarize(n_genre = n_distinct(genres)) %>% .$n_genre
genres <- data_frame(n_genres = number_different_genres, n_combination_genres = different_combination)
genres %>% knitr::kable()


#every movie has a genre even if it is not listed "(no genres listed)"
edx %>% filter(genres == NA)
edx %>% filter(genres == "(no genres listed)")

#top 10 genres
edx %>% group_by(genres) %>% summarize(count = n()) %>% arrange(desc(count)) %>% slice(1:10)

### Timestamp Observations

#Timestamps represent seconds since midnight Coordinated Universal Time (UTC) of January 1, 1970.
new_edx <- edx %>% mutate(time=as_datetime(timestamp))
new_edx <- new_edx %>% mutate(year=year(time), month=month(time))
new_edx <- new_edx %>% mutate(season=case_when(.$month%in%c(12,1,2)~"Winter",.$month%in%c(3,4,5)~"Spring",
                                               .$month%in%c(6,7,8)~"Summer",.$month%in%c(9,10,11)~"Fall"))
#based on boxplot and distributions of ratings per season, the season column is not a determinant predictor
#in predicting the rating of a movie
new_edx %>% ggplot(aes(season,rating)) + geom_boxplot()
new_edx %>% group_by(season) %>% summarize(n=n())
new_edx %>% group_by(season) %>%summarize(m=mean(rating))
p_winter<- new_edx %>% filter(season=="Winter") %>% ggplot(aes(rating)) + geom_histogram() + ggtitle("Winter Ratings Distribution")
p_spring<- new_edx %>% filter(season=="Spring") %>% ggplot(aes(rating)) + geom_histogram() + ggtitle("Spring Ratings Distribution")
p_summer<- new_edx %>% filter(season=="Summer") %>% ggplot(aes(rating)) + geom_histogram() + ggtitle("Summer Ratings Distribution")
p_fall<- new_edx %>% filter(season=="Fall") %>% ggplot(aes(rating)) + geom_histogram() + ggtitle("Fall Ratings Distribution")
grid.arrange(p_winter,p_spring,p_summer,p_fall)

# analyzing per year, there seems to be more variability though it is not significant
new_edx %>% group_by(year) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(year, rating)) + geom_point() + geom_smooth() 

### Title and Year

#We can observe that the year of the movie appear in parantheses at the end of the title
#let's separate the year it came out from the title name
new_edx<-new_edx%>%mutate(year_movie = substr(title,nchar(as.character(title))-4,nchar(as.character(title))-1))
#we first need to convert this as a date, so we will add jan 1st to the year. This will allow us to extract the year later.
new_edx<- new_edx %>% mutate(year_movie=paste(year_movie,"-01-01", sep = ""))
new_edx<- new_edx %>% mutate(year_movie=year(year_movie))
new_edx %>%  group_by(year_movie) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(year_movie, rating)) +  geom_point()  + geom_smooth()


### Data Cleaning

#new edx dataframe 
new_edx<- new_edx%>% select(-time,-timestamp,-season,-month)
#new validation dataframe 
new_validation <- validation %>% mutate(time=as_datetime(timestamp))
new_validation <- new_validation %>% mutate(year=year(time))
new_validation<-new_validation%>%mutate(year_movie = substr(title,nchar(as.character(title))-4,nchar(as.character(title))-1))
new_validation<-new_validation%>%mutate(year_movie=paste(year_movie,"-01-01", sep = ""))
new_validation<-new_validation%>%mutate(year_movie=year(year_movie))
new_validation <- new_validation%>%select(-time,-timestamp)


## Models 

### The Naive Average Rating Movie Model

#calculating the average of all ratings
mu_hat <- mean(new_edx$rating)
mu_hat

#calculate the RMSE 
naive_rmse <- RMSE(new_validation$rating, mu_hat)
naive_rmse

#Store the value of the RMSE in a table
rmse_results <- data_frame(method = "Just the average rating", RMSE = naive_rmse)
rmse_results %>% knitr::kable()


### Movie Effect Model

mu <- mean(new_edx$rating) 
#caculate b_i
movie_avgs <- new_edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))


#observation
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

#calculate b_i for new_validation
predicted_ratings <- mu + new_validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

#calculate the RMSE 
model_1_rmse <- RMSE(predicted_ratings, new_validation$rating)
#store result
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
#show result
rmse_results %>% knitr::kable()

### Movie + User Effect Model

#observation
new_edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

#calculating b_u for new_edx
user_avgs <- new_edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

#calculating b_u for new_validation
predicted_ratings <- new_validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

#calculating the RMSE for this model
model_2_rmse <- RMSE(predicted_ratings, new_validation$rating)

#Storing the RMSE for this model
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
#show
rmse_results %>% knitr::kable()

### Movie + User + Year_Movie Effect Model

#calculating b_iym for new_edx
year_movie_avgs <- new_edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(year_movie) %>%
  summarize(b_iym = mean(rating - mu - b_i- b_u))

#calculating b_iym for new_validation
predicted_ratings <- new_validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_movie_avgs, by='year_movie') %>%
  mutate(pred = mu + b_i + b_u +b_iym) %>%
  .$pred

#calculating the RMSE for this model
model_3_rmse <- RMSE(predicted_ratings, new_validation$rating)

#Storing the RMSE for this model
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User + Year_Movie Effects Model",  
                                     RMSE = model_3_rmse ))
#show
rmse_results %>% knitr::kable()


### Movie + User + Year_Movie + Year rated Effects Model

#calculating b_iuy for new_edx
year_avgs <- new_edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_movie_avgs, by='year_movie') %>%
  group_by(year) %>%
  summarize(b_iuy = mean(rating - mu - b_i- b_u-b_iym))

#calculating b_iuy for new_validation
predicted_ratings <- new_validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_movie_avgs, by='year_movie') %>%
  left_join(year_avgs, by='year') %>%
  mutate(pred = mu + b_i + b_u +b_iym +b_iuy) %>%
  .$pred

#calculating the RMSE for this model
model_4_rmse <- RMSE(predicted_ratings, new_validation$rating)

#Storing the RMSE for this model
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User + Year_Movie + Year rated Effects Model",  
                                     RMSE = model_4_rmse ))
#show
rmse_results %>% knitr::kable()

### Movie + User + Year_Movie + Year rated + Genres Effects Model

#calculating b_g for new_edx
genre_avgs <- new_edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_movie_avgs, by='year_movie') %>%
  left_join(year_avgs, by='year') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i- b_u-b_iym-b_iuy))

#calculating b_g for new_validation
predicted_ratings <- new_validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(year_movie_avgs, by='year_movie') %>%
  left_join(year_avgs, by='year') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u +b_iym +b_iuy +b_g) %>%
  .$pred

#calculating the RMSE for this model
model_5_rmse <- RMSE(predicted_ratings, new_validation$rating)
#Storing the RMSE for this model
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User + Year_Movie + Year rated + Genres Effects Model",  
                                     RMSE = model_5_rmse ))
#show
rmse_results %>% knitr::kable()

### Regularized Movie + User Effect Model

#define the different lambdas
lambdas <- seq(0, 10, 0.25)

#cross validation algorithm
rmses <- sapply(lambdas, function(l){
  #average rating
  mu <- mean(new_edx$rating)
  
  #finding b_i for new_edx using l
  b_i <- new_edx  %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  #finding b_u for new_edx  using l
  b_u <- new_edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  #Calculating the RMSE with l
  predicted_ratings <- 
    new_validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, new_validation$rating))
})

#show graph
qplot(lambdas, rmses)  

#find minimum lambda 
lambda <- lambdas[which.min(rmses)]
lambda

#pick the lambda that minimises the RMSE
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))
#show
rmse_results %>% knitr::kable()

