################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding")

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


#################################end of setup code

###########################exploratory data analysis
#check structure and dimensions of data
str(edx)
dim(edx)

##check if data needs to be cleaned
#check for NAs in data
edx %>% filter(is.na(userId))
edx %>% filter(is.na(movieId))
edx %>% filter(is.na(rating))
edx %>% filter(is.na(timestamp))
edx %>% filter(is.na(title))
edx %>% filter(is.na(genres))

#check for duplicates
duplicates <- edx[duplicated(edx), ]
duplicates

#calculate min and max of rating
min(edx$rating)
max(edx$rating)

#calculate the mean, median, and std dev of the edx set
mean_edx <- mean(edx$rating)
median_edx <- median(edx$rating)
sd_edx <- sd(edx$rating)

#histogram of ratings
hist(edx$rating, main = "Figure 1: Histogram of Ratings")

#install lubridate package
if (!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

#create date column from timestamp column
edx <- edx %>%
  mutate(date = as_datetime(timestamp))

#min and max of date
min(edx$date)
max(edx$date)

#graph date vs rating
#find avg rating by year
mean_by_yr <- data.table(year = rep(NA, 11), avg_rating = rep(NA, 11), n = rep(NA, 11))
mean_by_yr <- edx %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(avg_rating = mean(rating))

#filter out 1995 since there are only 2 nonrepresentive data points
mean_by_yr <- mean_by_yr %>%
  filter(year != 1995)

#graph date vs rating
mean_by_yr %>% ggplot(aes(x = year, y = avg_rating)) +
  geom_point() +
  ggtitle("Figure 2: Rating vs. Time of Rating")

#make table of genres
genres <- edx %>%
  group_by(genres) %>%
  summarize(n = n(),
            avg_rating = mean(rating)) %>%
  filter(n >= 10000)

#top 10 genres by rating
genres %>% top_n(10, avg_rating) %>%
  arrange(desc(avg_rating))

#bottom 10 genres by rating
genres %>% top_n(10, -avg_rating) %>%
  arrange(avg_rating)

##################################model creation
#calculate the expected rating deviation due to the specific film
spec_movie_dev <- edx %>%
  group_by(movieId) %>%
  summarize(spec_movie_dev = mean(rating - mean_edx))

#add specific movie deviation to edx table
edx <- edx %>%
  left_join(spec_movie_dev, by = "movieId")

#calculate the expected rating deviation due to the user	
user_dev <- edx %>%
  group_by(userId) %>%
  summarize(user_dev = mean(rating - mean_edx - spec_movie_dev))

#add user deviation to edx table
edx <- edx %>%
  left_join(user_dev, by = "userId")

#installs Metrics package for rmse calculation
if(!require(Metrics)) install.packages("Metrics", repos = "http://cran.us.r-project.org")

#if the predicted rating is between 0.5 and 5, give the predicted rating
#otherwise check to see if the rating is over 5 and return 5 if so,
#return 0.5 otherwise
predicted_rating_initial <- ifelse((mean_edx + edx$spec_movie_dev + edx$user_dev) < 5 & 
                                     (mean_edx + edx$spec_movie_dev + edx$user_dev) >= 0.5,
                                   mean_edx + edx$spec_movie_dev +edx$user_dev,
                                   ifelse((mean_edx + edx$spec_movie_dev +
                                             edx$user_dev) > 5, 5, 0.5))

#calculates rmse
train_rmse_initial <- rmse(predicted_rating_initial, edx$rating)

#explore alphas for regularization
#define sequence of alphas to test
alpha1 <- seq(0, 20, 2)

#calculate regularized predictions for each alpha
regularized_rmse <- sapply(alpha1, function(a) {
  #calculate regularized specific movie deviation
  regul_spec_movie_dev <- edx %>%
    group_by(movieId) %>%
    summarize(regul_spec_movie_dev = sum(rating - mean_edx)/(a + n()))
  
  #add specific movie deviation to edx table
  edx <- edx %>%
    left_join(regul_spec_movie_dev, by = "movieId")
  
  #calculate regularized user deviation
  regul_user_dev <- edx %>%
    group_by(userId) %>%
    summarize(regul_user_dev = sum(rating - mean_edx - regul_spec_movie_dev)/(a + n()))
  
  #add user deviation to edx table
  edx <- edx %>%
    left_join(regul_user_dev, by = "userId")
  
  #calculate predicted rating for each movie
  #if the predicted rating is between 0.5 and 5, give the predicted rating
  #otherwise check to see if the rating is over 5 and return 5 if so,
  #return 0.5 otherwise
  regul_predicted_rating <- ifelse((mean_edx + edx$regul_spec_movie_dev + 
                                      edx$regul_user_dev) < 5 & 
                                     (mean_edx + edx$regul_spec_movie_dev +
                                        edx$regul_user_dev) >= 0.5,
                                   mean_edx + edx$regul_spec_movie_dev +
                                     edx$regul_user_dev,
                                   ifelse((mean_edx + edx$regul_spec_movie_dev +
                                             edx$regul_user_dev) > 5, 5, 0.5))
  
  #calculate rmse
  return(rmse(regul_predicted_rating, edx$rating))
})
#print rmse vector
regularized_rmse

#explore alphas near zero for regularization
#define sequence of alphas
alpha2 <- seq(0, 1, 0.1)

#calculate regularized predictions for each alpha
regularized_rmse2 <- sapply(alpha2, function(a) {
  #calculate regularized specific movie deviation
  regul_spec_movie_dev <- edx %>%
    group_by(movieId) %>%
    summarize(regul_spec_movie_dev = sum(rating - mean_edx)/(a + n()))
  
  #add specific movie deviation to edx table
  edx <- edx %>%
    left_join(regul_spec_movie_dev, by = "movieId")
  
  #calculate regularized user deviation
  regul_user_dev <- edx %>%
    group_by(userId) %>%
    summarize(regul_user_dev = sum(rating - mean_edx - regul_spec_movie_dev)/(a + n()))
  
  #add user deviation to edx table
  edx <- edx %>%
    left_join(regul_user_dev, by = "userId")
  
  
  #if the predicted rating is between 0.5 and 5, give the predicted rating
  #otherwise check to see if the rating is over 5 and return 5 if so,
  #return 0.5 otherwise
  regul_predicted_rating <- ifelse((mean_edx + edx$regul_spec_movie_dev + 
                                      edx$regul_user_dev) < 5 & 
                                     (mean_edx + edx$regul_spec_movie_dev +
                                        edx$regul_user_dev) >= 0.5,
                                   mean_edx + edx$regul_spec_movie_dev +
                                     edx$regul_user_dev,
                                   ifelse((mean_edx + edx$regul_spec_movie_dev +
                                             edx$regul_user_dev) > 5, 5, 0.5))
  
  #calculate rmse
  return(rmse(regul_predicted_rating, edx$rating))
}) 
#print rmse vector
regularized_rmse

#define optimal alpha and rmse values
alpha_final <- alpha2[which.min(regularized_rmse2)]
train_rmse_final <- min(regularized_rmse2)

##final model
#calculate regularized specific movie deviation in order to add to edx table
regul_spec_movie_dev <- edx %>%
  group_by(movieId) %>%
  summarize(regul_spec_movie_dev = sum(rating - mean_edx)/(alpha_final + n()))

#add specific movie deviation to edx table
edx <- edx %>%
  left_join(regul_spec_movie_dev, by = "movieId")

#calculate regularized user deviation in order to add to edx table
regul_user_dev <- edx %>%
  group_by(userId) %>%
  summarize(regul_user_dev = sum(rating - mean_edx - regul_spec_movie_dev)/
              (alpha_final + n()))

#add user deviation to edx table
edx <- edx %>%
  left_join(regul_user_dev, by = "userId")

#############################################validation
#add user deviation to validation table to allow rmse calculation
validation <- validation %>%
  left_join(regul_spec_movie_dev, by = "movieId")

#add user deviation to validation table to allow rmse calculation
validation <- validation %>%
  left_join(regul_user_dev, by = "userId")

#calculates predicted rating for each movie
#if the predicted rating is between 0.5 and 5, give the predicted rating
#otherwise check to see if the rating is over 5 and return 5 if so,
#return 0.5 otherwise
predicted_rating_final <- ifelse((mean_edx + validation$regul_spec_movie_dev + 
                                    validation$regul_user_dev) < 5 & 
                                   (mean_edx + validation$regul_spec_movie_dev +
                                      validation$regul_user_dev) >= 0.5,
                                 mean_edx + validation$regul_spec_movie_dev +
                                   validation$regul_user_dev,
                                 ifelse((mean_edx +
                                           validation$regul_spec_movie_dev +
                                           validation$regul_user_dev) > 5, 5, 0.5))
rmse <- rmse(predicted_rating_final, validation$rating)

#create histogram of predictions
hist_pred <- hist(predicted_rating_final, main = "Figure 5: Histogram of Predicted Ratings")
hist_real <- hist(validation$rating, main = "Figure 6: Histogram of Actual Ratings")


