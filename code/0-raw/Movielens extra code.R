#number of different movies
edx %>% distinct(edx$movieId) %>% nrow()

#number of different users 
edx %>% distinct(edx$userId) %>% nrow()

#number of movie ratings in following genres:
edx %>% filter(str_detect(genres, "Drama")) %>% nrow() 
edx %>% filter(str_detect(genres, "Comedy")) %>% nrow()
edx %>% filter(str_detect(genres, "Thriller")) %>% nrow()
edx %>% filter(str_detect(genres, "Romance")) %>% nrow()

#movie with most ratings
edx %>% group_by(title) %>%
  summarize(numRatings = length(rating)) %>%
  top_n(10, numRatings) %>%
  arrange(desc(numRatings))

```{r}
alpha <- seq(0, 1, 0.1)
mu <- mean_edx
just_the_sum <- edx %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())

reg_rmse3 <- sapply(alpha, function(l) {
  mu <- mean_edx
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- edx %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, edx$rating))
})
```

# sample_index <- createDataPartition(y = edx$rating, times = 1,
# 									p = 0.1, list = FALSE)
# sample_data <- edx[sample_index,]
#find the avg rating by genre, for genres with over 10,000 ratings

#installs gridExtra package for histogram visualization
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")