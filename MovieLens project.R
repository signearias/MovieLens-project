#Install latex
tinytex::install_tinytex()

##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Explore headings
head(edx)

# Have a look at the summary data
summary(edx)

# Compute the frequency of the ratings
edx %>%
  ggplot(aes(rating)) +
  geom_bar(fill = "darkgreen") +
  xlab("rating") +
  ylab("number of ratings")

# Compute RMSE
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Compute the initial model
mu_hat <- mean(edx$rating)
mu_hat

first_rmse <- RMSE(edx$rating, mu_hat)
first_rmse

# Compute the movie effect
mu_hat <- mean(edx$rating) 
movie_eff <- edx %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu_hat))

predicted_ratings <- mu_hat + edx %>% 
  left_join(movie_eff, by='movieId') %>%
  pull(b_i)

movie_eff_rmse <- RMSE(predicted_ratings, edx$rating)
movie_eff_rmse

# Comparison table
comp_table <- data_frame(Method = "The initial model", RMSE = first_rmse)
comp_table <- bind_rows(comp_table,
                          data_frame(Method="The movie effect",  
                                     RMSE = movie_eff_rmse))
comp_table %>% knitr::kable()

# Compute the user effect
user_eff <- edx %>%
  left_join(movie_eff, by = "movieId") %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu_hat - b_i))

predicted_ratings <- edx %>%
  left_join(movie_eff, by = "movieId") %>%
  left_join(user_eff, by = "userId") %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  pull(pred)

user_eff_rmse <- RMSE(predicted_ratings, edx$rating)
user_eff_rmse

#Comparison table
comp_table <- bind_rows(comp_table,
                        data_frame(Method="The user effect",  
                                   RMSE = user_eff_rmse))
comp_table %>% knitr::kable()

# Compute with regularisation
lambda <- seq(0, 10, 0.25)
rmse <- sapply(lambda, function(x){
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu_hat)/(n()+x))
  b_u <- edx %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu_hat)/(n()+x))
  predicted_ratings <- edx %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu_hat + b_i + b_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, edx$rating))
})

#Compute and plot lambda
qplot(lambda, rmse)

low_lambda <- lambda[which.min(rmse)]
low_lambda

rmse_regularisation <- min(rmse)
rmse_regularisation

#Final comparison table
comp_table <- bind_rows(comp_table,
                        data_frame(Method="Regularisation",  
                                   RMSE = rmse_regularisation))
comp_table %>% knitr::kable()