#_____________________________________________
# Movielens project for Edx capstone
# Student: Luis Vazquez de Lara
# Date: September nineth, 2020
#______________________________________________

library(tidyverse)
library(lubridate)
library(caret)

coldef <- 'darkolivegreen4'
filldef <- 'darkolivegreen2'

# Creation of edx and validation sets. ------------------------------------

# //////////////////////////////////////////////////////////
# / Create edx set, validation set (final hold-out test set)
# //////////////////////////////////////////////////////////

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

# if using R 3.6 or earlier
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                            title = as.character(title),
#                                            genres = as.character(genres))
# if using R 4.0 or later
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
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

# Graphics of user and movie ratings --------------------------------------

g1 <- edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, fill = filldef, color = coldef ) + 
  scale_x_log10() + 
  ggtitle("Movies") +
  labs(x = 'Number of times a movie gets rated',
       y = 'Number of movies')

g2 <- edx %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, fill = filldef, color = coldef) + 
  scale_x_log10() + 
  ggtitle("Users") +
  labs(x = 'Number of times a user rates a movie',
       y = 'Number of users')  
gridExtra::grid.arrange(g1, g2, ncol = 2)


# Graphic and table of genre effect on rating ---------------------------------------

edx %>%
  group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>% filter(n >= 50000) %>%
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - (3*se), ymax = (avg + 3*se))) +
  geom_point(color = I(coldef)) +
  geom_errorbar(color = I(coldef)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = 'Genres', y = 'Rating average')

lgenres <- levels(as.factor(edx$genres))
ugenres <- str_split(lgenres, '\\|') %>% unlist
ugenres <- sort(unique(ugenres))

#Function to count the genres
numgenders <- function(x){
  ng <-  edx %>%
    filter(str_detect(genres, x)) %>%
    summarise(mean = mean(rating),
              n = n())
  return(c(ng$mean,ng$n))
  
}

#Takes a while, creates the matrix with the genre names, mean rating and n.
mxugenres <- sapply(ugenres, numgenders)
mxugenres <- t(mxugenres)

bdugenres <- data.frame(rating = round(mxugenres[,1],2),
                        movies = format(mxugenres[,2],nsmall = 0, big.mark = ','))

#Features of interest for the .rmd file
genreMaxMovies <- rownames(mxugenres)[which.max(mxugenres[,2])]
genreMinMovies <- rownames(mxugenres)[which.min(mxugenres[-1,2])+1]
genreMaxRating <- rownames(mxugenres)[which.max(mxugenres[,1])]
genreMinRating <- rownames(mxugenres)[which.min(mxugenres[-1,1])+1]

numMaxMovies <- bdugenres$movies[which(row.names(bdugenres) == genreMaxMovies)]
numMinMovies <- bdugenres$movies[which(row.names(bdugenres) == genreMinMovies)]
numMaxRating <- bdugenres$rating[which(row.names(bdugenres) == genreMaxRating)]
numMinRating <- bdugenres$rating[which(row.names(bdugenres) == genreMinRating)]


# Graphics of the ratings-per-year effect ---------------------------------

#As numeric variable
edx %>%
  mutate(year = as.numeric(str_sub(title, start = str_length(title)-4, end = str_length(title)-1 ))) %>%
  group_by(movieId) %>%
  summarize(n = n(),
            years = 2018 - first(year),
            myrating = mean(rating)) %>%
  mutate(yrate = round(n/years)) %>%
  ggplot(aes(yrate, myrating)) +
  geom_point(color = coldef) +
  geom_smooth(fill= filldef) +
  labs(x = 'Ratings per year since the movie premiered',
       y = 'Rating')

# As categorical variable
edx %>%
  mutate(year = as.numeric(str_sub(title, start = str_length(title)-4, end = str_length(title)-1 ))) %>%
  group_by(movieId) %>%
  summarize(n = n(),
            years = 2018 - first(year),
            myrating = mean(rating)) %>%
  mutate(yrate = round(n/years)) %>%
  mutate(yrate = case_when(yrate <= 50 ~ 50,
                           between(yrate, 51,100) ~ 100,
                           between(yrate, 101, 200) ~ 200,
                           between(yrate, 201, 300) ~ 300,
                           between(yrate, 301, 400) ~ 400,
                           between(yrate, 401, 500) ~ 500,
                           between(yrate, 501, 600) ~ 600,
                           between(yrate, 601, 700) ~ 700,
                           between(yrate, 701, 800) ~ 800,
                           between(yrate, 801, 900) ~ 900,
                           between(yrate, 901, 1000) ~ 1000,
                           between(yrate, 1001, 1100) ~ 1100,
                           between(yrate, 1101, 1200) ~ 1200,
                           between(yrate, 1201, 1300) ~ 1300,
                           between(yrate, 1301, 1400) ~ 1400,
                           yrate >= 1401 ~ 1500
  )) %>%
  ggplot(aes(x = as.factor(yrate), y = myrating)) +
  geom_boxplot(fill = filldef, color = coldef) +
  labs(x = 'Ratings per year since the movie premiered',
       y = 'Rating')


# Partition of edx dataset ------------------------------------------------


set.seed(395914)
test_index <- createDataPartition(y = edx$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")
lentrain <- format(dim(train_set)[1], nsmall = 0, big.mark = ',' )
lentest <- format(dim(test_set)[1], nsmall = 0, big.mark = ',' )
#allow for memory space
rm(edx)

# RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Model with the user and movie effects -----------------------------------

mu <- mean(train_set$rating)
movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarise(b_i = mean(rating - mu))

user_avgs <- train_set %>%
  left_join(movie_avgs, by = 'movieId') %>%
  group_by(userId)%>%
  summarise(b_u = mean(rating - mu - b_i))


modelui <- test_set %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  select(pred) %>%
  pull

rmse_ui <- RMSE(test_set$rating, modelui)

# Model with the user and movie effects, with regularization. -------------

#Calculation of lambda by cross-validation for the model of user and movie effects with regularization

lambdas_ui <- seq(4.75, 4.85, 0.02)

rmses_ui <- sapply(lambdas_ui, function(l){
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <-
    test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

lambda_ui <- lambdas_ui[which.min(rmses_ui)]

#Model with the user and movie effects,with regularization
pb_i <- train_set %>%
  group_by(movieId) %>%
  summarise(pb_i = sum(rating - mu)/(lambda_ui + n()))
pb_u <- train_set %>% 
  left_join(pb_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(pb_u = sum(rating - pb_i - mu)/(lambda_ui + n()))

pen_thirdmodel <- test_set %>%
  left_join(pb_i, by = 'movieId') %>%
  left_join(pb_u, by = 'userId') %>%
  mutate(pred = mu + pb_i + pb_u)

#RMSE of model_ui_pen:
rmse_ui_pen <- RMSE(pen_thirdmodel$pred, test_set$rating) # 0.8658014

# Model with the genre, without regularization. ---------------------------

# Create an object with the genres
lgenres2 <- levels(as.factor(train_set$genres))
ugenres2 <- str_split(lgenres2, '\\|') %>% unlist
ugenres2 <- sort(unique(ugenres2))
ugenres2 = ugenres2[-1]

#Function to calculate the coefficients of the movie genres.
fbetas_k <- function(x){
  train_set %>%
    filter(str_detect(genres, x)) %>%
    left_join(movie_avgs, by = 'movieId') %>%
    left_join(user_avgs, by = 'userId') %>%
    summarise(beta_gk = mean(rating - mu - b_i - b_u)) %>%
    pull(beta_gk)
}

betas_k <- sapply(ugenres2, fbetas_k)

x_uik <- train_set %>%
  filter(genres != '(no genres listed)')%>%
  distinct(genres)%>%
  mutate(
    Action = if_else(str_detect(genres, ugenres2[1]), betas_k[1], 0),
    Adventure = if_else(str_detect(genres, ugenres2[2]), betas_k[2], 0),
    Animation = if_else(str_detect(genres, ugenres2[3]), betas_k[3], 0),
    Children = if_else(str_detect(genres, ugenres2[4]), betas_k[4], 0),
    Comedy = if_else(str_detect(genres, ugenres2[5]), betas_k[5], 0),
    Crime = if_else(str_detect(genres, ugenres2[6]), betas_k[6], 0),
    Documentary = if_else(str_detect(genres, ugenres2[7]), betas_k[7], 0),
    Drama = if_else(str_detect(genres, ugenres2[8]), betas_k[8], 0),
    Fantasy = if_else(str_detect(genres, ugenres2[9]), betas_k[9], 0),
    FilmNoir = if_else(str_detect(genres, ugenres2[10]), betas_k[10], 0),
    Horror = if_else(str_detect(genres, ugenres2[11]), betas_k[12], 0),
    IMAX = if_else(str_detect(genres, ugenres2[12]), betas_k[12], 0),
    Musical = if_else(str_detect(genres, ugenres2[13]),betas_k[13], 0),
    Mistery = if_else(str_detect(genres, ugenres2[14]), betas_k[14], 0),
    Romance = if_else(str_detect(genres, ugenres2[15]), betas_k[15], 0),
    SciFi = if_else(str_detect(genres, ugenres2[16]), betas_k[16], 0),
    Thriller = if_else(str_detect(genres, ugenres2[17]), betas_k[17], 0),
    War = if_else(str_detect(genres, ugenres2[18]), betas_k[18], 0),
    Western = if_else(str_detect(genres, ugenres2[19]), betas_k[19], 0)
  )%>%
  mutate(sumK = rowSums(.[,2:20], na.rm = TRUE),
         genres = as.factor(genres)) %>%
  select(genres, sumK)


#Model with genres
modelwgenres <- test_set %>%
  filter(genres != '(no genres listed)') %>%
  mutate(genres = as.factor(genres)) %>%
  left_join(movie_avgs, by= 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  left_join(x_uik, by = 'genres') %>%
  mutate(pred = mu + b_u + b_i + sumK) %>%
  select(pred, rating)

rmse_genres <- RMSE(modelwgenres$pred, modelwgenres$rating)

# Model with the genre, with regularization -------------------------------

#Function to calculate the coefficients with regularization
fbetas_kpen <- function(x){
  train_set %>%
    filter(str_detect(genres,x)) %>%
    left_join(movie_avgs, by = 'movieId') %>%
    left_join(user_avgs, by = 'userId') %>%
    summarise(beta_gk = sum(rating - mu - b_i - b_u)/(n()+lambda_ui)) %>%
    pull(beta_gk)
}

betas_kpen <- sapply(ugenres2, fbetas_kpen)

x_uikpen <- train_set %>%
  filter(genres != '(no genres listed)')%>%
  distinct(genres)%>%
  mutate(
    Action = if_else(str_detect(genres, ugenres2[1]), betas_kpen[1], 0),
    Adventure = if_else(str_detect(genres, ugenres2[2]), betas_kpen[2], 0),
    Animation = if_else(str_detect(genres, ugenres2[3]), betas_kpen[3], 0),
    Children = if_else(str_detect(genres, ugenres2[4]), betas_kpen[4], 0),
    Comedy = if_else(str_detect(genres, ugenres2[5]), betas_kpen[5], 0),
    Crime = if_else(str_detect(genres, ugenres2[6]), betas_kpen[6], 0),
    Documentary = if_else(str_detect(genres, ugenres2[7]), betas_kpen[7], 0),
    Drama = if_else(str_detect(genres, ugenres2[8]), betas_kpen[8], 0),
    Fantasy = if_else(str_detect(genres, ugenres2[9]), betas_kpen[9], 0),
    FilmNoir = if_else(str_detect(genres, ugenres2[10]), betas_kpen[10], 0),
    Horror = if_else(str_detect(genres, ugenres2[11]), betas_kpen[12], 0),
    IMAX = if_else(str_detect(genres, ugenres2[12]), betas_kpen[12], 0),
    Musical = if_else(str_detect(genres, ugenres2[13]),betas_kpen[13], 0),
    Mistery = if_else(str_detect(genres, ugenres2[14]), betas_kpen[14], 0),
    Romance = if_else(str_detect(genres, ugenres2[15]), betas_kpen[15], 0),
    SciFi = if_else(str_detect(genres, ugenres2[16]), betas_kpen[16], 0),
    Thriller = if_else(str_detect(genres, ugenres2[17]), betas_kpen[17], 0),
    War = if_else(str_detect(genres, ugenres2[18]), betas_kpen[18], 0),
    Western = if_else(str_detect(genres, ugenres2[19]), betas_kpen[19], 0)
  )%>%
  mutate(sumK = rowSums(.[,2:20], na.rm = TRUE),
         genres = as.factor(genres)) %>%
  select(genres, sumK)


#Model with genres
modelwgenres_pen <- test_set %>%
  filter(genres != '(no genres listed)') %>%
  mutate(genres = as.factor(genres)) %>%
  left_join(movie_avgs, by= 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  left_join(x_uikpen, by = 'genres') %>%
  mutate(pred = mu + b_u + b_i + sumK) %>%
  select(pred, rating)

rmse_genres_pen <- RMSE(modelwgenres_pen$pred, modelwgenres_pen$rating)

# Model with the number of ratings per year, without regularization --------

#Get the year of the premier from the title of the movie and create the yrate variable
year_rate1 <- train_set %>%
  mutate(year = as.numeric(str_sub(title, start = str_length(title)-4, end = str_length(title)-1 ))) %>%
  group_by(movieId) %>%
  summarize(n = n(),
            years = 2018 - first(year),
            myrating = mean(rating)) %>%
  mutate(yrate = round(n/years))

#Transform yrate to a categorical variable
year_rate <-
  year_rate1 %>%
  mutate(yrate = case_when(yrate <= 50 ~ 50,
                           between(yrate, 51,100) ~ 100,
                           between(yrate, 101, 200) ~ 200,
                           between(yrate, 201, 300) ~ 300,
                           between(yrate, 301, 400) ~ 400,
                           between(yrate, 401, 500) ~ 500,
                           between(yrate, 501, 600) ~ 600,
                           between(yrate, 601, 700) ~ 700,
                           between(yrate, 701, 800) ~ 800,
                           between(yrate, 801, 900) ~ 900,
                           between(yrate, 901, 1000) ~ 1000,
                           between(yrate, 1001, 1100) ~ 1100,
                           between(yrate, 1101, 1200) ~ 1200,
                           between(yrate, 1201, 1300) ~ 1300,
                           between(yrate, 1301, 1400) ~ 1400,
                           yrate >= 1401 ~ 1500
  )) %>%
  mutate(yrate = as.factor(yrate)) %>%
  select(movieId, yrate)

#Calculate the coefficients grouping by yrate
plusyear <- train_set %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  left_join(year_rate, by = 'movieId') %>%
  group_by(yrate)%>%
  summarise(b_y = mean(rating - mu - b_i - b_u)) %>%
  right_join(year_rate, by = 'yrate') #each movie gets its year rate coefficient.

# Get de RMSE with the predictions on the test set.
model_ryr <- test_set %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  left_join(plusyear, by = 'movieId') %>%
  mutate(pred = mu + b_i + b_u + b_y)%>%
  select(pred, rating)

rmse_ryr <- RMSE(model_ryr$rating, model_ryr$pred) #0.8662424

# Model with the number of ratings per year, with regularization, ---------

# cross-validation to find the best lambda:
lambdas_ryr <- seq(4, 5, 0.2)

rmses_ryr <- sapply(lambdas_ryr, function(l){
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))

  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))

  b_y <- train_set %>%
    left_join(b_i, by = 'movieId') %>%
    left_join(b_u, by = 'userId') %>%
    left_join(year_rate, by = 'movieId') %>%
    group_by(yrate) %>%
    summarise(b_y = sum(rating - mu - b_i - b_u)/(n()+l)) %>%
    right_join(year_rate, by = 'yrate')

  predicted_ratings <-
    test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_y, by = 'movieId') %>%
    mutate(pred = mu + b_i + b_u + b_y) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

lambda_ryr <- lambdas_ryr[which.min(rmses_ryr)]

#With the best lambda, 
b_i_pen <- train_set %>%
  group_by(movieId) %>%
  summarise(b_i = sum(rating - mu)/(n()+lambda_ryr))

b_u_pen <- train_set %>% 
  left_join(b_i_pen, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda_ryr))

b_y_pen <- train_set %>%
  left_join(b_i_pen, by = 'movieId') %>%
  left_join(b_u_pen, by = 'userId') %>%
  left_join(year_rate, by = 'movieId') %>%
  group_by(yrate) %>%
  summarise(b_y = sum(rating - mu - b_i - b_u)/(n()+ lambda_ryr)) %>%
  right_join(year_rate, by = 'yrate')

#Model
model_ryr_pen <-
  test_set %>%
  left_join(b_i_pen, by = 'movieId') %>%
  left_join(b_u_pen, by = 'userId') %>%
  left_join(b_y_pen, by = 'movieId') %>%
  mutate(pred = rating + b_i + b_u + b_y) %>%
  select(pred, rating)

rmse_ryr_pen <- RMSE(model_ryr_pen$pred, model_ryr_pen$rating)

# Plots of the cross-validations ------------------------------------------

qplot(lambdas_ui, rmses_ui,
      colour = I(coldef),
      xlab = expression('Value of '~ lambda),
      ylab = 'RMSE')

qplot(lambdas_ryr, rmses_ryr,
      colour = I(coldef),
      xlab = expression('Value of '~ lambda),
      ylab = 'RMSE')

# Table with  the RMSEs of the models -------------------------------------

rmsestab <- data.frame(model = c('Normalization of movie and user effects',
                                 'Normalization of movie and user effects, with regularization',
                                 'Movie and user effects, adding the effect of genre',
                                 'Movie and user effects, adding the effect of genre with regularization',
                                 'Movie and user effects, adding the ratings-per-year effect',
                                 'Movie and user effects, adding the ratings-per-year effect with regularization'),
                       rmse = c(rmse_ui, rmse_ui_pen, rmse_genres, rmse_genres_pen, rmse_ryr, rmse_ryr_pen)) %>%
  arrange(rmse)

# Evaluation of the best model with the validation set and table w --------

load('validation.rda')
model_ryr_pen_fin <-
  validation %>%
  left_join(b_i_pen, by = 'movieId') %>%
  left_join(b_u_pen, by = 'userId') %>%
  left_join(b_y_pen, by = 'movieId') %>%
  mutate(pred = rating + b_i + b_u + b_y) %>%
  select(pred, rating) %>%
  na.omit

rmse_best <- RMSE(model_ryr_pen_fin$pred, model_ryr_pen_fin$rating)

finalres <- data.frame(model = c('Movie and user effects, adding the ratings-per-year effect with regularization'),
                       rmse = c(rmse_best))