# Horror Movies by David Robinson 22/10/2019
# Source (YouTube): https://www.youtube.com/watch?v=yFRSTlk3kRQ
# Tidytuesdays source (Github) : https://github.com/rfordatascience/tidytuesday 

# Interesting use of:
# extract()
# pull()
# fct_lump()
# fct_reorder()
# separate_rows()
# separate()
# distinct()
# add_count()
# cast_sparse()
# add_count()

library(tidyverse)
library(scales)
library(tidytext)
library(glmnet)
library(Matrix)
library(broom)
theme_set(theme_minimal())

# DATA --------------------------------------------------------------------
horror_movies_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

# EDA ---------------------------------------------------------------------

# General
glimpse(horror_movies_raw)

# Better ratings
horror_movies_raw %>% 
  arrange(desc(review_rating)) %>% 
  View()

# Extract year
horror_movies <- horror_movies_raw %>% 
  arrange(desc(review_rating)) %>% 
  extract(title, "year", "\\((\\d\\d\\d\\d)\\)$", remove = FALSE, convert = TRUE) %>% 
  mutate(
    budget = parse_number(budget) #removing dollar format
  )

# No year
horror_movies %>% 
  filter(is.na(year)) %>% 
  pull(title)

# Distribution 
horror_movies %>% 
  filter(year >= 2005) %>% 
  ggplot(aes(year)) +
  geom_histogram() # Most of the movies are since 2012

# Other variables
horror_movies %>% 
  count(genres, sort = TRUE)

horror_movies %>% 
  count(language, sort = TRUE)

horror_movies %>% 
  count(budget, sort = TRUE)

horror_movies %>% 
  ggplot(aes(budget)) +
  geom_histogram() +
  scale_x_log10(labels = dollar) 

# Do higher budget movies end up higher rated? -> No relationship
horror_movies %>% 
  ggplot(aes(budget, review_rating)) +
  geom_point() +
  scale_x_log10(labels = dollar) +
  geom_smooth(method = "lm")

# How about movie rating and review
horror_movies %>% 
  count(movie_rating, sort = TRUE)

horror_movies %>% 
  mutate(
    movie_rating = fct_lump(movie_rating, 5)
  )

horror_movies %>% 
  mutate(
    movie_rating = fct_lump(movie_rating, 5),
    movie_rating = fct_reorder(movie_rating, review_rating, na.rm = TRUE)
  ) %>% 
  ggplot(aes(movie_rating, review_rating)) +
  geom_boxplot() +
  coord_flip()

horror_movies %>% 
  filter(!is.na(movie_rating)) %>% 
  mutate(
    movie_rating = fct_lump(movie_rating, 5)
  ) %>% 
  lm(review_rating ~ movie_rating, data = .) %>% 
  anova() #F value an p value show it's not by chance

# Genres
horror_movies %>% 
  count(genres, sort = TRUE) #Films have several genres at the same time

horror_movies %>% 
  separate_rows(genres, sep = "\\| ") %>% #duplicates movies with several genres
  select(title, year, genres) %>% 
  count(genres, sort = TRUE)

horror_movies %>% 
  separate_rows(genres, sep = "\\| ") %>% #duplicates movies with several genres
  mutate(
    genre = fct_lump(genres, 5),
    genre = fct_reorder(genre, review_rating, na.rm = TRUE)
  ) %>% 
  ggplot(aes(genre, review_rating)) +
  geom_boxplot()


# TEXT MINING ---------------------------------------------------------------
horror_movies %>% 
  head() %>% 
  pull(plot)

horror_movies %>% 
  mutate(
    plot = str_remove(plot, "Directed.*?\\.") #Begins with Directed and ends with dot
  ) %>% 
  head() %>% 
  pull(plot)

horror_movies %>% 
  separate(plot, c("director", "cast_sentence", "plot"), 
           extra = "merge", sep = "\\. ", fill = "right") %>% 
  head() %>% 
  pull(plot)

# Tidytext
horror_movies2 <- horror_movies %>% 
  separate(plot, c("director", "cast_sentence", "plot"), 
           extra = "merge", sep = "\\. ", fill = "right") %>% 
  distinct(title, .keep_all = TRUE) # There are duplicated tittles

horror_movies_unnested <- horror_movies2 %>% 
  unnest_tokens(word, plot) %>% 
  anti_join(stop_words, by = "word") %>% 
  filter(!is.na(word))

horror_movies_unnested %>% 
  count(word, sort = TRUE)

horror_movies_unnested %>% 
  filter(!is.na(review_rating)) %>% 
  group_by(word) %>% 
  summarize(
    movies = n(),
    avg_rating = mean(review_rating)
    ) %>% 
  arrange(desc(movies)) %>% 
  filter(movies >= 100) %>% 
  mutate(
    word = fct_reorder(word, avg_rating)
  ) %>% 
  ggplot(aes(avg_rating, word)) +
  geom_point()


# MODEL: LASSO REGRESSION -------------------------------------------------

# Lasso regression for predicting review rating based on words in plot

# Duplicated tittles?
horror_movies2 %>% 
  count(title, sort = TRUE) # Yes, corrected in previous steps

movie_word_matrix <- horror_movies_unnested %>% 
  filter(!is.na(review_rating)) %>% 
  add_count(word) %>% 
  filter(n >= 20) %>% 
  count(title, word) %>% 
  cast_sparse(title, word, n)

dim(movie_word_matrix)

rownames(movie_word_matrix)

rating <- horror_movies2$review_rating[match(rownames(movie_word_matrix), 
                                        horror_movies2$title)]
qplot(rating)

# glmnet
lasso_model <- cv.glmnet(movie_word_matrix, rating)

lasso_model$glmnet.fit
tidy(lasso_model$glmnet.fit) 


# Lambda (penalty parameter)
lasso_model$glmnet.fit
tidy(lasso_model$glmnet.fit) %>% 
  filter(term %in% c("friends", "evil", "college", "haunted", "mother")) %>% 
  ggplot(aes(lambda, estimate, color = term)) + 
  geom_line() +
  scale_x_log10() +
  geom_hline(yintercept = 0, lty = 2)

plot(lasso_model)

lasso_model$lambda.min

tidy(lasso_model$glmnet.fit) %>% 
  filter(lambda == lasso_model$lambda.min,
         term != "(Intercept)") %>% 
  mutate(term = fct_reorder(term, estimate)) %>% 
  ggplot(aes(term, estimate)) +
  geom_col() +
  coord_flip()

lasso_model$glmnet.fit
tidy(lasso_model$glmnet.fit) %>% 
  filter(term %in% c("quickly", "seek", "army", "teacher", "unexpected",
                     "friends", "evil")) %>% 
  ggplot(aes(lambda, estimate, color = term)) + 
  geom_line() +
  scale_x_log10() +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = lasso_model$lambda.min)

# Throwing everything into a linear model: director, cast, genre, rating, plot words

horror_movies2 %>% 
  select(title, genres, director, cast) %>% 
  mutate(director = str_remove(director, "Directed by ")) %>% 
  count(director, sort = TRUE)


horror_movies2 %>% 
  select(title, genres, director, cast) %>% 
  mutate(director = str_remove(director, "Directed by ")) %>%
  gather(type, value, -title) 

horror_movies2 %>% 
  select(title, genres, director, cast) %>% 
  mutate(director = str_remove(director, "Directed by ")) %>%
  gather(type, value, -title) %>% 
  separate_rows(value, sep = "\\| ?") %>%  # ? means space is optional
  count(type, value, sort = TRUE) %>% 
  View()

features <- horror_movies2 %>% 
  filter(!is.na(review_rating)) %>% 
  select(title, genres, director, cast, movie_rating, language, release_country) %>% 
  mutate(director = str_remove(director, "Directed by ")) %>%
  gather(type, value, -title) %>% 
  filter(!is.na(value)) %>% 
  separate_rows(value, sep = "\\| ?") %>%  # ? means space is optional
  unite(feature, type, value, sep = ": ") %>% 
  mutate(n = 1)

movie_feature_matrix <- horror_movies_unnested %>% 
  filter(!is.na(review_rating)) %>% 
  count(title, feature = str_c("word: ", word)) %>% 
  bind_rows(features) %>% 
  add_count(feature) %>% 
  filter(n >= 10) %>% 
  cast_sparse(title, feature)

dim(movie_feature_matrix)

features
features %>% filter(str_detect(feature, "movie"))

rating <- horror_movies$review_rating[match(rownames(movie_feature_matrix), horror_movies$title)]

feature_lasso_model <- cv.glmnet(movie_feature_matrix, rating)

plot(feature_lasso_model)

tidy(feature_lasso_model$glmnet.fit) %>%
  filter(lambda == feature_lasso_model$lambda.1se,
         term != "(Intercept)") %>%
  mutate(term = fct_reorder(term, estimate)) %>%
  ggplot(aes(term, estimate)) +
  geom_col() +
  coord_flip() +
  labs(x = "",
       y = "Coefficient for predicting movie rating",
       title = "What affects a horror movie rating?",
       subtitle = "Based on a lasso regression to predict IMDb ratings of ~3000 movies")


horror_movies2 %>%
  filter(str_detect(genres, "Comedy"),
         !is.na(movie_rating),
         !is.na(budget),
         movie_rating != "PG") %>%
  arrange(desc(review_rating)) %>%
  select(title, review_rating, movie_rating, plot, director, budget, language) %>%
  View()
