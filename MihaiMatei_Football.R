### clear screen

cat("\f")

### Default Options 

options(scipen=999)
options(stringsAsFactors = FALSE)

load_packages <- function(package_names) {
  list.of.packages <- as.vector(package_names)
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) {
    install.packages(new.packages)
  }
  sapply(list.of.packages, require, character.only = TRUE)
}

load_packages(c('rstudioapi', 'tidyverse', 'ggthemes', 'viridis', 'plotmo', 'devtools', 'lubridate',
                'parallel', 'MASS', 'glmnet', 'broom', 'texreg', 'randomForest', 'tree', 'xgboost'))


### Set the curretn directory to the script path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


########################### DATA VIZUALISATION ###########################

data <- as.data.frame(read.csv('./data/results.csv'))
data$date <- as.Date(data$date, "%Y-%m-%d")
summary(data)
# get the year as datetime

show_matches_year <- function(data) {
  wc_years <- c(1930, 1934, 1938, seq(1950, 2018, 4))

  tmp <- data %>% mutate(year = as.numeric(format(date, "%Y"))) %>%
    group_by(year) %>%
    summarise(nb_games = length(date))  %>%
    ungroup() %>% mutate(is_wc = year %in% wc_years)

  ggplot(tmp, aes(x=year, y=nb_games, group=1)) +
    geom_line() +
    geom_point(data = tmp %>% filter(is_wc), aes(colour=is_wc)) +
    labs(x="Year", title="Number of international football matchess", y="Number of games", colour="World cup") +
    scale_x_continuous(breaks=seq(1870, 2020, 10))
}

show_matches_year(data)

show_tournaments <- function(data, top_no) {
  options(repr.plot.width=7, repr.plot.height=4)
  top_tournaments <- data %>% count(tournament) %>% top_n(top_no,n) 
  data_top <- data %>% mutate(year=floor(as.numeric(format(data$date, "%Y")) / 4) * 4,
                                         tournament=ifelse(tournament %in% top_tournaments$tournament,tournament,"Other"))

  ggplot(data_top %>% group_by(tournament) %>% count(year) %>% filter(!is.na(year) & !is.na(n)),
         aes(x=year,y=n, fill=reorder(tournament, n, sum))) + 
    geom_area(show.legend=T, color="White",size=0.5) + scale_fill_viridis(discrete=T) + 
    scale_x_continuous(limits=c(min(data_top$year),max(data_top$year))) +
    labs(y="", fill='Name of tournament') + ggtitle("Annual matches") + theme_minimal()
}
show_tournaments(data, 5)

goals_period <- function(data, startDate, endDate) {
  data_goals <- data %>% filter(date > startDate & date < endDate) %>%
                         group_by(home_team, away_team) %>%
                         summarise(avg_home_score = sum(home_score) / length(date),
                                   avg_away_score = sum(away_score) / length(date),
                                   avg_total_goals = (sum(home_score) + sum(away_score)) / length(date),
                                   total_games = length(date) ) %>% ungroup()
  
  ggplot(data_goals, aes(x=avg_total_goals)) +
     geom_point(aes(y=avg_away_score), size=0.5, colour="red") + 
     geom_point(aes(y=avg_home_score), size=0.6, colour="green") + labs(x="Total goals", y="Goals", colour='sdds')
  
}

goals_period(data, as.Date("2007-01-01"), as.Date("2019-12-31"))

########################### FEDERATION AND COUNTRY AFFILIATION ###########################

federation_files = Sys.glob("./data/national-teams/*")

national_teams = data.frame(national_team=NULL, federation=NULL, continent=NULL)
for (f in federation_files) {
  federation = basename(f)
  switch(federation,
         'UEFA'={continent <- "Europe"},
         'AFC'={continent <- "Asia"},
         'CAF'={continent <- "Africa"},
         'OFC'={continent <- "Oceania"},
         'Concacaf'={continent <- "America"},
         'Conmebol'={continent <- "America"})

  teams = read.csv(f, header=FALSE)
  # Add the fedaration for each national team
  teams <- cbind(national_team=teams,
                 federation=rep(federation, nrow(teams)),
                 continent=rep(continent, nrow(teams)))
  # Append to the national_teams dataframe
  national_teams <- rbind(national_teams, teams)
}
colnames(national_teams) <- c("national_team", "federation", "continent")
national_teams <- national_teams %>% mutate(national_team=gsub("^\\s+|\\s+$", "", national_team))

data <- data %>% inner_join(national_teams, by=c('home_team'='national_team')) %>%
                 rename(home_federation=federation, home_continent=continent)
data <- data %>% inner_join(national_teams, by=c('away_team'='national_team')) %>%
                 rename(away_federation=federation, away_continent=continent)

intercontinent_games <- function(data) {
  wc_years <- c(1930, 1934, 1938, seq(1950, 2018, 4))
  
  to_plot <- data %>% mutate(game_year=year(date), inter=(home_continent != away_continent)) %>%
                      group_by(game_year) %>%
                      summarise(nr_matches=sum(inter), perc=nr_matches / length(inter) * 100) %>%
                      ungroup() %>% mutate(is_wc = game_year %in% wc_years)

  ggplot(to_plot, aes(x=game_year, y=perc)) +
    geom_line() +
    geom_point(data = to_plot %>% filter(is_wc), aes(colour=is_wc)) +
    geom_smooth(method="loess") +
    labs(x="Year", title="% of intercontinental games", y="%", colour="World cup year?") +
    scale_x_continuous(breaks = seq(1870,2020,10))
}

intercontinent_games(data)

########################### PREPROCESSING AND FEATURE SELECTION ###########################
#https://www.kaggle.com/tadhgfitzgerald/datasets
fifa_rankings <- as.data.frame(read.csv('./data/rankings/fifa_ranking.csv'))
fifa_rankings <- fifa_rankings %>% transmute(rank=rank,
                                             team=country_full,
                                             rank_year=year(as.Date(rank_date, "%Y-%m-%d")))
fifa_rankings <- fifa_rankings %>% distinct(team, rank_year, .keep_all=TRUE)

data <- data %>% mutate(game_year=year(date))
data <- data %>% inner_join(fifa_rankings, by=c('game_year'='rank_year', 'home_team'='team')) %>% rename(home_rank=rank)
data <- data %>% inner_join(fifa_rankings, by=c('game_year'='rank_year', 'away_team'='team')) %>% rename(away_rank=rank)
data <- data %>% dplyr::select(-c('game_year'))
summary(data)
nrow(data)
head(data,1)

########################### PREPROCESSING AND FEATURE SELECTION ###########################

split_factors <- function(data, max_level) {
  while (TRUE) {
    # get all current factor columns level count
    factor_columns <- sapply(data, function(x) ifelse(is.factor(x), nlevels(x), 1))
    has_split <- FALSE
    for (column in names(factor_columns)) {
      if (factor_columns[[column]] > max_level) {
        factors <- levels(data[[column]])[1:max_level-1]
        first <- paste(column, '_1', sep='')
        rest <- paste(column, '_2', sep='')
        data[[first]] <- as.factor(ifelse(data[[column]] %in% factors, as.character(data[[column]]), 'Split'))
        data[[rest]] <- as.factor(ifelse(data[[column]] %in% factors, 'Split', as.character(data[[column]])))
        data[[column]] <- NULL
        has_split <- TRUE
      }
    }
    
    if (has_split == FALSE) return(data)
  }
}

preprocess <- function(data, no_tournaments) {
   top_tournaments <- data %>% count(tournament) %>% top_n(no_tournaments, n) 
   start_date <- min(data$date)
  
   data_set <- data %>% mutate(tournament=ifelse(tournament %in% top_tournaments$tournament,tournament,"Other")) %>%
                        transmute(target = log(home_score + away_score + 1),
                                  feature_home_continent = as.factor(home_continent),
                                  feature_away_continent = as.factor(away_continent),
                                  feature_tournament = as.factor(tournament),
                                  feature_date = date - start_date,
                                  feature_home_rank = home_rank,
                                  feature_away_rank = away_rank,
                                  feature_home_field = ifelse(country == home_team, TRUE, FALSE))
   
   
   # split the factors to address the tree limitation of 32 max factor levels as well as random forest of 52
   data_set <- split_factors(data_set, 32)

   print(apply(data_set, 2, function(x) any(is.na(x) | is.infinite(x))))
   return(data_set)
}

split_data <- function(data, test) {
  spec = c(train = 1 - test, test = test)
  N = nrow(data)
    
  set.seed(Sys.time())
  data_split = sample(cut( seq(N), N * cumsum(c(0, spec)), labels = names(spec) ))
  
  return(split(data, data_split))
}

make_dataset <- function(data) {
  
  data_matrix = model.matrix(~.+0, data)
  return (list(data=data, X=data_matrix[,2:ncol(data_matrix)], Y=data_matrix[,1]))
}


# take only games with a maximum of 8 goals after 2000
data_set <- data %>% filter((home_score + away_score) <= 8) %>%
                     filter(date > as.Date("2000-01-01")) %>%
                     preprocess(20) %>% split_data(0.1)

train_set <- make_dataset(data_set$train)
test_set <- make_dataset(data_set$test)
print(nrow(train_set$data))
print(nrow(test_set$data))

########################### UTILITY FUCTIONS ###########################
parralel_compute <- function(func, types, env_variables, env, ...) {
  noCores = detectCores(all.tests = FALSE, logical = TRUE)
  cl <- makeCluster(max(noCores - 1, 1))
  # Export variables from the global environment so that
  # they are not duplicated by clusters and the
  # models can access them later if lazy evaluation is internally used
  clusterExport(cl=cl, varlist=env_variables, envir=env)
  
  time <- system.time(msd <- t(parSapply(cl, types, FUN=func, ...)))
  
  print("Time taken to compute tasks: ")
  print(time)
  stopCluster(cl)
  return(msd)
}

rsquared <- function(true, predicted, has_intercept) {
  sse <- sum((predicted - true)^2)
  sst <- ifelse(has_intercept, sum((true - mean(true))^2), sum(true^2))
  rsq <- 1 - sse / sst
  
  return (rsq)
}

mean_squared_error <- function(true, predicted) {
  # To removed the bias of the variance of the unobserved errors
  # instead of computing the mean we should devide by n-p-1 where
  # n is the number of samples and p is the number of degree of fredom,
  # the number of predictors (features) being used
  return( mean((predicted - true) ^ 2) )
}

wait_input <- function() invisible(readline(prompt="Press [enter] to continue"))

summaries <- function(models) {
  for (name in names(models)) {
    model = models[[name]]
    print(summary(model))
    wait_input()
  }
}

plot_residuals <- function(models) {
  for (name in names(models)) {
    model = models[[name]]
    print(paste('Ploiting residual information for', name))
    plotres(model)
    wait_input()
  }
}

plot_tree_importance <- function(models) {
  for (name in names(models)) {
    model = models[[name]]
    print(paste('Ploiting tree information for', name))
    print(importance(model))
    varImpPlot(model)
    wait_input()
  }
}

model_info <- function(y_true, y_models, has_intercept) {
  data = as.data.frame(list(R2=c(), MSE=c()))
  for (name in names(y_models)) {
    y_hats = y_models[[name]]
    model_info <- cbind(R2=c(rsquared(y_true, y_hats, has_intercept)), MSE=c(mean_squared_error(y_true, y_hats)))
    data <- rbind(data, model_info)
  }
  rownames(data) <- names(y_models)
  return(data)
}

########################### LINEAR MODEL OLS, GLMNET ###########################

build_linear_model <- function(type, formula, kfolds) {
  library(glmnet)

  switch(type,
         'lm'={
           lmodel <- lm(formula, train_set$data) # OLS library
           model <- list(lmodel,
                      step(lmodel, trace=0),
                      step(lmodel, trace=0, k=log(nrow(train_set$data))))
           names(model) <- list('lm', 'aic', 'bic')
         },
         'lasso'={
           # https://web.stanford.edu/~hastie/TALKS/enet_talk.pdf
           model = cv.glmnet(x=train_set$X, y=train_set$Y, alpha=1, # alpha=0 means lasso
                             lambda=NULL, nfolds=kfolds)
         },
         'ridge'={
           model <- cv.glmnet(x=train_set$X, y=train_set$Y, alpha=0, # alpha=0 means ridge
                             lambda=NULL, nfolds=kfolds)
         }
         )

  return(model)
}

formula_lm=target~.-1
R<-parralel_compute(build_linear_model, c('lm', 'lasso', 'ridge'),
                    env_variables=c('train_set'),
                    env=environment(),
                    formula=formula_lm,
                    kfolds=5)

LM_models <- list(
  LM=R[1,'lm'][['lm']]$lm,
  AIC=R[1,'lm'][['lm']]$aic,
  BIC=R[1,'lm'][['lm']]$bic,
  LASSO=R[1,'lasso'][['lasso']],
  RIDGE=R[1,'ridge'][['ridge']]
)

# Get the summaries off all the models
summaries(LM_models)

# Plot the residuals of all the models
plot_residuals(LM_models)

# Compute the Rsquared and MSE of all the models
LM_train_yhats <- list(LM=predict(LM_models$LM, train_set$data),
                       AIC=predict(LM_models$AIC, train_set$data),
                       BIC=predict(LM_models$BIC, train_set$data),
                       LASSO=predict(LM_models$LASSO$glmnet.fit, s=LM_models$LASSO$lambda.min, newx=train_set$X),
                       RIDGE=predict(LM_models$RIDGE$glmnet.fit, s=LM_models$RIDGE$lambda.min, newx=train_set$X))

print(model_info(train_set$data$target, LM_train_yhats, FALSE))

#screenreg(l=list(ols$lm, ols$aic, ols$bic))

# Plot the standard residuals for the liniar models (not the generalized ones)
residuals <- list(y_true=train_set$data$target,
                  Rlm=rstandard(LM_models$LM), Raic=rstandard(LM_models$AIC), Rbic=rstandard(LM_models$BIC))
ggplot(as.data.frame(residuals), aes(x=y_true)) +
  geom_point(aes(y=Rlm), size=0.5, colour="red", shape=15) + 
  geom_point(aes(y=Raic), size=0.5, colour="blue", shape=16) + 
  geom_point(aes(y=Rbic), size=0.5, colour="green", shape=17) +
  labs(x="Goal metric", y="Standardized residuals")


########################### XGBOOST, RANDOM FOREST AND TREES ###########################

build_tree_model <- function(type, kfolds, notrees) {
  library(tree)
  library(randomForest)
  library(xgboost)
  mean_squared_error <- function(true, predicted) {
    return( mean((predicted - true) ^ 2) )
  }
  
  mse <- 1e10
  switch(type,
         'tree'={
           rtree <- tree(formula_tree, train_set$data)
           cvModel <- cv.tree(rtree, K=kfolds, FUN=prune.tree)
           # https://stats.stackexchange.com/questions/6581/what-is-deviance-specifically-in-cart-rpart
           best_size <- cvModel$size[which(cvModel$dev==min(cvModel$dev))]
           model <- list(tree=rtree, best_tree=prune.tree(rtree, best=best_size))
         },
         'bagging'={
           for (ntree in notrees) {
             m <- randomForest(formula_tree, data=train_set$data,
                               mtry=ncol(train_set$data) - 1,
                               importance=TRUE, ntree=ntree)
             err <- mean_squared_error(train_set$Y, predict(m, train_set$data))
             if (err < mse) {
               mse <- err
               model <- m
             }
           }
         },
         'randomForest'={
           for (ntree in notrees) {
             m <- randomForest(formula_tree, data=train_set$data,
                               mtry=ncol(train_set$data) / 3,
                               importance=TRUE, ntree=ntree)
             err <- mean_squared_error(train_set$Y, predict(m, train_set$data))
             if (err < mse) {
               mse <- err
               model <- m
             }
           }
         },
         'xgboost'={
           for (ntree in notrees) {
             m <- xgboost(data=train_set$X, label=train_set$Y, max_depth = 5, eta = 1,
                          nthread = 2, nrounds = 2, objective = "reg:squarederror")
             err <- mean_squared_error(train_set$Y, predict(m, train_set$X))
             if (err < mse) {
               mse <- err
               model <- m
             }
           }
         }
  )
  
  return(model)
}


formula_tree <- target~.-1
TR<-parralel_compute(build_tree_model, c('tree', 'bagging', 'randomForest', 'xgboost'),
                     env_variables=c('train_set', 'formula_tree'),
                     env=environment(),
                     kfolds=10, notrees=c(100, 200))

TREE_models <- list(TREE=TR[1, 'tree'][['tree']]$best_tree,
                    BAGGING=TR[1, 'bagging'][['bagging']],
                    RANDOMFOREST=TR[1, 'randomForest'][['randomForest']],
                    XGBOOST=TR[1, 'xgboost'][['xgboost']])

# Get the summaries off all the models
summaries(TREE_models)

# Plot the residuals of all the models
plot_residuals(TREE_models[1:3])

plot_tree_importance(TREE_models[2:3])

# Compute the Rsquared and MSE of all the models
TREE_train_yhats <- list(TREE=predict(TREE_models$TREE, train_set$data),
                         BAGGING=predict(TREE_models$BAGGING, train_set$data),
                         RANDOMFOREST=predict(TREE_models$RANDOMFOREST, train_set$data),
                         XGBOOST=predict(TREE_models$XGBOOST, train_set$X))

print(model_info(train_set$data$target, TREE_train_yhats, FALSE))

########################### TEST RESULTS ###########################

test_yhats <- list(LM=predict(LM_models$LM, test_set$data),
                   AIC=predict(LM_models$AIC, test_set$data),
                   BIC=predict(LM_models$BIC, test_set$data),
                   LASSO=predict(LM_models$LASSO$glmnet.fit, s=LM_models$LASSO$lambda.min, newx=test_set$X),
                   RIDGE=predict(LM_models$RIDGE$glmnet.fit, s=LM_models$RIDGE$lambda.min, newx=test_set$X),
                   TREE=predict(TREE_models$TREE, test_set$data),
                   BAGGING=predict(TREE_models$BAGGING, test_set$data),
                   RANDOMFOREST=predict(TREE_models$RANDOMFOREST, test_set$data),
                   XGBOOST=predict(TREE_models$XGBOOST, test_set$X))

print(model_info(test_set$data$target, test_yhats, FALSE))

#confidence_interval_95 = predict(Oil_Model, x,  interval = c("confidence"), level = 0.95)
#prediction_interval_95 = predict(Oil_Model, x,  interval = c("prediction"), level = 0.95)
