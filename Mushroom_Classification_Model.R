library(data.table)
library(tidyverse)
library(inspectdf)

dataset <- fread("mushrooms (1).csv")


dataset %>% colnames()

names(dataset) <- names(dataset) %>% str_replace_all("-","_") %>% 
                  str_replace_all("\\%","_")

dataset %>% is.null() %>% sum()

dataset %>% inspect_na()


dataset %>% glimpse()

#dataset$cap_shape %>% unique() %>% length()

dataset %>% head()

dataset$class %>% unique()

dataset %>% colnames()

dataset %>% dim()

dataset$cap_shape <- dataset$cap_shape  %>% str_replace_all("'","") 
#dataset[,1:23] <- dataset %>% select(1:23) %>% str_replace_all("\\'","") %>% as.data.frame()

#dataset[,1:23]

dataset$cap_surface <- dataset$cap_surface %>% str_replace_all("'","") %>% as.factor()
dataset$cap_color <- dataset$cap_color %>% str_replace_all("'","") %>% as.factor() 
dataset$bruises_3F <- dataset$bruises_3F %>% str_replace_all("'","") %>% as.factor() 
dataset$odor <- dataset$odor %>% str_replace_all("'","") %>%as.factor() 
dataset$gill_attachment <- dataset$gill_attachment %>% str_replace_all("'","") %>%as.factor() 
dataset$gill_spacing <- dataset$gill_spacing %>% str_replace_all("'","") %>%as.factor() 
dataset$gill_size <- dataset$gill_size %>% str_replace_all("'","") %>%as.factor()
dataset$gill_color <- dataset$gill_color %>% str_replace_all("'","") %>%as.factor() 
dataset$stalk_shape <- dataset$stalk_shape %>% str_replace_all("'","") %>% as.factor()
dataset$stalk_root <- dataset$stalk_root %>% str_replace_all("'","") %>%as.factor() 
dataset$stalk_surface_above_ring <- dataset$stalk_surface_above_ring %>% str_replace_all("'","") %>%as.factor() 
dataset$stalk_surface_below_ring <- dataset$stalk_surface_below_ring %>% str_replace_all("'","") %>%as.factor() 
dataset$stalk_color_above_ring <- dataset$stalk_color_above_ring %>% str_replace_all("'","") %>% as.factor() 
dataset$stalk_color_below_ring <- dataset$stalk_color_below_ring %>% str_replace_all("'","") %>%as.factor()
dataset$veil_type <- dataset$veil_type %>% str_replace_all("'","") %>% as.factor() 
dataset$veil_color <- dataset$veil_color %>% str_replace_all("'","") %>%as.factor()
dataset$ring_number <- dataset$ring_number %>% str_replace_all("'","") %>%as.factor()
dataset$ring_type <- dataset$ring_type %>% str_replace_all("'","") %>%as.factor() 
dataset$spore_print_color <- dataset$spore_print_color %>% str_replace_all("'","") %>% as.factor()
dataset$population <- dataset$population %>% str_replace_all("'","") %>% as.factor()
dataset$habitat <- dataset$habitat %>% str_replace_all("'","") %>% as.factor() 


dataset$class <- dataset$class %>% gsub("'p'","p",.) %>% gsub("'e'","e",.)

dataset$habitat %>% unique()

dataset %>% head()

#dataset$class %>% table() %>% prop.table()

dataset$class <- dataset$class %>% factor(levels = c('p','e'),
                         labels = c(1,0))

dataset %>% dim()

dataset %>% glimpse()


##############################-----------------MODELING------------------------######################

library(rJava)

Sys.setenv(JAVA_HOME= "C:\\Program Files\\Java\\jre1.8.0_271")
Sys.getenv("JAVA_HOME")


library(h2o)
h2o.init(nthreads = -1, max_mem_size = '2g', ip = "127.0.0.1", port = 54321)


h2o_data <- dataset %>% as.h2o()

h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]

target <- 'class'
features <- dataset %>% select(-class) %>% names()


model <- h2o.automl(
  x = features, y = target,
  training_frame = train,
  validation_frame = test,
  leaderboard_frame = test,
  stopping_metric = "AUC",
  balance_classes = T,
  nfolds = 10, seed = 123,
  max_runtime_secs = 480)


model@leaderboard %>% as.data.frame()
model@leader #Error dunno wth

model@leader %>% 
  h2o.performance(test) %>% 
  h2o.find_threshold_by_max_metric('f1') -> treshold


model@leader %>% h2o.confusionMatrix(test) #NULL WHY???????

#AUC GINI ACCURACY CALCULATOR


tn <- model@leader %>% h2o.confusionMatrix(test) %>% .[1,1] 

fp <- model@leader %>% h2o.confusionMatrix(test) %>% .[1,2]

fn <- model@leader %>% h2o.confusionMatrix(test) %>% .[2,1]

tp <- model@leader %>% h2o.confusionMatrix(test) %>% .[2,2]



precision = tp/(tp+fp)

recall = tp/(tp+fn)

F1_score = 2 * precision * recall / (precision + recall) 

F1_score



