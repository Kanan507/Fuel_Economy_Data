library(tidyverse) 
library(data.table)
library(rstudioapi)
library(skimr)
library(inspectdf)
library(mice)
library(plotly)
library(highcharter)
library(recipes) 
library(caret) 
library(purrr) 
library(graphics) 
library(Hmisc) 
library(glue) 

data <- ggplot2::mpg
data %>% View()

data %>% skim()

names(data) <- names(data) %>% 
  str_replace_all(" ","_") %>% 
  str_replace_all("-","_") %>% 
  str_replace_all("/","_")


df.num <- data %>%
  select_if(is.numeric)

df.chr <- data %>%
  select_if(is.character)

df.num %>% inspect_na()

df.chr %>% inspect_na()


# One Hote Encoding ----
df.chr <- dummyVars(" ~ .", data = df.chr) %>% 
  predict(newdata = df.chr) %>% 
  as.data.frame()

df <- cbind(df.chr,df.num) %>% select(year,cyl,cty,hwy,everything())
df %>% View()

names(df) <- names(df) %>% 
  str_replace_all(" ","_") %>%
  str_replace_all("-","_") %>%
  str_replace_all("\\(","") %>% 
  str_replace_all("\\)","") %>% 
  str_replace_all("\\'","")

# ----------------------------- Multicollinearity -----------------------------

target <- 'cty'
features <- df.num %>% select(-cty,-hwy) %>% names()

f <- as.formula(paste(target, paste(features, collapse = " + "), sep = " ~ "))
glm <- glm(f, data = df.num)

glm %>% summary()

coef_na <- attributes(alias(glm)$Complete)$dimnames[[1]]
features <- features[!features %in% coef_na]

f <- as.formula(paste(target, paste(features, collapse = " + "), sep = " ~ "))
glm <- glm(f, data = df.num)

glm %>% summary()




