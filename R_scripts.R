# Name: Kwan Srisilpa
# Title: Introduction to R
# Course: Poli 281(not need)
# Created: 14-Feb-2024
# Updated: 23-Feb-2024
# Email:Kwansrimd@gmail.com


  ### read me ###
## use with Rstudio
## to use code in script press "ctrl+enter"
## select by have cursor on a row of code or higthlight all needed code
# to select specific row in dataframe use "$" to specific 


# Preparation ########################################################

# clear workspace of any objects, data, or functions
rm(list = ls()) 

# get working directory(folder of currect setting)
getwd()

# set working directory(somewhere for R file)change if new project somewhere else
setwd("D:/R")

## install and load packages
#load base packages
library(datasets)
pacman::p_load(pacman, rio)
# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, psych, rio, tidyverse, dplyr) 

# LOAD DATA ########################################################

## import data 
# data with *.csv (comma separate variable)format
# have file.csv in the working directory(in setwd "...")
# import by assign to an object this one name"name_data_yourself"
df <- read.csv("file.csv", header = TRUE)
View(df) #check is data correct 

# WORK WITH DATA ########################################################

### clean data ###

#checking of data 
srt(df)
View(df)

#missing data
missing <- !complete.cases(df)# create new object for missing data

df[missing,]# indentify missing data

#Dealing with duplication
df %>% 
  distinct()

# changing variable name
df %>% 
  rename("new_var" = "var1") %>% 
  head()

# change to factor 
df$var1 <- as.factor(df$var1)
class(df$var1)

# change back to character(mutate: create ,new or write over)
df %>% 
  mutate(var1 = as.character(var1))
class(df$var1)

#changing factor levels 

levels(df$var1) #check levels

df <- df %>% 
  mutate(var1 = factor(var1, levels = c("male", "female", "hermaphroditic", "none")))

levels(df$var1) #check levels after change

# Recode data
df %>% 
  select(var1) %>% 
  mutate(var1 = recode(var1, "male" = "man",
                      "female" = "women"))
View(starwars) 

### Manipulate ###

# Create or change a variable (mutate)
df %>% 
  mutate(height_m = height/100) %>% #create new var with new data for old var
  select(name ,height, height_m) # compare between 2 data 

# Conditional change (if_else)
df %>% 
  mutate(height_m = height/100) %>% 
  select(name, height, height, height_m) %>% 
  mutate(tallness =                #create new variable name tallness
           if_else(height_m < 1 ,  #if_else function 
                   "short",
                   "tall"))       # else

# Reshape data with pivot wider 
data<- df %>% 
  select(country, year, lifeExp) #first is dataframe, then variable)

View(data)

wide_data <- data %>% 
  pivot_wider(names_from = year, values_from = lifeExp)

View(wide_data) 
?pivot_wider

# Reshape data with Pivot longer
long_data <- wide_data %>% 
  pivot_longer(2:13,
               names_to = "year",
               values_to = "lifeExp")
View(long_data)     

### describe 
## categorical
# unique data in variable vore
unique(df$var1)

# table 
table(df$var1)

df %>% 
  select(var1, var2) %>% 
  filter(var1 %in% c("male", "female")) %>% # select "m" and "f" in var1 group
  table() %>% 
  prop.table(margin = 1)*100 # for percentage margin 1 = row, 2= column
  # comment out before pipe operator and prop.table lint to get number count  

## continuous 
#distribution check
#histogram 1
df %>% 
  drop_na(var1) %>% 
  ggplot(aes(var1))+ # first element is X 
  geom_histogram()+
  lab(title = "Histogram of var1",
      x = "var1")
#histogram 2
hist(df$var1,
     main = paste("Histogram of","var1"),
     xlab = "var1 ")
#shapiro test
df %>% 
  {shapiro.test(.var1)}

# Range checking - min-max, outliers, bimodal data, nomral range(~2SD)
summary(df)

# Range/ spread
min(df$var1)
max(dfp$var1)
range(df$var1)
IQR(df$var1)

# centrality
mean(df$var1)
median(df$var1)

#variance
var(msleep$awake)
sd(msleep$awake)

#select only wanted variable
df %>% 
  select(var2, var1) %>% 
  summary()

# Summarise your data

df %>% 
  drop_na(var1) %>% #drop missing value
  group_by(var1) %>% 
  summarise(Lower = min(var2),
            Average = mean(var2),
            Upper = max(var2),
            Differene = max(var2)-min(var2)) %>% 
  arrange(Average) %>%  #arrage data from low-high average,(-Average) from high-low
  View()

#plotting look at script "basic R"
### Analysis
# Hypothesis testing 
## categorical ~ categorical

#prepared data
group_size <- df %>% 
  mutate(Size = cut(var1,   # create var1 to categorical
                    breaks = 3,
                    labels = c("Small", "Medium","Large"))) %>% 
  select( var2 , Size) %>% # var2 is categorical , Size is categorical 

# Assumption check by expected (if pass use chi squared)
expected_test<-group_size %>% 
  table() %>% 
  chisq.test()

expected_test$expected
# chi squared #

# chi squared goodness of fit test
group_size %>% 
  select(Size) %>% 
  table() %>% 
  chisq.test()

# chi squared test of independence
group_size %>% 
  table() %>% 
  chisq.test()

# Exact test
group_size %>% 
  table() %>% 
  fisher.test()
## categorical ~ continues (independent)
# Assumption check (if pass t-test)
#t test p value
df %>% 
  filter(var1 %in% c("Africa", "Europe") ) %>% 
  t.test(var2 ~ var1, data = .,
         altrnative = "two.sided",
         paired = FALSE)

# Man-whitney/rank sum test

# one way ANOVA paired more than 2 categorical

df %>% 
  filter(year == 2007) %>%  # DF with time can filter spicific time
  filter(var1 %in% c("Americas", "Europe", "Asia")) %>% 
  aov(var2 ~ var1, data = .) %>% 
  summary()

# ANOVA with post hoc (compare with each of them )
df %>% 
  filter(year == 2007) %>% 
  filter(var1 %in% c("Americas", "Europe", "Asia")) %>% 
  aov(var2 ~ var1, data = .) %>% 
  TukeyHSD() %>% 
  plot()



## continues  ~ continues 
# Pearson correlation 
# Spearman correlation

#linear model 

df %>% 
  lm(var1 ~ var2 , data=. ) %>% 
  summary()


# CLEAN UP ########################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)  # Remove all add-ons
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)
