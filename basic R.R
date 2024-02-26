# steps of data science project 
# import -> Tidy -> Transform, Visualize, Model -> Communicate

data() 
View(starwars)
library(tidyverse)


starwars %>% # pipe operator it mean "and then" (shift + crtl + m)
  filter(height > 150 & mass < 200) %>% # select data
  mutate(height_in_meters = height/100) %>% # change 
  select(height_in_meters, mass) %>% 
  arrange(mass) %>% # if (-mass) big to small
  #View() # seen in table
  plot() # plot
# table of dataset
View(msleep)

glimpse(msleep)

# 6 first row
head(msleep)

# data type variable(row) sleep_total of msleep dataset
class(msleep$sleep_total)

# number of variable(row) in msleep
length(msleep)

# number of observation(column)
length(msleep$name)

# unique data in variable vore
unique(msleep$vore)

# create new object for missing data
missing <- !complete.cases(msleep)

# indentify missing data
msleep[missing,]

# select only as mention
starwars %>%  
  select(name, height, mass)

# same as above
starwars  %>% 
  select(1:3) 

# select with option function 
starwars %>% 
 #select(ends_with("color")) 
 #select(starts_with("s"))
  select(contains("color"))

# select with everything
starwars %>%  
  select(name, height, mass, everything()) 

# changing variable name
starwars %>% 
  #rename("name" = "characters") %>% 
  head()

# change to factor 
starwars$hair_color <- as.factor(starwars$hair_color)
class(starwars$hair_color)

# change back to character(mutate: creat ,new or wirte over)
starwars %>% 
  mutate(hair_color = as.character(hair_color))
  class(starwars$hair_color)

#changing factor levels 
  df <- starwars 
  df$sex <- as.factor(df$sex)
  
  levels(df$sex)

  df <- df %>% 
    mutate(sex = factor(sex, levels = c("male", "female", "hermaphroditic", "none")))
  
  levels(df$sex)  
  
# filter rows
  starwars %>% 
    select(mass, sex) %>% 
    filter ( mass < 55 & sex == "male")

# Recode data
  starwars %>% 
    select(sex) %>% 
    mutate(sex = recode(sex, "male" = "man",
                              "female" = "women"))
 View(starwars)  

# dealing with missing data 
 mean(starwars$height, na.rm = TRUE)
 
 starwars %>% 
   drop_na(hair_color)

#Dealing with duplication
 Name <- c("Peter","John","Andrew", "Peter")
 Age <- c(22,33,44,22)

 friends <- data.frame(Name,Age) 
friends

friends %>% 
  distinct()

distinct(friends)

#Manipulate 
#######################
# Create or change a variable (mutate)
starwars %>% 
  mutate(height_m = height/100) %>% 
  select(name ,height, height_m)
 
# Conditional change (if_else)
starwars %>% 
  mutate(height_m = height/100) %>% 
  select(name, height, height, height_m) %>% 
  mutate(tallness =                         #create new variable name tallness
           if_else(height_m < 1 ,          #if_else function if heigth_m <1 than short else tall
                   "short",
                   "tall"))

# Reshape data with pivot wider 
install.packages("gapminder")
library(gapminder)
View(gapminder)

data <- select(gapminder, country, year, lifeExp) #first is dataframe, then variable)

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

# Describe
#######################
View(msleep)

# Range/ spread
min(msleep$awake)
max(msleep$awake)
range(msleep$awake)
IQR(msleep$awake)

# centrality
mean(msleep$awake)
median(msleep$awake)
sd(msleep$awake)
#variance
var(msleep$awake)

summary(msleep$awake) #all of infomation above

# summary select data
msleep %>% 
  select(awake, sleep_total) %>% 
  summary()

summary(msleep) # all data 

# Summarise your data

msleep %>% 
  drop_na(vore) %>% #drop missing value
  group_by(vore) %>% 
  summarise(Lower = min(sleep_total),
            Average = mean(sleep_total),
            Upper = max(sleep_total),
            Differene = max(sleep_total)-min(sleep_total)) %>% 
  arrange(Average) %>%  #arrage data from low-high average,(-Average) from high-low
  View()
  
# Create tables
table(msleep$vore)

msleep %>% 
  select(vore, order) %>% 
  filter(order %in% c("Rodentia", "Primates")) %>% # select "r" and "P" in order group
  table() %>% 
  prop.table(margin = 1)*100 # percentage, comment out for frequency 

# Visualize
#############################
 plot(pressure)

# THE GRAMMAR OF GRAPHICS 
  #ggplot base(overall)
   # Data which data to be use, if %>% no need to type in or else data = dataframe
   # mapping (aesthetic:aes) : which data on X and y axis,color (which data group to diff color) 
    #can put in ggplot to define in general or defin in geometry
  # geometry: type of plot: size, color of graph

# Bar plots 
  ggplot(data = starwars, 
         mapping = aes(x = gender))+   
    geom_bar(color= "yellow" ,fill = "blue")#what type of plots, outline color, fill color
  
# Histograms
  starwars %>% 
    drop_na(height) %>% 
    ggplot(aes(height))+ # first element is X 
    geom_histogram()+
    labs(title = "Histogram of x",
        x = "x")

# bogeom_smooth()# box plots
  starwars %>% 
    drop_na(height) %>% 
    ggplot(aes(height))+
    geom_boxplot(fill = "steelblue")+
    theme_bw()+
    labs(title = "Boxplot of height", #labs = label 
         x = "Height of characters")

# Density plots
  starwars %>% 
    drop_na(height) %>% 
    filter(sex %in% c("male","female") ) %>% 
    ggplot(aes(height, 
               color = sex,
               fill = sex))+
    geom_density(alpha = 0.2) + # alpha transparency of color max = 1(if not apply)
    theme_bw()
  
# Scatter plots
  starwars %>% 
    filter(mass <200) %>% 
    ggplot(aes(height, mass, color = sex))+ #1st:x , 2nd:y
    geom_point(size = 5, alpha = 0.5) +
    theme_minimal()+
    labs(title = "Heigth and mass by sex")
# Smoothed model 
  starwars %>%
    filter(mass <200 ) %>% 
    ggplot(aes(height, mass, color = sex))+
    geom_point(size=3, alpha = 0.8)+
    geom_smooth()+ #(line + sd)
    facet_wrap(~sex) + #separated by sex
    theme_bw()+
    labs(title = "Height and mass by sex")
    
# Analyze
########################################################
    
# Hypothesis testing 
# T-test compaired with 2 group
library(gapminder)
View(gapminder)
  
  #graph showed diff in mean with desity plot 
library(dplyr)
 #create mean for the line in graph
Avg_life <- gapminder %>% 
  group_by(continent) %>% 
  filter(continent %in% c("Africa", "Europe")) %>%
  summarize(mean = mean(lifeExp))

TTest_plot <-gapminder %>% 
  filter(continent %in% c("Africa", "Europe")) %>% 
  ggplot(aes(lifeExp,
             color = continent,
             fill = continent))+
  geom_density(alpha = 0.4,)+
  theme_bw()+
  labs(title = "Density plot of Life Expectency in Africa and Europe",
      x = "Life expectency")+
  geom_vline(data = Avg_life,
             aes(xintercept = mean, color = continent)
             ) # mean line in both group

TTest_plot

#t test p value (is diff real-> test with t test)
gapminder %>%
  filter(continent %in% c("Africa", "Europe") ) %>% 
  leveneTest(lifeExp ~ continent, data = .)

gapminder %>%
  filter(continent %in% c("Africa", "Europe") ) %>% 
  var.test(lifeExp ~ continent, data = .)

gapminder %>% 
  filter(continent %in% c("Africa", "Europe") ) %>% 
  t.test(lifeExp ~ continent, data = .,
         altrnative = "two.sided",
         paired = FALSE)
  
#Wilcox rank sum tes
gapminder %>% 
  filter(continent %in% c("Africa", "Europe") ) %>% 
  wilcox.test(lifeExp ~ continent, data = .,
              alternatice = "two,side")


# ANOVA paired more than 3 group 

gapminder %>% 
  filter(year == 2007) %>% 
  filter(continent %in% c("Americas", "Europe", "Asia")) %>% 
  aov(lifeExp ~ continent, data = .) %>% 
  summary()

gapminder %>% 
  filter(year == 2007) %>% 
  filter(continent %in% c("Americas", "Europe", "Asia")) %>% 
  aov(lifeExp ~ continent, data = .) %>% 
  TukeyHSD() %>% 
  plot()

# chi squared 

flowers <- iris %>% 
  mutate(Size = cut(Sepal.Length, 
                    breaks = 3,
                    labels = c("Small", "Medium","Large"))) %>% 
  select( Species, Size)
  
 # graph each size
flowers %>% 
  ggplot(aes(x=Size))+
  geom_histogram(stat = "count")

 # graph each sizw with species
flowers %>% 
  ggplot(aes(x= Size,
         color= Species,
         fill = Species))+
  geom_histogram(stat = "count")
 
  # chi squared goodness of fit test
flowers %>% 
  select(Size) %>% 
  table() %>% 
  chisq.test()

 # chi squared test of independence
flower_chi <-flowers %>% 
  table() %>% 
  chisq.test()

flower_chi
 # expected-value
flower_chi$expected
  

#linear model 

head(cars, 10)

cars %>% 
  lm(dist ~ speed, data=. ) %>% 
  summary()

#pearson 
cor.test(cars$dist, cars$speed, method = "pearson")

ibrary(ggplot2)

View(mtcars)

ggplot(starwars, aes(x = height)) + 
  geom_histogram(aes(y =..density..),
                 colour = "black") +
  stat_function(fun = dnorm, args = with(starwars, c(mean = mean(height), sd = sd(height))))

mean(starwars$height)

starwars %>% 
  drop_na(height) %>% 
  ggplot(aes(height))+
  geom_histogram(aes(y = after_stat(density))) +
  stat_function(fun = dnorm, args = with(starwars , 
                                         c(mean = mean(height, na.rm=T),
                                           sd = sd(height, na.rm = T))))

                