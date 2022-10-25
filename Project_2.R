library(tidyverse) 
data(diamonds)
View(diamonds)

## To Summarize data
as_tibble(diamonds)

## Filter data
filter_tooth <- filter(ToothGrowth, dose == 0.5)

## Arrange data
arrange(filter_tooth,len)

## Arrange and Filter data
arrange(filter(ToothGrowth,dose==0.5),len)

View(ToothGrowth)

## Pipe to filter data 
filter_tooth_data <- ToothGrowth %>%
  filter(dose==0.5) %>%
  arrange(len) 

View(filter_tooth_data)

## Filter using groupby and summarize
library(dplyr)
filtered_toothgrowth <- ToothGrowth %>%
  filter(dose==0.5) %>%
  group_by(supp) %>%
  summarize(mean_len = mean(len,na.rm = T),new_group="drop")

View(filtered_toothgrowth)

## Show summary of Data
head(diamonds)
str(diamonds)
colnames(diamonds)

## To add colums
mutate(diamonds, carat_2 = carat*100)

## For data cleanning
install.packages("here")
install.packages("skimr")
install.packages("janitor")

install.packages("dplyr")

## Summary of data
library("dplyr")
library("palmerpenguins")
skimr::skim_without_charts(penguins)
glimpse(penguins)

## To select specific colums
penguins %>%
  select(species,island)

## To ignore colums
penguins %>% 
  select(-species)

## To change name of column
penguins %>%
  rename(island_new=island)

## To uppercase
rename_with(penguins,toupper)

## To Lowercase
rename_with(penguins,tolower)

## To clean labels
library(janitor)
clean_names(penguins)

## Arranging data assending
penguins %>%
  arrange(bill_length_mm)

## Arranging data decending
penguins %>%
  arrange(-bill_length_mm)

## Making dataframes
penguins_2 <- penguins %>%
  arrange(-bill_length_mm)
View(penguins_2)

## summarizing island with mean lenght
library("tibble")
library("dplyr")
library("tidyr")
penguins %>%
  group_by(island) %>%
  drop_na() %>%
  summarize(mean_lenght_mm = mean(bill_length_mm))

## To groupby species, island
library("tibble")
library("dplyr")
library("tidyr")
penguins %>% 
  group_by(species,island) %>% 
  drop_na() %>% 
  summarize(max_bl = max(bill_length_mm), mean_bl = mean(bill_length_mm))

## Creating data frames
id <- c(1:6)
name <- c("John Mendes", "Rob Stewart", "Rachel Abrahamson", "Christy Hickman", "Johnson Harper", "Candace benser")
job_title <- c("Professional", "Programmer", "Managesent", "Clerical","Developer", "Programmer")

employee <- data.frame(id,name,job_title)
View(employee)

## Seperate names
employee_new <- separate(employee,name,into=c("first_name","last_name"),sep=" ")
View(employee_new)

## Unite names
unite(employee_new,"Name",first_name,last_name, sep=" ")

## Mean,sd,corr of data
install.packages("Tmisc")
library(Tmisc)
data(quartet)
View(quartet)
quartet %>%
  group_by(set) %>%
  summarize(mean(x), sd(x), mean(y), sd(y), cor(x,y))

## Bias
install.packages("SimDesign")
library("SimDesign")
actual_sales = c(200,300,400,500)
predicted_sales = c(350,400,500,600)
bias(actual_sales,predicted_sales)


## Plotting
library("palmerpenguins")
library("ggplot2")
library("tidyr")
penguins_2 <- penguins %>%
  drop_na() 
View(penguins_2)
ggplot(data=penguins_2,aes(x=bill_length_mm, y=body_mass_g))+
  geom_point()

## improving Asthetics
ggplot(data=penguins_2,aes(x=bill_length_mm, y=body_mass_g, color=species))+
  geom_point()

## Plotting two plots in one
data(penguins_2)
library("ggplot2")
ggplot(data=penguins_2,aes(x=flipper_length_mm, y=body_mass_g))+
  geom_smooth(method="lm", se=FALSE)+
  geom_point()

## bar chart plotting
data(diamonds)
View(diamonds)
ggplot(data=diamonds,aes(x=cut,fill=clarity))+
  geom_bar()

## Using facet
##for peguins
ggplot(data=penguins_2,aes(x=flipper_length_mm, y=body_mass_g,color=species))+
  geom_point()+
  facet_wrap(~species)

## for diamonds
ggplot(data=diamonds,aes(x=color,fill=cut))+
  geom_bar()+
  facet_wrap(~cut)

## further facet
ggplot(data=penguins,aes(x=flipper_length_mm, y=body_mass_g,color=species))+
  geom_point()+
  facet_grid(sex~species)

## Adding Anotation
ggplot(data=penguins,aes(x=flipper_length_mm, y=body_mass_g,color=species))+
  geom_point()+
  facet_grid(sex~species)+
  labs(title="Graph represent pengiuns species with genders", subtitle = "3 color represent species", caption = "By Mujahid")+
  annotate("text",x=200, y=800, label="Mujahid Akber Ali",size=2)



