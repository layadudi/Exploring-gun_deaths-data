data = read.csv("gun_deaths.csv") #loading the data

####Generate a data frame that summarizes the number of gun deaths per month. Print the data frame as a formatted kable() table.

library(dplyr)
is.na(data) #Check for NA's
new_data = na.omit(data) #Omit data

is.na(new_data)

df = count(new_data, month) #creating dataframe for number of deaths per month

#install.packages('kableExtra')

library(kableExtra)

df1 = kable(df, format = "pandoc", digits = getOption("digits"), row.names = NA, col.names = c("Months", "Number of deaths"), 
      caption = "Number of Deaths per month")

###### Generate a bar chart with labels on the x-axis. That is, each month should be labeled "Jan","Feb", "Mar" and etc.

print(df1)

vec1 = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec")

new = df$n


barplot(height= new,
        main = "Count of deaths per month",
        xlab = "Months",
        ylab = "Number of deaths",
        names.arg = vec1,
        horiz = FALSE)

##### Generate a bar chart that identifies the number of gun deaths associated with each type of intent cause of death. The bars should be sorted from highest to lowest values.


Intent = count(new_data, intent)

tall = Intent$n

Decreasing = sort(tall, decreasing = TRUE)

barplot(height= Decreasing,
        main = "Intent of deaths",
        xlab = "Intent",
        ylab = "Number of deaths",
        names.arg = c("Suicide", "Homicide","Accidental","Undetermined"),
        horiz = FALSE)

#### Generate a boxplot visualizing the age of gun death victims, by sex. Print the average age of female gun death victims


a = boxplot(age~sex,data=new_data, main="Box plot",
            xlab="Sex", ylab="Age of gun death victims")

#Print the average age of female gun death victims.
a1 = new_data %>% filter(sex=="F")
a2 = mean(a1$age)

#### How many white males with at least a high school education were killed by guns in 2012?

b = new_data %>% filter(sex=="M" & race=="White" & year== "2012" & education == "HS/GED" | education == "BA+"| education=="Some college")
count(b)

##### Which season of the year has the most gun deaths?


f1 = new_data%>%
    mutate(
       season=case_when(
          month %in% 1:3 ~ "winter",
          month %in% 4:6 ~ "Spring",
          month %in% 7:9 ~ "Summer",
          TRUE ~ "Fall" ))
f2 = f1[,c(1,3,11)]
f3 = f2%>% count(season)

##### Are whites who are killed by guns more likely to die because of suicide or homicide? How does this compare to blacks and Hispanics? 

c = new_data%>%filter(race=="White"& intent=="Suicide")
count(c)
d = new_data%>%filter(race=="White"& intent=="Homicide")
count(d)

#whites who are killed by guns more likely to die because of suicide 

e = new_data%>%filter(race=="Black"& intent=="Homicide")
count(e)
f = new_data%>%filter(race=="Black"& intent=="Suicide")
count(f)

#Blacks who are killed by guns more likely to die because of Homicide

g = new_data%>%filter(race=="Hispanic"& intent=="Suicide")
count(g)
h = new_data%>%filter(race=="Hispanic"& intent=="Homicide")
count(h)

#Hispanic who are killed by guns more likely to die because of Homicide

##### Are police-involved gun deaths significantly different from other gun deaths? Assess the relationship between police involvement and other variables.


h1 = new_data%>%filter(police==1)
count(h1)
h2 = new_data%>%filter(police==0)
count(h2)

print(h1)
#When police were involved intent = Homicide
