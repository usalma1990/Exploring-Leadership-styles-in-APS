install.packages('tidytext')
install.packages('R.utils')
install.packages('wordcloud')
install.packages("viridis")

library(tidyverse)
library(RColorBrewer)
library(tidytext)
library(R.utils)
library(wordcloud)
library(viridis)

library(car)

df20 <- read.csv(file.choose())
View(df20)


library("plyr")
count(df20,'df20$q1')
################################3

{r}
#General Impressions : Current job
#library('tidyverse')
df20_1 <- select(df20,c('q18a':'q18g'))
for (i in colnames(df20_1)){
  print(count(df20_1[i]))}


{r}
library(ggplot2)
# Most basic bar chart
ggplot(df20, aes(x = factor(q18a))) +
  geom_bar()


{r}
unique(df20$q1)


{r}
#Explicitly changing to factors
for (i in colnames(df20_1))
  df20_1[i] <- factor(df20_1[i], levels = c("1", "2", "3", "4", "5") )


{r}
head(df20_1)


{r}
q18a_wide <- df20 %>%
  group_by(q1, q18a) %>%  # grouping by these two variables
  tally() %>%  # counting the number of responses
  mutate(perc = n / sum(n) * 100) %>%
  dplyr::select(-n) %>%
  group_by(q1) %>%
  spread(q18a, perc)

View(q18a_wide)




{r}
(which(is.na(df20$q1), arr.ind=TRUE))



summary(df20)


{r}
na.strings=c("", "NA")


{r}
# replacing blanks with na 
df20[df20==" "] <- NA
View(df20)

#################################################################
{r}
#df20_1 <- select(df20,c('q17a':'q17l'))

sum(is.na(df20_1))

# counting the number of null values per column 
sapply(df20, function(x) sum(is.na(x)))

{r}
#df20_18q <- select(df20,c('q18a':'q18g')) %>% mutate(q18a_num = recode(df20$q18a, "Strongly agree" = 1, "Agree" = 2, "Neither agree nor disagree" = 3, "Disagree" = 4, "Strongly disagree" = 5))


install.packages("magrittr") 
install.packages("dplyr", dependencies = T)
library(dplyr)

# converting likert scale to numerical equivalent 
#for 17th question 
df20_mod <- df20 %>% mutate_at(c("q17a","q17b","q17c","q17d","q17e","q17f","q17g","q17h","q17i","q17j","q17k","q17l" ), funs(recode(.,'"Strongly agree" = 1;"Agree" = 2;"Neither agree nor disagree" =3;"Disagree" = 4;"Strongly disagree" = 5')))
#for 18th 
df20_mod <- df20_mod %>% mutate_at(c("q18a","q18b","q18c","q18d","q18e","q18f","q18g"), funs(recode(.,'"Strongly agree" = 1;"Agree" = 2;"Neither agree nor disagree" =3;"Disagree" = 4;"Strongly disagree" = 5')))
#for 19th qn 
df20_mod <- df20_mod %>% mutate_at(c("q19a","q19b","q19c","q19d","q19e","q19f","q19g","q19h"), funs(recode(.,'"Strongly agree" = 1;"Agree" = 2;"Neither agree nor disagree" =3;"Disagree" = 4;"Strongly disagree" = 5')))
#for qns from 21 to 22
df20_mod <- df20_mod %>% mutate_at(c("q21a","q21b","q21c","q21d","q21e","q21f","q22a","q22b","q22c","q22d"), funs(recode(.,'"Strongly agree" = 1;"Agree" = 2;"Neither agree nor disagree" =3;"Disagree" = 4;"Strongly disagree" = 5')))
#for rest of the questions 
df20_mod <- df20_mod %>% mutate_at(c("q32a","q32b","q32c","q32d","q32e","q32f","q32g","q32h","q32i","q32j","q34a","q34b","q34c","q34d","q34e","q47a","q47b","q47c","q47d","q47e","q47f","q47g","q48a","q48b","q48c","q48d","q48e","q48f"), funs(recode(.,'"Strongly agree" = 1;"Agree" = 2;"Neither agree nor disagree" =3;"Disagree" = 4;"Strongly disagree" = 5')))
df20_mod <- df20_mod %>% mutate_at(c("q23a","q23b","q23c","q23d","q23e","q23f","q23g","q23h","q23i","q23j","q23k","q23l","q23m","q23n","q23o"), funs(recode(.,'"Strongly agree" = 1;"Agree" = 2;"Neither agree nor disagree" =3;"Disagree" = 4;"Strongly disagree" = 5')))
View(df20_mod)

#######################################
# converting from likert scale to positive, negetive and neutral for all the common questions 
df20_mod <- df20_mod %>% mutate_at(c("q17a","q17b","q17c","q17d","q17e","q17j","q18b","q58","q47d","q19g","q19a","q22b","q22a","q21b","q21d","q21c","q23a","q23l","q23c","q23d","q23e","q23k","q23n","q23g","q23j","q47a","q47b","q47c","q47d","q47f","q47e","q47g","q17g","q21d","q22d"), funs(recode(.,'"Very satisfied" = 1;"Satisfied" = 2;"Neither satisfied nor dissatisfied" =3;"Dissatisfied" = 4;"Very dissatisfied" = 5')))
View(df20_mod)
#grouping data based on responses 
#df20_mod <- df20_mod %>% mutate(q17a_mod=recode(q17a,1="Positive", 2="Positive", 3="Neutral", 4="Negative" , 5 = "Negative"))



df20_mod <- df20 %>% mutate_at(c("q18a","q18b","q18c","q18d","q18e","q18f","q18g"), funs(recode(.,'"Very satisfied" = 1;"Satisfied" = 2;"Neither satisfied nor dissatisfied" =3;"Dissatisfied" = 4;"Very dissatisfied" = 5')))
#for qn 19
df20_mod <- df20_mod %>% mutate_at(c("q19a","q19b","q19c","q19d","q19e","q19f","q19g","q19h"), funs(recode(.,'"Very satisfied" = 1;"Satisfied" = 2;"Neither satisfied nor dissatisfied" =3;"Dissatisfied" = 4;"Very dissatisfied" = 5')))
#21 to 22
df20_mod <- df20_mod %>% mutate_at(c("q21a","q21b","q21c","q21d","q21e","q21f","q22a","q22b","q22c","q22d"), funs(recode(.,'"Very satisfied" = 1;"Satisfied" = 2;"Neither satisfied nor dissatisfied" =3;"Dissatisfied" = 4;"Very dissatisfied" = 5')))
#
df20_mod <- df20_mod %>% mutate_at(c("q32a","q32b","q32c","q32d","q32e","q32f","q32g","q32h","q32i","q32j","q34a","q34b","q34c","q34d","q34e","q47a","q47b","q47c","q47d","q47e","q47f","q47g","q48a","q48b","q48c","q48d","q48e","q48f"), funs(recode(.,'"Very satisfied" = 1;"Satisfied" = 2;"Neither satisfied nor dissatisfied" =3;"Dissatisfied" = 4;"Very dissatisfied" = 5')))
#
df20_mod <- df20_mod %>% mutate_at(c("q23a","q23b","q23c","q23d","q23e","q23f","q23g","q23h","q23i","q23j","q23k","q23l","q23m","q23n","q23o"), funs(recode(.,'"Very satisfied" = 1;"Satisfied" = 2;"Neither satisfied nor dissatisfied" =3;"Dissatisfied" = 4;"Very dissatisfied" = 5')))


df20_mod = df20 %>% mutate(q18a=recode(q18a,'"Strongly agree" = 1; "Agree" = 2; "Neither agree nor disagree" = 3; "Disagree" = 4; "Strongly disagree" = 5'))
View(df20_mod)
# creating inf and cat for b and f category
Inf_cat_20 <- select(df20_mod, q17a,q17b,q17c,q17d,q17e,q17j,q17k,q17l,q23a,q23b,q23c,q23d,q23e,q23f,q23g,q23h,q23i,q23j,q23k,q23l,q23m,q23n,q23o)
View(Inf_cat_20)

Des_cat_20 <- select(df20_mod, q17a_mod,q17b_mod,q17c_mod,q17d_mod,q17e_mod,q17j_mod,q17k_mod,q17l_mod,q23a_mod,q23b_mod,q23c_mod,q23d_mod,q23e_mod,q23f_mod,q23g_mod,q23h_mod,q23i_mod,q23j_mod,q23k_mod,q23l_mod,q23m_mod,q23n_mod,q23o_mod)
View(Des_cat_20)
# general impressions: current job 
#df20_mod <- df20_mod %>% mutate(q17a_mod=recode(q17a,1="Positive", 2="Positive", 3="Neutral", 4="Negative" , 5 = "Negative"))

{r}
df14_mod <- df14 %>% mutate_at(c("q18a","q18b","q18c","q18d","q18e","q18f","q18g","q18h"), funs(recode(.,'"Strongly agree" = 1;"Agree" = 2;"Neither agree nor disagree" =3;"Disagree" = 4;"Strongly disagree" = 5')))