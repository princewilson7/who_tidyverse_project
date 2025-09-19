library(tidyr)
library(dplyr)
library(ggplot2)

data("who")
glimpse(who)
View(who)
dim(who)

who_tidy=who %>%
  pivot_longer(new_sp_m014:newrel_f65,
               names_to = "key",
               values_to = "cases",
               values_drop_na = TRUE)
who_tidy

# seperate the key column 
who_ti=who_tidy %>%
  separate(key ,into = c("new","type","sexage"),sep = "_") %>%
  select(-new);who_ti

whoti=who_ti %>%
  mutate(
    sex=substr(sexage,1,1),  # start and the end position
    age=substr(sexage,2,nchar(sexage))
  )%>%
  select(-sexage);whoti


## summarize :
whoti %>%
  group_by(country)%>%
  summarise(total_cases=sum(cases,na.rm = TRUE))%>%
arrange(desc(total_cases))


whoti %>%
  group_by(sex) %>%
  summarise(total_count_by_sex=sum(cases,na.rm = TRUE))


whoti%>%
  group_by(year) %>%
  summarise(total_cases=sum(cases,na.rm = TRUE)) %>%
  ggplot(aes(x=year,y=total_cases))+
  geom_line(color="blue")+
  labs(title = "Total cases across years")

whoti %>%
  group_by(sex) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE)) %>%
  ggplot(aes(x = sex, y = total_cases, fill = sex)) +
  geom_col() +
  labs(title = "Cases by Sex")


