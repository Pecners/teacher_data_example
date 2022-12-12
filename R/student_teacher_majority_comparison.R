library(tidyverse)
library(wisconsink12)
library(scales)

staff <- readRDS("../teacher_data/staff.rda")


mke <- staff %>%
  filter(school_mailing_city == "Milwaukee")

mke_teachers <- mke %>%
  filter(position_classification == "Teachers") %>%
  modify_at(c("total_salary", "total_fringe"), function(x) as.numeric(str_remove_all(x, "\\$|,"))) %>%
  modify_at("assignment_fte", as.numeric) %>%
  mutate(total_compensation = total_salary + total_fringe)

sum1 <- mke_teachers %>%
  left_join(., schools %>% select(dpi_true_id, school_year)) %>%
  select(research_id, school_year, total_salary, total_fringe, contract_total_experience,
         total_compensation, raceethnicity, assignment_fte, dpi_true_id)

per_race_teach <- sum1 %>% 
  group_by(school_year, raceethnicity) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(school_year) %>%
  mutate(perc = n / sum(n),
         group = case_when(str_detect(raceethnicity, "Indian") ~ "American Indian",
                           str_detect(raceethnicity, "Asian") ~ "Asian",
                           str_detect(raceethnicity, "Black") ~ "Black/African American",
                           str_detect(raceethnicity, "Hispanic") ~ "Hispanic/Latinx",
                           str_detect(raceethnicity, "Hawaiian") ~ "Native Hawaiian/\nOther Pacific Islander",
                           str_detect(raceethnicity, "Two") ~ "Two or More",
                           str_detect(raceethnicity, "White") ~ "White")) %>%
  select(-raceethnicity)

# Student Body

mke_schools <- make_mke_schools()


mke_enr <- left_join(mke_schools, enrollment)

mke_sub_enr <- mke_enr %>%
  filter(group_by == "Race/Ethnicity") %>%
  select(school_year,
         group = group_by_value,
         student_count)

sum_students <- mke_sub_enr %>%
  group_by(group, school_year) %>%
  summarise(total = sum(student_count, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(school_year) %>%
  mutate(per_school_total = total / sum(total),
         group = case_when(str_detect(group, "Indian") ~ "American Indian",
                           str_detect(group, "Asian") ~ "Asian",
                           str_detect(group, "Black") ~ "Black/African American",
                           str_detect(group, "Hisp") ~ "Hispanic/Latinx",
                           str_detect(group, "Hawaii") ~ "Native Hawaiian/\nOther Pacific Islander",
                           str_detect(group, "Two") ~ "Two or More",
                           str_detect(group, "White") ~ "White"))


maj_student_w_teacher <- left_join(sum_students, per_race_teach) %>%
  modify_at(c("n", "perc"), replace_na, replace = 0) %>%
  rename(percent_teachers = perc) %>%
  filter(!is.na(group)) %>%
  pivot_longer(cols = c("per_school_total", "percent_teachers"), names_to = "percent_group", values_to = "values") %>%
  filter(school_year > "2015-16") %>%
  mutate(percent_group = ifelse(percent_group == "per_school_total", "Students", "Teachers"))


maj_student_w_teacher %>%
  ggplot(aes(school_year, values, group = percent_group, color = percent_group)) +
  geom_line() +
  facet_wrap(~ group) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1)) +
  labs(y = "Percent of Citywide Total", x = "School Year",
       title = "White Teachers Make Up Vastly Disproportionate Number of Educators",
       caption = "Data only available for public schools.",
       color = "") +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title.position = "plot",
        plot.caption.position = 'plot',
        plot.caption = element_text(hjust = 0))

