library(tidyverse)
library(wisconsink12)

staff <- readRDS("staff.rda")

mke <- staff %>%
  filter(school_mailing_city == "Milwaukee")

mke_teachers <- mke %>%
  filter(position_classification == "Teachers") %>%
  modify_at(c("total_salary", "total_fringe"), function(x) as.numeric(str_remove_all(x, "\\$|,"))) %>%
  modify_at("assignment_fte", as.numeric) %>%
  mutate(total_compensation = total_salary + total_fringe)

sum1 <- mke_teachers %>%
  left_join(., schools %>% select(dpi_true_id, school_year, accurate_agency_type)) %>%
  select(research_id, school_year, total_salary, total_fringe, contract_total_experience,
         total_compensation, raceethnicity, accurate_agency_type, assignment_fte, dpi_true_id) %>%
  mutate(accurate_agency_type = ifelse(is.na(accurate_agency_type), "Traditional Public", accurate_agency_type))

per_w_salaries <- sum1 %>% 
  group_by(school_year, dpi_true_id) %>% 
  summarise(no_without_salary = sum(is.na(assignment_fte)), 
            no_with_salary = sum(!is.na(assignment_fte))) %>% 
  ungroup() %>% 
  mutate(perc_with_salary_data = no_with_salary / (no_with_salary + no_without_salary))

summary_data <- sum1 %>% 
  right_join(., per_w_salaries %>% filter(perc_with_salary_data > .9)) %>%
  group_by(school_year, dpi_true_id, accurate_agency_type) %>% 
  summarise(total_fte = sum(assignment_fte)) %>% 
  ungroup() %>% 
  left_join(., enrollment %>%
              filter(group_by == "All Students")) %>%
  mutate(ratio = student_count / total_fte) %>%
  filter(school_year == "2020-21")

outlier <- quantile(summary_data$ratio, na.rm = TRUE)[4] + IQR(summary_data$ratio, na.rm = TRUE) * 1.5
no_removed <- summary_data %>%
  filter(school_year == "2020-21" & ratio > round(outlier, 1))

summary_data %>%
  filter(school_year == "2020-21") %>%
  ggplot(aes(accurate_agency_type, ratio)) +
  ggbeeswarm::geom_quasirandom(width = .25, alpha = 0.5) +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  geom_hline(color = "red", linetype = 3, yintercept = 20) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0)) +
  labs(x = "Agency Type", y = "Student:Teacher FTE Ratio",
       title = "Student-Teacher Ratios On Average Below 20:1 (shown as red line)",
       subtitle = "2020-21 School Year",
       caption = "Agencies not presented here did not have available data.")
