library(tidyverse)
library(synapser)
synLogin()
admins <- c(3324230, 3342573)

get_user_or_team_names <- function(id){
  name <- try(synGetTeam(id)$name, silent = T) ##try to get the team name from id 
  if(class(name)=='try-error'){ ##if it is not a team, will return error, so then try to get user profile
    try({
      prof <- synGetUserProfile(id = id) ##get first and last name
      fn <- prof$firstName
      ln <- prof$lastName
      if(is.null(fn) | is.null(ln)){
        un <- prof$userName
        return(un) 
      }else if(fn == "" | ln == ""){ ##if empty, get username instead
        un <- prof$userName
        return(un)
      }else{
        return(paste(fn, ln))
      }
    })
  }else{
    return(name)
  }
}

leaderboard <- readr::read_csv("main.csv") %>% 
  select(createdOn, submitterId, objectId, userId, teamId, sc3_joint_weighted_sum_rmse, sc2_joint_weighted_sum_rmse, sc1_weighted_sum_error, status) %>% 
  mutate(day = anytime::anydate(createdOn/1000)) %>% 
  filter(!submitterId %in% admins) %>% 
  filter(day != '2020-01-09') %>% 
  mutate(name = sapply(submitterId, get_user_or_team_names))
 
test <- readr::read_csv("test.csv") %>% 
  select(createdOn, submitterId, objectId, userId, teamId, status) %>% 
  mutate(day = anytime::anydate(createdOn/1000)) %>% 
  filter(!submitterId %in% admins)

ggplot(bind_rows(leaderboard,test)  %>% 
         mutate(weekday = weekdays(day)) %>% 
         group_by(weekday) %>% 
         count()) + 
  geom_bar(aes(x = ordered(weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                             "Friday", "Saturday", "Sunday")), y = n), stat = 'identity')+
  theme_minimal() +
  theme(axis.text = element_text(size = 12)) +
  labs(x = "Weekday", y = "Submission Count")

ggsave("submissions_by_day.png")


ggplot(leaderboard %>% 
         group_by(day) %>% 
         count() %>% 
         ungroup() %>% 
         mutate(cumsum = cumsum(n))) +
  geom_line(aes(x = day, y = cumsum))+
  theme_minimal() +
  theme(axis.text = element_text(size = 12)) +
  labs(x = "Month", y = "Cumulative Submission Count")

ggsave("cumulative_leaderboard_submissions.png")


ggplot(test %>% 
         group_by(day) %>% 
         count() %>% 
         ungroup() %>% 
         mutate(cumsum = cumsum(n))) +
  geom_line(aes(x = day, y = cumsum)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12)) +
  labs(x = "Month", y = "Cumulative Submission Count")

ggsave("cumulative_test_submissions.png")

ggplot(test) + 
  geom_bar(aes(x = status, fill = status))+
  theme_minimal() + 
  theme(axis.text = element_text(size = 12)) +
  labs(x = "Submission Status", y = "Submission Count")

ggsave("test_q_status.png")

ggplot(leaderboard) + 
  geom_bar(aes(x = status, fill = status)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12)) +
  labs(x = "Submission Status", y = "Submission Count")

ggsave("leaderboard_status.png")

ggplot(leaderboard %>% 
         filter(status == "ACCEPTED") %>% 
         mutate(week = lubridate::week(day)) %>% 
         group_by(week) %>% 
         summarize(median = median(sc1_weighted_sum_error), 
                   sd = sd(sc1_weighted_sum_error))) +
  geom_line(aes(x = week, y = median)) +
  geom_vline(xintercept = lubridate::week(as.Date("2020-04-21")), color = 'red') +
  theme_minimal() + 
  theme(axis.text = element_text(size = 12)) +
  labs(x = "Month", y = "SC1 Score")


ggplot(leaderboard %>% 
         filter(status == "ACCEPTED"),
       aes(x = day, y = sc1_weighted_sum_error)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(log(x))) +
  geom_vline(xintercept = as.Date("2020-04-21"), color = 'red') +
  theme_minimal() +
  theme(axis.text = element_text(size = 12)) +
  labs(x = "Month", y = "SC1 Score")

ggsave("sc1_all_performers.png")

ggplot(leaderboard %>% 
         filter(status == "ACCEPTED"),
       aes(x = day, y = sc2_joint_weighted_sum_rmse)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(log(x))) +
  geom_vline(xintercept = as.Date("2020-04-21"), color = 'red') +
  theme_minimal() +
  theme(axis.text = element_text(size = 12)) +
  labs(x = "Month", y = "SC2 Score")

ggsave("sc2_all_performers.png")

ggplot(leaderboard %>% 
         filter(status == "ACCEPTED"),
       aes(x = day, y = sc3_joint_weighted_sum_rmse)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(log(x))) +
  geom_vline(xintercept = as.Date("2020-04-21"), color = 'red') +
  theme_minimal() +
  theme(axis.text = element_text(size = 12)) +
  labs(x = "Month", y = "SC3 Score")

ggsave("sc3_all_performers.png")

top_performers <- bind_rows(
  leaderboard %>% top_n(10, -sc1_weighted_sum_error),
  leaderboard %>% top_n(10, -sc2_joint_weighted_sum_rmse),
  leaderboard %>% top_n(10, -sc3_joint_weighted_sum_rmse)
) %>% pluck('submitterId') %>% 
  unique()
#gets us a list of 5 teams total for plotting purposes, cutoff is arbitrary

ggplot(leaderboard %>% 
         filter(status == "ACCEPTED") %>% 
         filter(submitterId %in% top_performers) %>% 
         mutate(week = lubridate::week(day)) %>% 
         group_by(week, name) %>% 
         summarize(median = median(sc1_weighted_sum_error))) + 
  geom_line(aes(x = week, y = median, group = name, color = as.factor(name))) +
  # geom_vline(xintercept = lubridate::week(as.Date("2020-04-21")), color = 'red') +
  theme_minimal() +
  theme(axis.text = element_text(size = 12)) +
  labs(x = "week", y = "median SC1 score") +


ggsave("sc1_top_performers.png")

ggplot(leaderboard %>% 
         filter(status == "ACCEPTED") %>% 
         filter(submitterId %in% top_performers) %>% 
         mutate(week = lubridate::week(day)) %>% 
         group_by(week, name) %>% 
         summarize(median = median(sc2_joint_weighted_sum_rmse))) + 
  geom_line(aes(x = week, y = median, group = name, color = as.factor(name))) +
  # geom_vline(xintercept = lubridate::week(as.Date("2020-04-21")), color = 'red') +
  theme_minimal() +
  theme(axis.text = element_text(size = 12)) +
  labs(x = "week", y = "median SC2 score")

ggsave("sc2_top_performers.png")

ggplot(leaderboard %>% 
         filter(status == "ACCEPTED") %>% 
         filter(submitterId %in% top_performers) %>% 
         mutate(week = lubridate::week(day)) %>% 
         group_by(week, name) %>% 
         summarize(median = median(sc3_joint_weighted_sum_rmse))) + 
  geom_line(aes(x = week, y = median, group = name, color = as.factor(name))) +
  # geom_vline(xintercept = lubridate::week(as.Date("2020-04-21")), color = 'red') +
  theme_minimal() +  
  theme(axis.text = element_text(size = 12)) +
  labs(x = "week", y = "median SC3 score")

ggsave("sc3_top_performers.png")


leaderboard %>% 
         filter(status == "ACCEPTED") %>% 
         mutate(data_time = case_when(day < as.Date("2020-04-21") ~ "before", 
                                      day >= as.Date("2020-04-21") ~ "after")) %>% 
         group_by(submitterId, data_time) %>% 
         top_n(1, -sc1_weighted_sum_error) %>% 
  ggplot() +
  geom_point(aes(x = ordered(data_time, levels = c("before", "after")),  y = sc1_weighted_sum_error, group = submitterId, color = as_factor(name))) +
  geom_line(aes(x = data_time, y = sc1_weighted_sum_error, group = submitterId, color = as_factor(name))) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12)) +
  labs(x = "best performance relative to updated datasets", y = "SC1 Score")

ggsave("sc1_best_performance_before_and_after_data_update.png", width = 10, height = 5)


leaderboard %>% 
  filter(status == "ACCEPTED") %>% 
  mutate(data_time = case_when(day < as.Date("2020-04-21") ~ "before", 
                               day >= as.Date("2020-04-21") ~ "after")) %>% 
  group_by(submitterId, data_time) %>% 
  top_n(1, -sc2_joint_weighted_sum_rmse) %>% 
  ggplot() +
  geom_point(aes(x = ordered(data_time, levels = c("before", "after")),  y = sc2_joint_weighted_sum_rmse, group = submitterId, color = as_factor(name))) +
  geom_line(aes(x = data_time, y = sc2_joint_weighted_sum_rmse, group = submitterId, color = as_factor(name))) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12)) +
  labs(x = "best performance relative to updated datasets", y = "SC2 Score")

ggsave("sc2_best_performance_before_and_after_data_update.png", width = 10, height = 5)

leaderboard %>% 
  filter(status == "ACCEPTED") %>% 
  mutate(data_time = case_when(day < as.Date("2020-04-21") ~ "before", 
                               day >= as.Date("2020-04-21") ~ "after")) %>% 
  group_by(submitterId, data_time) %>% 
  top_n(1, -sc3_joint_weighted_sum_rmse) %>% 
  ggplot() +
  geom_point(aes(x = ordered(data_time, levels = c("before", "after")),  y = sc3_joint_weighted_sum_rmse, group = submitterId, color = as_factor(name))) +
  geom_line(aes(x = data_time, y = sc3_joint_weighted_sum_rmse, group = submitterId, color = as_factor(name))) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12)) +
  labs(x = "best performance relative to updated datasets", y = "SC3 Score")

ggsave("sc3_best_performance_before_and_after_data_update.png", width = 10, height = 5)

