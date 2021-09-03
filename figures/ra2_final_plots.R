library(reticulate)
reticulate::use_condaenv('synapse')
library(ggplot2)
library(challengerutils)
challengerutils::syn_login()

tab <- challengerutils::evaluation_queue_query('select * from evaluation_9614499') 

tab_valid <- filter(tab, status == "ACCEPTED") %>% 
  mutate(names = sapply(submitterId, get_submitter_name)) %>% 
  mutate(short_names = stringr::str_trunc(names, 15)) %>% 
  mutate(day = anytime::anydate(as.numeric(createdOn)/1000))
  
ggplot(tab_valid) +
  geom_bar(aes(x = day)) +
  theme_bw() 

ggsave('valid_submission_by_day_final_round.png')

ggplot(tab_valid) +
geom_bar(aes(x = forcats::fct_reorder(objectId, sc1_weighted_sum_error), 
               y = as.numeric(sc1_weighted_sum_error)), 
           stat = "identity", 
           fill = "lightgrey") +
  geom_text(aes(x = forcats::fct_reorder(objectId, sc1_weighted_sum_error), 
                y = as.numeric(sc1_weighted_sum_error)/2, 
                label = short_names), 
            stat = "identity",
            angle = 90,
            color = "blue") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Submission ID", y = "SC1 Score (Joint Weighted Sum RMSE)")

ggsave('sc1_final_round.png')

ggplot(tab_valid %>% filter(sc1_weighted_sum_error < 2)) +
  geom_bar(aes(x = forcats::fct_reorder(objectId, sc1_weighted_sum_error), 
               y = as.numeric(sc1_weighted_sum_error)), 
           stat = "identity", 
           fill = "lightgrey") +
  geom_text(aes(x = forcats::fct_reorder(objectId, sc1_weighted_sum_error), 
                y = as.numeric(sc1_weighted_sum_error)/2, 
                label = short_names), 
            stat = "identity",
            angle = 90,
            color = "blue") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Submission ID", y = "SC1 Score (Weighted Sum Error)")

ggsave('sc1_filt_final_round.png')

ggplot(tab_valid) +
  geom_bar(aes(x = forcats::fct_reorder(objectId, sc2_joint_weighted_sum_rmse), 
               y = as.numeric(sc2_joint_weighted_sum_rmse)), 
           stat = "identity", 
           fill = "lightgrey") +
  geom_text(aes(x = forcats::fct_reorder(objectId, sc2_joint_weighted_sum_rmse), 
                y = as.numeric(sc2_joint_weighted_sum_rmse)/2, 
                label = short_names), 
            stat = "identity",
            angle = 90,
            color = "blue") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Submission ID", y = "SC2 Score (Joint Weighted Sum RMSE)")

ggsave('sc2_final_round.png')
 
ggplot(tab_valid) +
  geom_bar(aes(x = forcats::fct_reorder(objectId, sc3_joint_weighted_sum_rmse), 
               y = as.numeric(sc3_joint_weighted_sum_rmse)), 
           stat = "identity", 
           fill = "lightgrey") +
  geom_text(aes(x = forcats::fct_reorder(objectId, sc3_joint_weighted_sum_rmse), 
                y = as.numeric(sc3_joint_weighted_sum_rmse)/2, 
                label = short_names), 
            stat = "identity",
            angle = 90,
            color = "blue") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = "Submission ID", y = "SC3 Score (Joint Weighted Sum RMSE)")

ggsave('sc3_final_round.png')
