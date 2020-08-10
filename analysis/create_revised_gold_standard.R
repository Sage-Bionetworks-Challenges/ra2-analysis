library(tidyverse)
library(openxlsx)
library(synapser)
synLogin()

original_gold_standard <- synGet("syn22254942")$path %>% 
  readr::read_csv() 

tidy_og_gold_standard <- original_gold_standard %>% 
  gather(measurement, score, -Patient_ID)

updated_values <- synGet("syn22296931")$path %>% 
  openxlsx::read.xlsx() %>% 
  select(Patient_ID, measurement, score, Re_score)

updated_gold_standard <- left_join(tidy_og_gold_standard, updated_values) %>% 
  mutate(updated_score = case_when(is.na(Re_score) ~ score,
                                   !is.na(Re_score) ~ Re_score)) 

glue::glue("number of updated values: {nrow(updated_gold_standard[updated_gold_standard$score!=updated_gold_standard$updated_score,])}")

ggplot(updated_gold_standard) +
  geom_point(aes(x=log(score),y=log(updated_score)))

updated_gold_standard <- updated_gold_standard %>% 
  select(Patient_ID, measurement, updated_score) %>% 
  spread(measurement, updated_score)

glue::glue("number of missing values: {any(is.na(updated_gold_standard))}")

write_csv(updated_gold_standard, "revised_gold_standard_final_round.csv")

synStore(File("revised_gold_standard_final_round.csv", parent = "syn20545112"))

