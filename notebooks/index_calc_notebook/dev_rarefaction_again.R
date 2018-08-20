
count.df <- taxa.df %>% 
  group_by(unique_id) %>% 
  summarize(count = sum(reporting_value))


unique.vec <- c("10020_1", "10021_1", "10047_1")

count.df <- taxa.df %>% 
  filter(unique_id %in% unique.vec[3])

count.df %>% 
  group_by(unique_id) %>% 
  summarize(sum = sum(reporting_value))

test <- count.df %>% 
  select(unique_id, tsn_final, reporting_value) %>% 
  group_by(unique_id) %>% 
  mutate(total = sum(reporting_value)) %>% 
  group_by(unique_id, tsn_final) %>% 
  mutate(prob = reporting_value / total,
         estimate = prob * 100,
         restimate = ceiling(estimate)) %>% 
  group_by(unique_id) %>% 
  arrange(desc(prob)) %>% 
  mutate(cumsum = cumsum(restimate)) %>% 
  mutate(min_cumsum = min(cumsum[cumsum >= 100]),
         min_value = reporting_value[cumsum == min_cumsum],
         rand_sel = sum(reporting_value == min_value & cumsum <= 100)) %>% 
  mutate(action = case_when(
    reporting_value > min_value ~ "keep",
    reporting_value == min_value ~ "sample",
    reporting_value < min_value ~ "drop",
    TRUE ~ "ERROR"
  )) %>% 
  filter(action != "drop") %>% 
  group_by(unique_id, action) %>% 
  mutate(rand = sample(1:n(), n())) %>% 
  ungroup() %>% 
  filter(!(action == "sample" & rand > rand_sel))

test %>% 
  group_by(unique_id) %>% 
  summarize(sum = sum(restimate))

keep.df <- filter(test, action == "keep")
sample.df <- filter(test, action == "sample") %>% 
  sample_n(size = unique(.$rand_sel))

final.df <- bind_rows(keep.df, sample.df)

