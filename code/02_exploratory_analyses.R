data_summary_line <- structurelinePSL %>%
  select(site, line) %>% 
  unique() %>% 
  mutate(no = 1) %>% 
  group_by(site) %>% 
  summarise(tot = sum(no))

data_summary_segment <- structurelinePSL %>%
  select(site, line, segment) %>% 
  unique() %>% 
  mutate(no = 1) %>% 
  group_by(site, line) %>% 
  summarise(tot = sum(no))
  