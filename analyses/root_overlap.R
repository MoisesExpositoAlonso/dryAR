
merge(acc,by.x='id',root,by.y='ACC_ID') %>% dim()
merge(ft1001,by.x='id',root,by.y='ACC_ID') %>% dim()

merge(acc,by.x='id',root,by.y='ACC_ID') %>% write.csv("~/field_root_overlap.csv")

merge(acc,by.x='id',root,by.y='ACC_ID') %>% filter(country=="SWE")


root$root=1

dplyr::full_join(acc,root,by=c("id" ='ACC_ID'))  %>% filter(country == "SWE", is.na(root)) %>%
# dplyr::full_join(acc,root,by=c("id" ='ACC_ID'))  %>% filter(country == "SWE", !is.na(root)) %>%
  # dim()
  write.csv("~/field_root_missing.csv")
