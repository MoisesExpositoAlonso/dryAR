library(field)
library(moiR)


data(field)


field %>%
  select(site=="madrid", water=="l" , indpop="i", replicate=="1") 

# here I will separate the fitnes per replicate to calculate genome-wide
# selection


# Run gemma

rungemma()


