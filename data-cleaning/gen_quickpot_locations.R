# ---
# title: Read the harvesting data
# author: "Moises Exposito-Alonso (moisesexpositoalonso@gmail.com)"
# date: '`r Sys.Date()`'
# ---

message('-------------------------------------------------')
message('Generating quickpot locations list data')
message('-------------------------------------------------')

library(devtools)

devtools::load_all(".")
devtools::load_all("~/mexposito/moiR/")

#### quickpots in space ####
qp=quickpots()
write.tsv(qp,file = 'data/qp.tsv',col.names = F)

print("The head lines of qp:")
print(head(qp,n=5))

devtools::use_data(qp,overwrite = T)
