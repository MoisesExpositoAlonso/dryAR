library(devtools)

devtools::load_all(".")
devtools::load_all("~/mexposito/moiR/")
# library(field)
# library(moiR)

#### quickpots in space ####
qp=quickpots()
write.tsv(qp,file = 'data/qp.tsv',col.names = F)

print("The head lines of qp:")
head(qp,n=5)

devtools::use_data(qp)
