library(tidyverse)
library(flextable)
library(magrittr)

set_flextable_defaults(
  theme_fun = theme_booktabs,
  big.mark = " ", 
  font.color = "#666666",
  border.color = "#666666",
  padding = 3,
)
## ---- transfers --------
dat <- read.csv("../Data/Raw_data/Transfers.csv")
ddat <- dat[c(1,2,3,5,8,11,13,15,17)]

names(ddat) <- c("AOW", "seq", "BPBM", "AB", "valid", "rev1", "rev2", "rev3", "rev4")

ddat <- ddat[ddat$seq=="N",]

ddat$AOW <- sub("Species: ", "", ddat$AOW)

ddat$latitude <- ""
ddat$longitude <- ""

ddat$name <- sub("^(\\S*\\s+\\S+).*", "\\1", ddat$AOW)   
	# 	^ - start of string
	# (\\S*\\s+\\S+) - Group 1 capturing 0+ non-whitespace chars, then 1+ whitespaces, and then 1+ non-whitespaces
	# .* - any 0+ chars, as many as possible (up to the end of string).
	## Note that in case your strings might have leading whitespace, and you do not want to count that whitespace in, you should use
	## sub("^\\s*(\\S+\\s+\\S+).*", "\\1", x)
	
ddat$AOW <- gsub("[(]|[)]", "", ddat$AOW)
ddat$description <- sub("^(\\S*\\s+\\S+)\\s+(.*)", "\\2", ddat$AOW)

ddat <- ddat[c("name", "latitude", "longitude", "BPBM","description", "rev1", "rev2", "rev3", "rev4")]

ddat$rev1
ddat$rev2
ddat$rev3
ddat$rev4

ddat$revisions <- ddat$rev1
ddat$revisions[ddat$rev2!=""] <- with(ddat[ddat$rev2!="",], paste(rev1, rev2, sep=" || "))
ddat <- ddat[c("name","latitude","longitude","BPBM","description", "revisions")]
write.csv(ddat, file="../Data/Raw_data/add_gps.csv")