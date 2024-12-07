
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")
inp <- get_input("2015/01", parse = F, user = "blakeebets", cache = T)

# Problem -----------------------------------------------------------------

ss <- strsplit(inp, "")[[1]]

sum(ss == "(") - sum(ss == ")")

# 0:36 (1st)

ss[ss == "("] <- 1
ss[ss == ")"] <- -1
ss <- as.numeric(ss)

cs <- cumsum(ss)
which(cs == -1)[1]

# 1:12 (1st)
