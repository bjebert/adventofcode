
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")
inp <- get_input("2015/08", parse = F, user = "blakeebets", cache = T)

# Problem -----------------------------------------------------------------


code_len <- sapply(inp, nchar)

memory <- gsub('\\\\\\\\', '\\\\', inp)
memory <- gsub('\\\\"', "'", memory)
memory <- gsub('"', '', memory)
memory <- gsub('\\\\x[0-9|a-f][0-9|a-f]', '*', memory)
                 
sum(code_len) - sum(sapply(memory, nchar))  # 1439 hi

# 11:52 (75th)

ss <- strsplit(inp, "")

encoded_len <- sum(sapply(ss, function(x) {
    2 + sum(x == '"') + sum(x == '\\') + length(x)
}))

sum(encoded_len) - sum(code_len)

# 17:20 (56th)