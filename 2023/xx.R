rm(list=ls())
source("utilities.R")
inp <- get_input("2023/xx", parse = F, deesblake = F, cache = T)

inp <- strsplit("", "\n")[[1]]