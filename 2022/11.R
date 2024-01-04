rm(list=ls())
source("utilities.R")
inp <- get_input("2022/11", parse = F, deesblake = F, cache = T)

# 4:48

inp <- strsplit("Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1", "\n")[[1]]

monkey_id <- as.numeric(unlist(strsplit(sapply(strsplit(inp[seq(1, length(inp), 7)], " "), function(x) x[2]), ":")))

divisors <- as.numeric(sapply(strsplit(sapply(strsplit(inp[seq(4, length(inp), 7)], ": "), function(x) x[2]), " "), function(x) x[3]))
names(divisors) <- monkey_id

items <- lapply(strsplit(sapply(strsplit(inp[seq(2, length(inp), 7)], ": "), function(x) x[2]), ", "), function(x) as.numeric(x))
monkeys <- setNames(lapply(items, function(x) lapply(x, function(y) y %% divisors)), monkey_id)

fns <- lapply(gsub("old", "x", sapply(strsplit(sapply(strsplit(inp[seq(3, length(inp), 7)], ": "), function(x) x[2]), " = "), function(x) x[2])),
              function(x) eval(parse(text = sprintf("function(x) %s", x))))
names(fns) <- monkey_id

mTrue <- as.numeric(sapply(strsplit(sapply(strsplit(inp[seq(5, length(inp), 7)], ": "), function(x) x[2]), " "), function(x) x[4]))
names(mTrue) <- monkey_id

mFalse <- as.numeric(sapply(strsplit(sapply(strsplit(inp[seq(6, length(inp), 7)], ": "), function(x) x[2]), " "), function(x) x[4]))
names(mFalse) <- monkey_id

inspections <- setNames(rep(0, length(monkeys)), monkey_id)

for(rnd in 1:10000) {
    for(m in monkey_id) {
        items <- monkeys[[as.character(m)]]
        
        for(i in items) {
            inspections[[as.character(m)]] <- inspections[[as.character(m)]] + 1
            worry <- sapply(i, fns[[as.character(m)]]) %% divisors
            
            if(worry[as.character(m)] == 0) {
                recipient <- mTrue[[as.character(m)]]
            } else {
                recipient <- mFalse[[as.character(m)]]
            }
            
            monkeys[[as.character(recipient)]] <- c(monkeys[[as.character(recipient)]], list(worry))
        }
        
        monkeys[[as.character(m)]] <- c()
    }
}



inspections
prod(sort(inspections, decreasing = T)[1:2])  # 5:04








