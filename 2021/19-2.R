input <- readLines("19.txt")

input <- strsplit('0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb', '\n')[[1]]


rules <- input[1:(which(input == "") - 1)]
messages <- input[(which(input == "") + 1):length(input)]

rules <- gsub('"', '', rules)

rules_list <- as.list(sapply(strsplit(rules, ": "), function(x) x[2]))
names(rules_list) <- sapply(strsplit(rules, ": "), function(x) x[1])

rules_0 <- strsplit(rules_list[["0"]], " ")[[1]]

for(message in messages) {
    message
    
    rules_list[["4"]]        
}
