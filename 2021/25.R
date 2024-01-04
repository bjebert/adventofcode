input <- c(3418282, 8719412)

# Actual input ^ ----------------------------------------------------------

# input <- c(5764801, 17807724)

value <- 1
subject <- 7
loop_size <- 1

while(TRUE) {
    value <- (value * subject) %% 20201227
    
    if(value == input[2]) {
        print(loop_size)
        break
    }
    
    loop_size <- loop_size + 1
}

# Loop size for card: 8987376
# Loop size for door: 14382089

value <- 1
subject <- input[1]
loop_size <- 14382089
for(i in 1:loop_size) {
    value <- (value * subject) %% 20201227
}

value
