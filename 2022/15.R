rm(list=ls())
source("utilities.R")
inp <- get_input("2022/15", parse = F, deesblake = F, cache = T)

inp <- strsplit("Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3", "\n")[[1]]

sensors <- list()
beacons <- list()
dist <- list()

for(i in 1:length(inp)) {
    ss <- strsplit(strsplit(inp[i], "at ")[[1]], ":")
    sensor <- as.numeric(sapply(strsplit(strsplit(ss[[2]][1], ", ")[[1]], "="), function(x) x[2]))
    beacon <- as.numeric(sapply(strsplit(strsplit(ss[[3]][1], ", ")[[1]], "="), function(x) x[2]))
    
    sensors[[i]] <- sensor
    beacons[[i]] <- beacon
    dist[[i]] <- sum(abs(sensor - beacon))
}

beacons <- unique(beacons)
# cleared <- list()

# invisible(lapply(1:length(sensors), function(i) {
#     x_range <- (sensors[[i]] - dist[[i]])[1]:(sensors[[i]] + dist[[i]])[1]
#     y_range <- (sensors[[i]] - dist[[i]])[2]:(sensors[[i]] + dist[[i]])[2]
#     
#     for(y in y_range) {
#         y_dist <- abs(sensors[[i]][2] - y)
#         x_vals <- (sensors[[i]][1] - (dist[[i]] - y_dist)):(sensors[[i]][1] + (dist[[i]] - y_dist))
#         cleared <<- c(cleared, lapply(x_vals, function(x) c(x, y)))
#     }
# }))
# 
# cleared <- unique(cleared)
# cleared <- cleared[!(cleared %in% beacons)]
# cleared <- cleared[!(cleared %in% sensors)]


# Part 2 ------------------------------------------------------------------

# Will take too long searching 4M x 4M space
# Only one possible spot for a distress beacon; it must be just 1 space outside the max bounds of each sensor

potentials <- unique(unlist(lapply(1:length(sensors), function(i) {
    y_bounds <- c(sensors[[i]][2] - dist[[i]] - 1, sensors[[i]][2] + dist[[i]] + 1)
    
    pot_sensor <- unlist(lapply((y_bounds[1] + 1):(y_bounds[2] - 1), function(y) {
        x_delta <- abs((sensors[[i]][2] - y) - dist[[i]]) + 1
        
        list(c(sensors[[i]][1] - x_delta, y),
             c(sensors[[i]][1] + x_delta, y))
    }), recursive = F)
    
    # Add top and bottom
    pot_sensor <- c(pot_sensor,
                    list(c(sensors[[i]][1], sensors[[i]][2] - dist[[i]] - 1),
                         c(sensors[[i]][1], sensors[[i]][2] + dist[[i]] + 1)))
    
    return(pot_sensor)    
}), recursive = F))

bounds <- c(0, 4000000)
potentials <- potentials[sapply(potentials, function(x) x[1] >= bounds[1] && x[1] <= bounds[2] && x[2] >= bounds[1] && x[2] <= bounds[2])]
'potentials' <- potentials[!(potentials %in% c(beacons, sensors))]

# Try and remove potentials, by checking whether each sensor covers it

clears <- unlist(lapply(1:length(sensors), function(i) {
    x_range <- (sensors[[i]] - dist[[i]])[1]:(sensors[[i]] + dist[[i]])[1]
    y_range <- (sensors[[i]] - dist[[i]])[ <- 2]:(sensors[[i]] + dist[[i]])[2]
    
    cleared <- unlist(lapply(y_range, function(y) {
        y
        
        y_dist <- abs(sensors[[i]][2] - y)
        x_vals <- (sensors[[i]][1] - (dist[[i]] - y_dist)):(sensors[[i]][1] + (dist[[i]] - y_dist))
        lapply(x_vals, function(x) c(x, y))
        
    }), recursive = F)
    
    return(cleared)    
}), recursive = F)

# Convert to string?
potentials[!(sapply(potentials, function(x) sprintf("%s|%s", x[1], x[2])) %in% sapply(clears, function(x) sprintf("%s|%s", x[1], x[2])))][[1]]

       