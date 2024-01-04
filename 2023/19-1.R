inp <- strsplit("px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}", "\n")[[1]]

inp <- readLines("2023/19.txt")

w_blank <- which(inp == "")

workflow_names <- sapply(strsplit(inp[1:(w_blank-1)], "\\{"), function(x) x[1])
workflow_desc <- gsub("\\}", "", sapply(strsplit(inp[1:(w_blank-1)], "\\{"), function(x) x[2]))
workflows <- setNames(strsplit(workflow_desc, ","), workflow_names)

ratings <- lapply(inp[(w_blank+1):length(inp)], function(x) {
    part_vec <- gsub("\\{|\\}", "", unlist(strsplit(strsplit(x, "=")[[1]], ",")))
    setNames(as.numeric(part_vec[seq(2, length(part_vec), 2)]), part_vec[seq(1, length(part_vec), 2)])
})

# Part 1 ------------------------------------------------------------------

test <- function(part, flow) {
    step <- flow[1]
    
    if(step == "A") {
        return(TRUE)
    } else if(step == "R") {
        return(FALSE)
    }
    
    if(grepl(":", step)) {
        step_split <- strsplit(step, ":")[[1]]
        cond_split <- strsplit(step_split[1], ">|<")[[1]]
        
        is_gt <- grepl(">", step_split[1])
        
        cond_met <- (is_gt && part[cond_split[1]] > as.numeric(cond_split[2])) ||
            (!is_gt && part[cond_split[1]] < as.numeric(cond_split[2]))
        
        if(cond_met) {
            return(test(part, step_split[2]))
        } else {
            return(test(part, flow[2:length(flow)]))
        }
        
    } else {
        return(test(part, workflows[[step]]))
    }
}

is_approved <- sapply(ratings, function(part) test(part, workflows[["in"]]))

sum(sapply(ratings[is_approved], sum))
