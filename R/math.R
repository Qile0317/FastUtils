bound_num <- function(num, lowerbound, upperbound) {
    min(max(num, lowerbound), upperbound)
}

is_bound_between <- function(num, lowerbound, upperbound) {
    (num >= lowerbound) & (num <= upperbound)
}

add <- function(x, y) x + y
subtract <- function(x, y) x - y
multiply <- function(x, y) x * y
divide <- function(x, y) x / y

is_even <- function(x) x %% 2 == 0
is_odd <- function(x) x %% 2 == 1

