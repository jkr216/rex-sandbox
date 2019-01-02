
portfolio_return_max <- function (.lst, returns) {
  l <- list(name = "max_return", type = "maximize", 
            funName = calc_port_return, params = list(returns), 
            min = -Inf, max = Inf)
  
  if (missing(.lst)) {
    return(list(l))
  }
  else {
    return(c(.lst, list(l)))
  }
}


portfolio_risk_min <- function(.lst, bench_weights, cov_matrix) {
  
  l <- list(name = "min_risk", type = "constraint", 
            funName = calc_port_risk, params = list(cov_matrix),
            min = -Inf, max = calc_port_risk(bench_weights, cov_matrix))
  
  if (missing(.lst)) {
    return(list(l))
  }
  else {
    return(c(.lst, list(l)))
  }
}


tracking_error <- function(.lst, cov_matrix, bench_weights, max_track_error){
  
  l <- list(name = "tracking_error", type = "constraint", 
            funName = calc_port_TE, params = list(bench_weights, cov_matrix),
            min = -Inf, max = max_track_error)
  
  if (missing(.lst)) {
    return(list(l))
  }
  else {
    return(c(.lst, list(l)))
  }
}


total_assets <- function(.lst, min, max){
  
  l <- list(name = "total_assets", type = "constraint", 
            funName = calc_nnonzeroassets, params = list(),
            min = min, max = max)
  
  if (missing(.lst)) {
    return(list(l))
  }
  else {
    return(c(.lst, list(l)))
  }
}


minimum_nonzero_weight <- function(.lst, lower_bound, min){
  
  l <- list(name = "min_nonzero_weight", type = "constraint", 
            funName = calc_min_nonzero_weight, params = list(lower_bound),
            min = min, max = Inf)
  
  if (missing(.lst)) {
    return(list(l))
  }
  else {
    return(c(.lst, list(l)))
  }
}


turnover <- function(.lst, current_weights, max){
  
  l <- list(name = "turnover", type = "constraint", 
            funName = calc_turnover, params = list(current_weights),
            min = -Inf, max = max)
  
  if (missing(.lst)) {
    return(list(l))
  }
  else {
    return(c(.lst, list(l)))
  }
}


relative_weights_equity <- function(.lst, equity_weights, bench_weights, slack){
  
  l <- list(name = "relative_weights_equity", type = "constraint", 
            funName = calc_weighted, params = list(equity_weights),
            min = sum(bench_weights[equity_weights == 1]) - slack,
            max = sum(bench_weights[equity_weights == 1]) + slack)
  
  if (missing(.lst)) {
    return(list(l))
  }
  else {
    return(c(.lst, list(l)))
  }
}


relative_weights_fixed_income <- function(.lst, fixed_inc_weights, bench_weights, slack){
  
  l <- list(name = "relative_weights_fixed_inc", type = "constraint", 
            funName = calc_weighted, params = list(fixed_inc_weights),
            min = sum(bench_weights[fixed_inc_weights == 1]) - slack,
            max = sum(bench_weights[fixed_inc_weights == 1]) + slack)
  
  if (missing(.lst)) {
    return(list(l))
  }
  else {
    return(c(.lst, list(l)))
  }
}

relative_weights_other <- function(.lst, other_weights, bench_weights, slack){
  
  l <- list(name = "relative_weights_other", type = "constraint", 
            funName = calc_weighted, params = list(other_weights),
            min = sum(bench_weights[other_weights == 1]) - slack,
            max = sum(bench_weights[other_weights == 1]) + slack)
  
  if (missing(.lst)) {
    return(list(l))
  }
  else {
    return(c(.lst, list(l)))
  }
}


weights_fi_cash <- function(.lst, fixed_inc_weights, min, max){
  
  l <- list(name = "weights_fi_cash", type = "constraint", 
            funName = calc_weighted, params = list(fixed_inc_weights),
            min = min,
            max = max)
  
  if (missing(.lst)) {
    return(list(l))
  }
  else {
    return(c(.lst, list(l)))
  }
}

# US large should be at least 75% of US Large + US Small equity.  This type of constraint requires a bit of creativity.
weights_us_large_relative <- function(.lst, us_large_relative_weights, min, max){
  
  l <- list(name = "us_large_relative_weights", type = "constraint", 
            funName = calc_weighted, params = list(us_large_relative_weights),
            min = min,
            max = max)
  
  if (missing(.lst)) {
    return(list(l))
  }
  else {
    return(c(.lst, list(l)))
  }
}
