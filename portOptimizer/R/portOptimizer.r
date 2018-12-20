#' Calculates slack
#'
#' Calculates weights that can be added or subtracted from a portfolios without violating box contraints.
#' Also returns the indices representing which asset classes may be increased and decreased
#'
#' @param x Portfolio weights
#' @param boxmin Box min constraints
#' @param boxmax Box max constraings
#' @param maxdelta Maximum amount by which to change an asset class
#'
#' @return
#'
#' @examples calc_slack(x, boxmin, boxmax, 0.03)
calc_slack <- function(x, boxmin, boxmax, maxdelta){
  incr <- pmin(pmax(0, boxmax - x), maxdelta)
  decr <- pmin(pmax(0, x - boxmin), maxdelta)
  classesToIncr <- which(incr>0)
  classesToDecr <- which(decr>0)
  return(list(incr=incr, decr=decr, classesToIncr=classesToIncr, classesToDecr=classesToDecr))
}

#' Portfolio optimization using a pairwise algorithm
#'
#' This is a sub function using one maxdelta
#' This will attempt to maximize a supplied objective function within box constraints.
#' The algorithm begins by finding all the asset classes with weights that can be increased and those
#' that can be decreased.  Using pairs of classes within the same account, it increases one and decreases the other
#' finding the best pair to change.  If a pair improves the objective value, the process is repeated.  If no pair is found
#' the maxdelta is halved and the process is repeated until the delta is <= 0.00001.
#'
#' @param x Initial set of weights. Must sum to 1 (or whatever sum is desired).  Weights across accounts must be correct.
#' @param LB Box minimum
#' @param UB Box maximum
#' @param objFun Objective function
#' @param maxdelta Initial setting of delta.
#' @param ... Parameters to be passed to the objective function
#'
#' @return List with MaxObj the value of the objective; x the corresponding portfolio weights, and Iter the number of iterations
#'
#' @examples maximize_pairwise(x, LB, UB, objFun, maxdelta, ...)
maximize_objective_delta <- function(x, LB, UB, objFun, maxdelta, ...){
  bestx <- x
  maxobj <- objFun(x=bestx, ...)
  slack <- calc_slack(x=bestx, boxmin=LB, boxmax=UB, maxdelta=maxdelta)
  bestfx <- maxobj

  iter <- 0
  while(maxdelta > 0.00001){
    iter <- iter + 1
    for(i in slack[["classesToIncr"]]){
      for(j in slack[["classesToDecr"]]){
        # only increase / decrease pairs within same account.

          delta <- min(slack[["incr"]][i], slack[["decr"]][j])
          x<-bestx
          x[i] <- bestx[i] + delta
          x[j] <- bestx[j] - delta
          fx <- objFun(x, ...)
          if(fx > bestfx){
            bestfx <- fx
            besti <- i
            bestj <- j
          }

      }
    }
    if(bestfx > maxobj){
      delta <- min(slack[["incr"]][besti], slack[["decr"]][bestj])
      bestx[besti] <- bestx[besti] + delta
      bestx[bestj] <- bestx[bestj] - delta
      maxobj <- bestfx
      slack <- calc_slack(bestx, LB, UB, maxdelta)
      # cat(iter, maxobj, "\n")
    } else {
      maxdelta <- maxdelta/2
      slack <- calc_slack(bestx, LB, UB, maxdelta)
    }

  }
  out <- list()
  out$MaxObj <- maxobj
  out$x <- bestx
  # out$Iter <- iter
  return(out)
}

#' Portfolio optimization using a pairwise algorithm
#'
#' This will attempt to maximize a supplied objective function within box constraints.
#' The algorithm begins by finding all the asset classes with weights that can be increased and those
#' that can be decreased.  Using pairs of classes, it increases one and decreases the other
#' finding the best pair to change.  If a pair improves the objective value, the process is repeated.  If no pair is found
#' the maxdelta is halved and the process is repeated until the delta is <= 0.00001.
#'
#' @param x Initial set of weights. Must sum to 1 (or whatever sum is desired).  Weights across accounts must be correct.
#' @param LB Box minimum
#' @param UB Box maximum
#' @param objFun Objective function. First parameter should be the portfolio weights
#' @param maxdelta Initial setting of delta.
#' @param ... Parameters to be passed to the objective function
#'
#' @return List with MaxObj the value of the objective; x the corresponding portfolio weights, and Iter the number of iterations
#' @export
#'
#' @examples maximize_pairwise(x, LB, UB, objFun, maxdelta, ...)
maximize_objective <- function(x, LB, UB, objFun, ...){
  #if(sum(x < LB) + sum(x > UB) >0){
  #  stop("At least one portfolio weight is below its lower bound or above its upper bound.")
  #}
  maxdelta <- c(0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.10)
  #get("maxdelta", envir=portOptimizer.env)
  newfun <- function(md, ...){
    temp <- maximize_objective_delta(x=x,
                              LB=LB,
                              UB=UB,
                              objFun= objFun,
                              maxdelta = md,
                              ...)
  }
  temp <- lapply(maxdelta, newfun, ...)
  out <- temp[[which.max(sapply(temp, "[", "MaxObj"))]]
  return(out)
}

#' Create / add a term to an objective definition
#'
#' @param .lst If missing, this is ignored. If present, the function appends a term (a list) to this.
#' @param name A description of the term. Useful for provide a breakdown of each term's contribution to the objective function.
#' @param type Either 'maximize', 'minimize', or 'contraint'. If a constraint, then the `min` and/or `max` should come into play.
#' @param funName A function
#' @param params The (not x) parameters needed by `funName`.  All functions will need the weights (x). We assume that's the first parameter and it should be ommitted from `params``
#' @param min Minimum allowed value for constraints. Default is -Inf.  Ignored if -Inf.
#' @param max Maximum allowed value for constraints. Default is Inf. Ignored if Inf.
#'
#' @return List
#' @export
#'
#' @examples addTermToObjective(name = "MaxReturn", type="maximize", funName=calc_port_return, params = list(expected.returns))
#' @examples addTermToObjective(name = "Risk", type = "constraint", funName = calc_port_risk, params = list(as.matrix(expected.cov)), min = -Inf, max = calc_port_risk(benchX, as.matrix(expected.cov)))
addTermToObjective <- function(.lst, name = "", type = "constraint", funName, params = list(x), min = -Inf,
                               max = Inf){
  type <- tolower(type)
  if(!(type %in% c("maximize", "minimize", "constraint"))) error('type not in c("maximize", "minimize", "constraint")')
  l <- list(name = name,
            type = type,
            funName = funName,
            params = params,
            min = min,
            max = max)
  if(missing(.lst)) {
    return(list(l))
  } else {
    return(c(.lst, list(l)))
  }
}

#' Calculate objective max or min associated with an object definition
#'
#' @param obj List defining constraint
#' @param x vector of portfolio weights
#'
#' @return scalar
#'
#' @examples calc_maxmin(obj, x)
calc_maxmin <- function(obj, x){
  do.call(obj$funName, c(list(x),obj$params))
}

#' Calculate penalty associated with a constraint in an object definition
#'
#' Only returns squared error. The penaltyFactor has not been applied.
#'
#' @param obj List defining constraint
#' @param x vector of portfolio weights
#'
#' @return scalar
#'
#' @examples calc_constraint(obj, x)
calc_constraint <- function(obj, x){
  v <- do.call(obj$funName, c(list(x),obj$params))
  return(ifelse(v >= obj$min, 0, (v - obj$min) ^2) +
           ifelse(v <= obj$max, 0, (v - obj$max) ^2))
}

#' Calculate an objective given a objective definition
#'
#' @param x Vector of portfolio weights
#' @param obj_definition A list serving as an object definition.
#' @param penaltyFactor A large value by which squared errors are multiplied to produce a penalty
#'
#' @return value
#' @export
#'
#' @examples calc_objective(x, obj_definition)
calc_objective <- function(x, obj_definition, penaltyFactor = 25e9){
  constraints <- unlist(sapply(obj_definition, "[", "type"))  == "constraint"
  maximize <- unlist(sapply(obj_definition, "[", "type"))  == "maximize"
  minimize <- unlist(sapply(obj_definition, "[", "type"))  == "minimize"
  sum_obj <- ifelse(sum(maximize) == 0, 0, sum(sapply(obj_definition[maximize], calc_maxmin, x=x))) -
    ifelse(sum(minimize) == 0,0, sum(sapply(obj_definition[minimize], calc_maxmin, x=x)))
  sum_penalties <- ifelse(sum(constraints) == 0, 0, sum(sapply(obj_definition[constraints],calc_constraint, x=x)))
  return(sum_obj - sum_penalties * penaltyFactor)
}

#' Calculate individual terms of the objective function
#'
#' @param x Vector of portfolio weights
#' @param obj_definition objective definition list perhaps created with addTermToObjective function
#' @param terms Default NULL in which all terms calculated. Otherwise a numeric vector of terms to be calculated.
#'
#' @return tibble with Name, Value and Type of the term.
#' @export
#'
#' @examples calc_objective_terms(x, obj_definition)
#' @examples calc_objective_terms(x, obj_definition, 1)
#' @examples calc_objective_terms(x, obj_definition, c(3,5))
calc_objective_terms <- function(x, obj_definition, terms=NULL){
  if(is.null(terms)) terms <- 1:length(obj_definition)
  out <- tibble(Name=sapply(terms, function(y) obj_definition[[y]]$name),
                Value=sapply(terms, function(y) obj_definition[[y]]$type),
                Type=sapply(terms, function(y) calc_objective(x=x, obj_definition = obj_definition[y])))
  return(out)
}
