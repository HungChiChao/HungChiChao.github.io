# README

This is the documentation for R codes that works on tax filing
efficiency.

# 1 Calculate optimal tax using Gurobi

## 1.1 Intro to Gurobi

For how to install Gurobi Optimizer and the `gurobi` R package, see

- [Installing the R
  package](https://docs.gurobi.com/projects/optimizer/en/current/reference/r/setup.html)

- [CRAN- Gurobi Installation
  Guide](https://cran.r-project.org/web/packages/prioritizr/vignettes/gurobi_installation_guide.html)

Gurobi is used for solving **linear programming** problems. The
mathematical problem is passed to Gurobi by a “Gurobi model”, which is
composed of the following components.

- constraint matrix `A`
- sense of each constraint `sense`
- right hand side of the constraints `rhs`
- coefficients of the objective function `obj`
- model sense
- variable type

Here is an simple example provide by [Installing the R
package](https://docs.gurobi.com/projects/optimizer/en/current/reference/r/setup.html).

![\begin{aligned}
\max\_{x_1, x_2, x_3} \quad & x_1 + x_2 + 2x_3 \\
\text{s.t.} \quad 
& x_1 + 2x_2 + 3x_3 \leq 4 \\
& x_1 + x_2 \geq 1 \\
& x_1, x_2, x_3 \in \\0, 1\\
\end{aligned}](https://latex.codecogs.com/svg.latex?%5Cbegin%7Baligned%7D%0A%5Cmax_%7Bx_1%2C%20x_2%2C%20x_3%7D%20%5Cquad%20%26%20x_1%20%2B%20x_2%20%2B%202x_3%20%5C%5C%0A%5Ctext%7Bs.t.%7D%20%5Cquad%20%0A%26%20x_1%20%2B%202x_2%20%2B%203x_3%20%5Cleq%204%20%5C%5C%0A%26%20x_1%20%2B%20x_2%20%5Cgeq%201%20%5C%5C%0A%26%20x_1%2C%20x_2%2C%20x_3%20%5Cin%20%5C%7B0%2C%201%5C%7D%0A%5Cend%7Baligned%7D "\begin{aligned}
\max_{x_1, x_2, x_3} \quad & x_1 + x_2 + 2x_3 \\
\text{s.t.} \quad 
& x_1 + 2x_2 + 3x_3 \leq 4 \\
& x_1 + x_2 \geq 1 \\
& x_1, x_2, x_3 \in \{0, 1\}
\end{aligned}")

The following code solve the problem.

``` r
library(gurobi)
```

    Warning: package 'gurobi' was built under R version 4.5.0

    Loading required package: slam

``` r
# create optimization problem
model <- list()
model$obj        <- c(1, 1, 2)
model$modelsense <- "max"
model$rhs        <- c(4, 1)
model$sense      <- c("<", ">")
model$vtype      <- "B"
model$A          <- matrix(c(1, 2, 3, 1, 1, 0), nrow = 2, ncol = 3,
                           byrow = TRUE)

# solve the optimization problem using Gurobi
result <- gurobi(model, list())
```

    Set parameter Username
    Set parameter LicenseID to value 2689162
    Academic license - for non-commercial use only - expires 2026-07-18
    Gurobi Optimizer version 12.0.3 build v12.0.3rc0 (mac64[arm] - Darwin 23.1.0 23B81)

    CPU model: Apple M1
    Thread count: 8 physical cores, 8 logical processors, using up to 8 threads

    Non-default parameters:
    LogToConsole  0

    Optimize a model with 2 rows, 3 columns and 5 nonzeros
    Model fingerprint: 0xba2d0add
    Variable types: 0 continuous, 3 integer (3 binary)
    Coefficient statistics:
      Matrix range     [1e+00, 3e+00]
      Objective range  [1e+00, 2e+00]
      Bounds range     [0e+00, 0e+00]
      RHS range        [1e+00, 4e+00]
    Found heuristic solution: objective 2.0000000
    Presolve removed 2 rows and 3 columns
    Presolve time: 0.01s
    Presolve: All rows and columns removed

    Explored 0 nodes (0 simplex iterations) in 0.01 seconds (0.00 work units)
    Thread count was 1 (of 8 available processors)

    Solution count 2: 3 2 

    Optimal solution found (tolerance 1.00e-04)
    Best objective 3.000000000000e+00, best bound 3.000000000000e+00, gap 0.0000%

``` r
print(result$objval) # objective
```

    [1] 3

``` r
print(result$x)
```

    [1] 1 0 1

## 1.2 Solving Optimal Tax Filing Strategy

**The Dependency Problem**

Suppose that A and B are a couple who have three grown-up children. Each
of them have salary income and pay taxes. When filing the tax in May,
claiming parents as dependency can potentially reduce the tax required,
but one parent can only be dependent to one children. Who should the two
parents depend on? Or should they not depend on any of the children?

The following table show the data for a family, in which

1.  There are three chidren in this family who are currently paying
    taxes,
2.  Only father is currently alive,
3.  The father has no income and is over 70. Hence, he brings NT 145500
    deduction to the child who he is dependent to.

``` r
#|message = false
#|warning = false

library(data.table)
```


    Attaching package: 'data.table'

    The following object is masked from 'package:slam':

        rollup

``` r
exm_dt <- data.table(
  rfn = c("0000001", "0000002", "0000003"), # fake rfn
  true_income = c(1000000, 500000, 100000), 
  fr_alive = c(1,1,1), # should be all identical
  mo_alive = c(0,0,0),  # should be all identical
  fr_effect = c(145500, 145500, 145500),
  mo_effect = c(0,0,0)
)

exm_dt
```

| rfn     | true_income | fr_alive | mo_alive | fr_effect | mo_effect |
|:--------|------------:|---------:|---------:|----------:|----------:|
| 0000001 |       1e+06 |        1 |        0 |    145500 |         0 |
| 0000002 |       5e+05 |        1 |        0 |    145500 |         0 |
| 0000003 |       1e+05 |        1 |        0 |    145500 |         0 |

Fortunately, this problem can be represented as a linear programming
problem. Here’s how we can formalize the problem.

![\begin{aligned}
(y_1^\*, y_2^\*, y_3^\*) =& \arg \min \left( \sum\_{k} \text{tax}(y_i) + 0\*f_i + 0\*m_i \right) \quad \text{such that} \\
&f\_{1} \cdot F + m\_{1} \cdot M + y_1 = i_1\\
&f\_{2} \cdot F + m\_{2} \cdot M + y_2 = i_2\\
&f\_{3} \cdot F + m\_{3} \cdot M + y_3 = i_3\\
&\sum\_{i=1}^3 f\_{i} = 1\\ 
&\sum\_{i=1}^3 m\_{i} = 1\\
&m_i \le \text{Has\\Mo}, f_i \le \text{Has\\Fr}.
\end{aligned}](https://latex.codecogs.com/svg.latex?%5Cbegin%7Baligned%7D%0A%28y_1%5E%2A%2C%20y_2%5E%2A%2C%20y_3%5E%2A%29%20%3D%26%20%5Carg%20%5Cmin%20%5Cleft%28%20%5Csum_%7Bk%7D%20%5Ctext%7Btax%7D%28y_i%29%20%2B%200%2Af_i%20%2B%200%2Am_i%20%5Cright%29%20%5Cquad%20%5Ctext%7Bsuch%20that%7D%20%5C%5C%0A%26f_%7B1%7D%20%5Ccdot%20F%20%2B%20m_%7B1%7D%20%5Ccdot%20M%20%2B%20y_1%20%3D%20i_1%5C%5C%0A%26f_%7B2%7D%20%5Ccdot%20F%20%2B%20m_%7B2%7D%20%5Ccdot%20M%20%2B%20y_2%20%3D%20i_2%5C%5C%0A%26f_%7B3%7D%20%5Ccdot%20F%20%2B%20m_%7B3%7D%20%5Ccdot%20M%20%2B%20y_3%20%3D%20i_3%5C%5C%0A%26%5Csum_%7Bi%3D1%7D%5E3%20f_%7Bi%7D%20%3D%201%5C%5C%20%0A%26%5Csum_%7Bi%3D1%7D%5E3%20m_%7Bi%7D%20%3D%201%5C%5C%0A%26m_i%20%5Cle%20%5Ctext%7BHas%5C_Mo%7D%2C%20f_i%20%5Cle%20%5Ctext%7BHas%5C_Fr%7D.%0A%5Cend%7Baligned%7D "\begin{aligned}
(y_1^*, y_2^*, y_3^*) =& \arg \min \left( \sum_{k} \text{tax}(y_i) + 0*f_i + 0*m_i \right) \quad \text{such that} \\
&f_{1} \cdot F + m_{1} \cdot M + y_1 = i_1\\
&f_{2} \cdot F + m_{2} \cdot M + y_2 = i_2\\
&f_{3} \cdot F + m_{3} \cdot M + y_3 = i_3\\
&\sum_{i=1}^3 f_{i} = 1\\ 
&\sum_{i=1}^3 m_{i} = 1\\
&m_i \le \text{Has\_Mo}, f_i \le \text{Has\_Fr}.
\end{aligned}")

The following function create a `gurobi model` and solve the model. The
input data should be a `data.table` as above. The variables are listed
in order of

![\\f\_{1}, m\_{1}, y_1, f\_{2}, m_2, y_2,...\\.](https://latex.codecogs.com/svg.latex?%5C%7Bf_%7B1%7D%2C%20m_%7B1%7D%2C%20y_1%2C%20f_%7B2%7D%2C%20m_2%2C%20y_2%2C...%5C%7D. "\{f_{1}, m_{1}, y_1, f_{2}, m_2, y_2,...\}.")

In our setting the objective function `tax()`is not linear but
piece-wise linear. So we use `pwlobj` instead of `obj`. Two vectors, `u`
and `uy` should be supported beforehand. I did not make them arguments
of the function because I use function factories.

``` r
best_strategy_fm <- function(Data){
    D <- copy(Data)
    
    {
      I <- Data$true_income 
      
      # Whether their father/ mother still alive 
      Has_father <- Data$fr_alive[1]
      Has_mother <-  Data$mo_alive[1]
      
      # Deduction
      fr_effect <- Data$fr_effect[1]
      mo_effect <- Data$mo_effect[1]
      K <- length(I)
      n_variable <- K*3
      
      # Create Groubi Model
      model <- list()
      model$modelsense <- "min"
      
      # List of constraints
      A_matrix <- matrix(0,nrow = K, ncol = n_variable)
      rhs_vector <- numeric(K)
      
      for (i in 1:length(I)) {
        x_no_support <- I[i]
        
        # Equality Constraint i
        # xi= fi*support + mi*support_mother + (1-fi-mi)*x_no_support
        A_matrix[i,(i-1)*3+1] <- fr_effect
        A_matrix[i,(i-1)*3+2] <- mo_effect
        A_matrix[i,(i-1)*3+3] <- 1
        rhs_vector[i] <- max(x_no_support,0)
        
        # Piece-wise linear Objective function of Tax( xi)
        model$pwlobj[[(i-1)*3+3]] <- list(var=(i-1)*3+3, x=u, y=uy)
        model$pwlobj[[(i-1)*3+2]] <- list(var=(i-1)*3+3, x=0, y=0)
        model$pwlobj[[(i-1)*3+1]] <- list(var=(i-1)*3+3, x=0, y=0)
      }
      
      # Father Dependency Constraint (Binary, Inequality)
      sum_constraint <- numeric(n_variable)
      sum_constraint[seq(1,by=3,length.out=K)] <- Has_father
      A_matrix <- rbind(A_matrix,sum_constraint)
      rhs_vector <- c(rhs_vector,Has_father)
      
      # Mother Dependency Constraint (Binary, Inequality)
      sum_constraint <- numeric(n_variable)
      sum_constraint[seq(2,by=3,length.out=K)] <-Has_mother
      A_matrix <- rbind(A_matrix,sum_constraint)
      rhs_vector <- c(rhs_vector,Has_mother)
      
      model$A <- A_matrix
      model$rhs <- rhs_vector
      
      # 可以都不分配爸媽 
      model$sense <- c(rep("=",K),rep("<=",2))
      
      # Upper Bound And Lower Bound
      model$lb <- rep(c(0,0,-Inf),K)
      model$ub <- rep(c(Has_father,Has_mother,Inf),K)
      
      # Variable Types: Variable Types, binary or continuous
      model$vtype <- rep(c(rep("B",2),"C"),K)
      
      
      result <- gurobi(model,params = list( OutputFlag=0))
      
      solution <- result$x
      
    }
    Fr_distribution <- solution[seq(1, by=3, length.out=K)]
    Mo_distribution <- solution[seq(2, by=3, length.out=K)]
    
    D[,best_daddy := Fr_distribution]
    D[,best_mommy := Mo_distribution]
    D[,best_income_net_dis := solution[seq(3,by=3,length.out=K)]]
    
    return(D)
  }
```

## 1.3 Calculating Tax the Function

The `tax_calcultor` is a function factory that generate year-specific
functions for calculating the progressive tax.

``` r
tax_rate_table <- data.frame(
  Yr99 = c(500000,1130000,2260000,4230000, NA) ,
  Yr100 = c(500000,1130000,2260000,4230000, NA),
  Yr101 = c(500000,1130000,2260000,4230000, NA),
  Yr102 = c(520000,1170000,2350000,4400000, NA),
  Yr103 = c(520000,1170000,2350000,4400000, NA),
  Yr104 = c(520000,1170000,2350000,4400000, 10000000),
  Yr105 = c(520000,1170000,2350000,4400000, 10000000)
  )


tax_calculator <- function( cutoff){
    cutoff <- cutoff[! is.na( cutoff)]
    income_lowerbound <- c(0, cutoff)
    
    rate_scheme <- c(0.05, 0.12, 0.2, 0.3, 0.4, 0.45)
    rate <- rate_scheme[ 1: length(income_lowerbound) ]
    
    # This is the magic formula
    cum_diff <- cumsum( c(0, cutoff * diff(rate)))
    
    calculator <- function(inc){
        loc <- sum( inc > income_lowerbound)
        tax <- inc*rate[loc] - cum_diff[loc]
        return( max(tax, 0) )
      }
      
      return(calculator)
}
```

You can use the function to calculate u and v beforehand and supply it
to the `best_strategy_fm()` function.

``` r
best_strategy_fm_yearly <- function(yr){
  
  ## 0 Set Up (By year) --------------------------------------------
  
  # functions for calculating tax
  tax_cal <- tax_calculator( tax_rate_table[[ yr - START_YR + 1]] )
  cutoff <- tax_rate_table[[yr - START_YR + 1]]
  cutoff <- cutoff[ ! is.na(cutoff) ]
  
  # Objective Function
  u <- c( -10000, 0, cutoff, 100000000)
  uy <- sapply(u,tax_cal)
  
  best_strategy_fm <- function(Data){
    #...
    model$pwlobj[[(i-1)*3+3]] <- list(var=(i-1)*3+3, x=u, y=uy)
    #...
  }
  
  return(best_strategy_fm)
}
```
