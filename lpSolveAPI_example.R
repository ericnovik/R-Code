library("lpSolveAPI")

# test lp_solve on this problem: https://www.youtube.com/watch?v=-32jcGMpD2Q
lp_model <- make.lp(0, 2)

# for larger problems use set.column(), set.constr.type(), and set.rhs()
# http://www.icesi.edu.co/CRAN/web/packages/lpSolveAPI/vignettes/lpSolveAPI.pdf

add.constraint(lp_model, c(1, 1), "<=", 240)
add.constraint(lp_model, c(2, 1), "<=", 320)
set.objfn(lp_model, c(40, 30))
lp.control(lp_model, sense = 'max')
str(lp_model)
lp_model 
dimnames(lp_model) <- list(c("First", "Second"),  c("X", "Y"))
lp_model

# solve will return 0 if success
if (!solve(lp_model)) {
  cat("Optimal Solution:", get.objective(lp_model), "\n")
  cat("Optimal Allocation:", get.variables(lp_model), "\n")
} else {
  cat("No solution")
}
