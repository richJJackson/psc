### print.psc


print.psc <- function(x){
  cat("Call:\n", "model + beta")

  cat("Coefficients:\n")
  print.default(format(coef(x), digits = 1), print.gap = 2L,
                quote = FALSE)

}



#function (x, digits = max(3L, getOption("digits") - 3L), ...)
#{
#  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
#      "\n\n", sep = "")
#  if (length(coef(x))) {
#    cat("Coefficients:\n")
#    print.default(format(coef(x), digits = digits), print.gap = 2L,
#                  quote = FALSE)
#  }
#  else cat("No coefficients\n")
#  cat("\n")
#  invisible(x)
#}
