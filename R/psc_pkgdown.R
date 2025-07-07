### pkg_down

library(pkgdown)

# Run once to configure your package to use and deploy pkgdown
usethis::use_pkgdown_github_pages()

use_pkgdown()

pkgdown::build_site()
