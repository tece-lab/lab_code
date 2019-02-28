# 28/2/19 Pedro Neves (@Neves-P) RUG, TECE Lab

# Let's quickly create an R package!

# To start from scratch, it's easy to use the package usethis

install.packages("usethis")
install.packages("devtools")

# Create package automatically creates the barebones of an R package
usethis::create_package("D:/Documents/code/anewpackage/")

# We now have a new package!
# Let's setup other useful goodies
# This is a handy package description
usethis::use_package_doc()

# This documents our package! Make sure you have roxygen documentation already
# written
devtools::document()

# We must also add a license! I will use GPL-3. We don't have to edit this
# manually in the DESCRIPTION file (even though we could), but it's enough
# to run usethis::use_gpl3_license() (other license templates are available)
usethis::use_gpl3_license(name = "Pedro Neves")

# We should really write some tests. This will initialize the testthat
# framework for our package
usethis::use_testthat()

# And this creates a test for our nice functions. We can even do this before we
# have written our code. (Test-driven development)
usethis::use_test("R/compute_cubes.R")

# Building our package is essentially sourcing everything, making it ready to
# export (in a .tar.gz) and loads it again. To share, a package MUST build
# or it can't be installed and loaded with library() or require(). To build
# use the shortcut Ctrl-Shift-B, or run devtools::build(). Do this, or
# alternatively Ctrl-Shift-L (devtools::load_all()) anytime you make changes to
# your code and want to re-run your package. Think of it a bit like sourcing, or
# compiling in a compiled language (which R is not, it's interpreted).
devtools::build()

# It may happen that some files in your directory should not be in the package.
# Perhaps you created a temporary figure in the wd, or have an extra license
# file. This is ok, we just need to tell R to not bother about those files.
# Do this using usethis::use_build_ignore(). I will run this on this source file
# so this code doesn't run when I build the package.
usethis::use_build_ignore(files = "R/notes.R")


# Since we've written some nice tests, we can now run them. A shortcut for this
# is Ctrl-Shift-T, or running devtools::test()
devtools::test()

# Finally, the last check we should do is, precisely, Check! The hotkey for this
# is Ctrl-Shift-E or run devtools::check(). Check runs the same tests that CRAN
# will run if you submit your package to them. It checks many things, including
# if your dependencies are correct, if your DESCRIPTION file is fine, if you
# have a license, if all your exported functions are documented (as they should)
# and many more useful things. Use it (very) often!

# Finally, some notes on git. If you created a package like this, you can
# usethis::use_git(), intializing a git repo
usethis::use_git()
