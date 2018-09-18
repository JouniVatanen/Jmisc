# Load required packages
if (!require("pacman")) install.packages("pacman") #pacman loads and installs packages
pacman::p_load("checkpoint")
checkpoint("2018-08-01") # use the same date when R Open 3.5.1. was released
pacman::p_load("roxygen2", "devtools")

# Create package documentation
document()

# Install package
setwd("..")
install("Jmisc")
setwd("./Jmisc")

# Remember to commit changes and push the files to the github!
# 1. Commit changes either shell or Rstudio UI
# 2. Push to the github from shell "git push" and insert username and password
# 3. You can install from github using devtools::install_github(JouniVatanen/Jmisc)
