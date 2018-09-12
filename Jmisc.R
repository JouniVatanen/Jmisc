# Load required packages
library(roxygen2)
library(devtools)

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
