# Load required packages
#pacman loads and installs packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load("checkpoint")

# Use the date few weeks after R-3.5.3. was released
checkpoint("2019-04-01", checkpointLocation = Sys.getenv("USERPROFILE"))
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
