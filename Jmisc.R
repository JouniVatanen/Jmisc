# Load required packages
if (!require("checkpoint")) install.packages("checkpoint")

# Use the date few weeks after R-3.6.1. was released
checkpoint("2019-07-28", R.version = "3.6.1",
           checkpointLocation = Sys.getenv("USERPROFILE"))
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


