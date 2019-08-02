# Load required packages
if (!require("checkpoint")) install.packages("checkpoint"); library(checkpoint)

# Use the date few weeks after R-3.6.1. was released
checkpoint("2019-07-28", R.version = "3.6.1",
           checkpointLocation = Sys.getenv("USERPROFILE"))
library(roxygen2)
library(devtools)

# Create package documentation
document()

# Install package
setwd("..")
install("jmisc")
setwd("./jmisc")

# Commit changes and push the files to the github
# 1. Commit changes shell "git add .;git commit -m 'comment'" OR Rstudio UI
# 2. Push changes either shell "git push" OR Rstudio UI
# 3. Install from github with devtools::install_github(JouniVatanen/Jmisc)
