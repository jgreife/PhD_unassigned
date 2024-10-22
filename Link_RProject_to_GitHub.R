library(usethis) #Library for GitHub connection to R
use_git() #creates a repo but doesn't push it to GH
#create_github_token() #only needs to be done once per expiration period (60 days)
install.packages("gitcreds") #if not installed
library(gitcreds) #loadtouse
gitcreds_set() #set credentials once per month, password = token!

use_github() #link project to GH repo


# --> set to private on GH