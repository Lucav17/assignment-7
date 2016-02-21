summary_info <- function(data) {
  summary <- list()
  # Number of people surveyed
  summary$num_surveyed <- nrow(data)
  # How many were interested in the INFO major
  summary$num_interested_in_info <- sum(data$Are.you.interested.in.applying.to.the.Informatics.major. == "Yes")
  # Number using Windows
  summary$num_windows <- sum(data$What.operating.system.do.you.typically.use. == "Windows")
  # Number using Mac
  summary$num_mac <- sum(data$What.operating.system.do.you.typically.use. == "Mac")
  # Number using Linux
  summary$num_linux <- sum(data$What.operating.system.do.you.typically.use. == "Linux")
  # Number who have never used R before this class
  summary$num_never_used_R <- sum(data$What.is.your.familiarity.with..Using.the.R.programming.language == "Never used it")
  # Average number of countries people have been to
  summary$avg_number_countries <- mean(data$How.many.countries.have.you.visited.in.your.life.)
  # Number of dog people
  summary$num_dog_people <- sum(data$Do.you.consider.yourself. == "A dog person...") +
                            sum(data$Do.you.consider.yourself. == "Both!")
  # Number of cat people
  summary$num_cat_people <- sum(data$Do.you.consider.yourself. == "A cat person....") +
                            sum(data$Do.you.consider.yourself. == "Both!")
  # Number of Seahawks fans
  summary$num_seahawks_fans <- sum(data$Are.you.a.Seahawks.fan. == "Yes") +
                               sum(data$Are.you.a.Seahawks.fan. == "YES!")
  
  return(summary)
}