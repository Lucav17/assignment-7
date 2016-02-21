library(dplyr)
library(plotly)

##Create a graph that compares students' decision to apply to Informatics to their school year

graph_1 <- function(dataset) {
  ##group by year, count applying decisions by school year in their own columns
  data <- dataset %>% group_by(What.is.your.current.class.standing.) %>% 
    summarise('Yes' = sum(Are.you.interested.in.applying.to.the.Informatics.major. == "Yes"), 
              'No' = sum(Are.you.interested.in.applying.to.the.Informatics.major. == "No"), 
              'Maybe' = sum(Are.you.interested.in.applying.to.the.Informatics.major. == "Maybe")) %>% 
    rename('Grade' = What.is.your.current.class.standing.)
  
  ##Create a bar graph that groups applying decisions by grade.
  yes_graph <- plot_ly(data,
    x = Grade,
    y = Yes,
    type = "bar",
    name = "Will Apply"
  )
  
  no_graph <- add_trace(
    yes_graph,
    x = Grade,
    y = No,
    type = "bar",
    name = "Will Not Apply"
  )
  
  maybe_graph <- add_trace(
    no_graph,
    x = Grade,
    y = Maybe,
    type = "bar",
    name = "Maybe Will Apply"
  ) %>% layout(xaxis = list(title = "School Year"),
               yaxis = list(title = "Number of People"),
               title = "Number of People Applying For Informatics Vs School Year")
  ##return the graph to be plotted
  return(maybe_graph)
}