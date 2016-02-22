# Lucas Audette @LunoA
# INFO 498f

library(plotly)
library(dplyr)

# Graph perspective student by operating system and programming experience

graph_2 <- function(df) {
  # rename long column names
  df <- df %>%
        rename(class = What.is.your.current.class.standing., 
               exp = What.is.your.programming.experience.,
               os = What.operating.system.do.you.typically.use.,
               major = Are.you.interested.in.applying.to.the.Informatics.major.) %>%
        filter(major == "Yes" || major == "Maybe")
  
  # Creates df of mac users by class
  mac <- filter(df, grepl("Mac", os)) %>% 
         num() %>% 
         rename(mac = n)
  
  # Creates df of windows users by class
  windows <- filter(df, grepl("Windows", os)) %>% 
             num() %>% 
             rename(windows = n)
  
  # Creates df of linux users by class
  linux <- filter(df, grepl("Linux", os)) %>% 
           num() %>% 
           rename(linux = n)
  class_total <- num(df) %>% rename(total = n)
  
  # Creates df of all students by class and os
  yaxis <- c("Freshman", "Sophomore", "Junior", "Senior")
  result <- data_frame(class = yaxis)
  result <- left_join(result, mac, by = c("class" = "class"))
  result <- left_join(result, windows, by = c("class" = "class"))
  result <- left_join(result, linux, by = c("class" = "class"))
  result <- left_join(result, class_total, by = c("class" = "class"))
  
  # Creates graph object and adds mac trace
  p <- plot_ly(result,
               x = mac, 
               y = class,
               name = "Mac",
               mode = "markers", 
               marker = list(symbol = "x-dot")) %>%
    
  # Creates windows trace
  add_trace(x = windows, 
            name = "Windows", 
            marker = list(symbol = "diamond-dot")) %>%
    
  # Creates linux trace
  add_trace(x = linux, 
            name = "Linux", 
            marker = list(symbol = "star-dot")) %>%
  
  add_trace(x = total, 
            name = "Class Total", 
            marker = list(symbol = "triangle-dot")) %>%
  
  # Labels
  layout(title = "Perspective Informatics Students by Class and Operating System",
         xaxis = list(title = "# of Informatics Applicants"),
         yaxis = list(title = "Class"),
         hovermode = "closest")
  
  return(p)
}

# returns the number of rows in a df by class
# summarised column name always "n"
num <- function(df) {
  df %>%
  group_by(class) %>%
  summarise(n = n()) %>%
  return()
}

