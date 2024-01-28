previous_data <- read.csv("data/peloton_data.csv")

new_count <- total_classes - nrow(previous_data)


if(new_count>0){
  workouts <- get_recent_workouts(new_count)
  
  performance<- data.frame()
  for(i in 1:nrow(workouts)){
    performance = rbind(performance, get_performance_metrics(workouts$id[i], workouts$fitness_discipline[i]))
  }
  #combine workout with performance
  new_data <- workouts|>left_join(performance, by="id")
  #combine updated data with previous data
  peloton_data<- rbind(new_data, previous_data)
}


write.csv(peloton_data, "data/peloton_data.csv", row.names=F)
