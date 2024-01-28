library(tidyverse)
library(pelotonR)
library(httr)
library(jsonlite)
library(glue)

#devtools::install_github("lgellis/pelotonR")

username <- Sys.getenv("API_USERNAME")
password <- Sys.getenv("API_PASSWORD")

base_api = "https://api.onepeloton.com/api"

auth_response <-pelotonR::authenticate(username,password)

instructors<-  pelotonR::get_instructors_df()

get_user_id<-function(){
  request = httr::GET(paste0(base_api,"/me"))
  jsonlite::fromJSON(rawToChar(request$content))$id
}

get_instructors<-function(){
  request = httr::GET(glue("{base_api}/instructor"))
  jsonlite::fromJSON(rawToChar(request$content))$data
}

get_page_workouts<-function(page){
  request = httr::GET(glue("{base_api}/user/{user_id}/workouts?page={page}&joins=peloton.ride"))
  data = jsonlite::fromJSON(rawToChar(request$content))$data
  
  data|>
    select(id, created_at, device_type, peloton, status, timezone, total_work)|>
    unnest(peloton)|>
    unnest(ride, names_sep = "_")|>
    select(id, ride_id, created_at, timezone, device_type, ride_fitness_discipline_display_name, ride_title, status, total_work, 
           ride_difficulty_rating_avg, ride_duration, ride_image_url, ride_instructor_id)|>
    rename(avg_difficulty = ride_difficulty_rating_avg,
           instructor_id = ride_instructor_id, 
           fitness_discipline = ride_fitness_discipline_display_name)|>
    left_join(instructors|>select(id, name)|>rename(instructor_name=name), by=c("instructor_id"="id"))|>
    mutate(created_at = as.POSIXct(created_at, origin = "1970-01-01"))
  
  
}

get_recent_workouts<-function(last_n=50){
  pages = ceiling(last_n/20)-1
  workouts = data.frame()
  for(i in 0:pages){
    workouts = rbind(workouts, get_page_workouts(i))
  }
  workouts|>head(last_n)
}




get_performance_metrics <-function(workout_id, class_type){
  #basic details
  base_url= 'https://api.onepeloton.com/api/'
  
  result =   data.frame(
    id = workout_id, 
    distance = NA,
    total_output = NA,
    avg_pace=NA,
    avg_speed = NA,
    avg_output = NA,
    avg_cadence = NA,
    avg_resistance = NA
  )
  
  
  if(tolower(class_type) %in% c("cycling","bike_bootcamp","bike bootcamp", "tread bootcamp", "running","walking","circuit")){
    perf= GET(paste0(base_url,'workout/',workout_id,"/performance_graph"))
    perf_data<-fromJSON(rawToChar(perf$content))
    if("summaries" %in% names(perf_data)){
      totals<-perf_data$summaries|>select(slug, value)|>spread(slug,value)
      if("distance" %in% names(totals)){
        result$distance = totals$distance
      }
      if("total_output" %in% names(totals)){
        result$total_output = totals$total_output
      }
    }
    if("average_summaries" %in% names(perf_data) & length(perf_data$average_summaries)!=0){
      avg<-perf_data$average_summaries|>select(slug, value)|>spread(slug,value)
      if("avg_speed" %in% names(avg)){
        result$avg_speed = avg$avg_speed
      }
      if("avg_output" %in% names(avg)){
        result$avg_output = avg$avg_output
      }
      if("avg_pace" %in% names(avg)){
        result$avg_pace = avg$avg_pace
      }
      if("avg_cadence" %in% names(avg)){
        result$avg_cadence = avg$avg_cadence
      }
      if("avg_resistance" %in% names(avg)){
        result$avg_resistance = avg$avg_resistance
      }
    }
  }
  result
}


user_id <- get_user_id()

req_stats <- httr::GET(glue("{base_api}/user/{user_id}/workouts?joins=peloton.ride"))
my_stats <- jsonlite::fromJSON(rawToChar(req_stats$content))

total_classes <- my_stats$total 
#pages are 0 indexed
total_pages <- my_stats$page_count-1