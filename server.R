popular_movies <- readRDS("popular_movies.rds")
R_normalized <- readRDS("R_normalized.rds")
S_top_30 <- readRDS("S_top_30.rds")
source("IBCF.R")

get_user_ratings = function(value_list) {
  dat = data.table(
    MovieID = sapply(strsplit(names(value_list), "_"), 
                     function(x) ifelse(length(x) > 1, x[[2]], NA)),
    Rating = unlist(as.character(value_list))
  )
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
  return(dat)
}

myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))


shinyServer(function(input, output, session) {
  
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", 
                     ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))
        )
      })))
    })
  })
  
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { 

      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)

      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list) 

      new_user_vector <- rep(NA, ncol(R_normalized))
      names(new_user_vector) <- colnames(R_normalized)  

      for (row_idx in seq_len(nrow(user_ratings))) {
        mid <- paste0("m", user_ratings$MovieID[row_idx])
        if (mid %in% names(new_user_vector)) {
          new_user_vector[mid] <- user_ratings$Rating[row_idx]
        }
      }
      
      top_movies <- myIBCF(new_user_vector, S_top_30, R_normalized, top_n = 10)
      
      predicted_ratings <- numeric(length(top_movies))
      for (i in seq_along(top_movies)) {
        movie_index <- which(colnames(R_normalized) == top_movies[i])
        similar_movies <- which(!is.na(S_top_30[movie_index, ]) & !is.na(new_user_vector))
        if (length(similar_movies) > 0) {
          weights <- S_top_30[movie_index, similar_movies]
          ratings <- new_user_vector[similar_movies]
          predicted_ratings[i] <- sum(weights * ratings, na.rm = TRUE) / sum(weights, na.rm = TRUE)
        }
      }
      
      top_movie_ids_str <- sub("^m", "", top_movies)  
      top_movie_ids <- as.integer(top_movie_ids_str)
      
      recom_results <- data.table(
        Rank = 1:length(top_movies), 
        MovieID = top_movie_ids, 
        Title = movies$Title[match(top_movie_ids, movies$MovieID)], 
        Predicted_rating = predicted_ratings
      )
      
      return(recom_results)
    }) # end withBusyIndicatorServer
  }) # end eventReactive
  
  
  output$results <- renderUI({
    recom_result <- df()
    num_rows <- 2
    num_movies <- 5
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, 
            title = paste0("Rank ", (i - 1) * num_movies + j),
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]]))
        )        
      })))
    })
  })
  
}) # end shinyServer
