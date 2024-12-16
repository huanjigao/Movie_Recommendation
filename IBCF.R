myIBCF <- function(new_user, S, R, top_n = 10) {
  predictions <- numeric(ncol(R))
  for (i in which(is.na(new_user))) { 
    similar_movies <- which(!is.na(S[i, ]) & !is.na(new_user))
    if (length(similar_movies) > 0) {
      weights <- S[i, similar_movies]
      ratings <- new_user[similar_movies]
      predictions[i] <- sum(weights * ratings, na.rm = TRUE) / sum(weights, na.rm = TRUE)
    }
  }
  
  rate <- which(!is.na(predictions))
  if (length(rate) < top_n) {
    already_rated <- which(!is.na(new_user))
    remaining_popular <- setdiff(popular_movies, colnames(R)[already_rated])
    predicted_movies <- colnames(R)[rate[order(predictions[rate], decreasing = TRUE)]]
    top_movies <- unique(c(predicted_movies, remaining_popular))[1:top_n]
  } else {
    top_indices <- rate[order(predictions[rate], decreasing = TRUE)[1:top_n]]
    top_movies <- colnames(R)[top_indices]
  }
  return(top_movies)
}
