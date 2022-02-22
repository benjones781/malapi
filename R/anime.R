generate_anime_search_url <- function(search_term, limit, offset, fields) {


  url <- paste("https://api.myanimelist.net/v2/anime?q=",
               gsub(" ", "%20", search_term),
               "&limit=",
               limit,
               "&offset=",
               offset,
               sep="")
  if (fields[1] == "all") {
    url <- paste(url,
                 "&fields=id,title,main_picture,alternative_titles,start_date,end_date,synopsis,mean,rank,popularity,num_list_users,num_scoring_users,nsfw,created_at,updated_at,media_type,status,genres,my_list_status,num_episodes,start_season,broadcast,source,average_episode_duration,rating,pictures,background,related_anime,related_manga,recommendations,studios,statistics",
                 sep=""
    )
  } else {
    selected_fields <- "&fields="
    for (i in fields) {
      selected_fields <- paste(selected_fields, i, sep=",")
    }
    url <- paste(url, selected_fields, sep="")
  }

  return(url)
}

generate_anime_details_url <- function(anime_id, fields) {

  url <- paste("https://api.myanimelist.net/v2/anime/",
               anime_id,
               sep="")
  if (fields[1] == "all") {
    url <- paste(url,
                 "?fields=id,title,main_picture,alternative_titles,start_date,end_date,synopsis,mean,rank,popularity,num_list_users,num_scoring_users,nsfw,created_at,updated_at,media_type,status,genres,my_list_status,num_episodes,start_season,broadcast,source,average_episode_duration,rating,pictures,background,related_anime,related_manga,recommendations,studios,statistics",
                 sep=""
    )
  } else {
    selected_fields <- "?fields="
    for (i in fields) {
      selected_fields <- paste(selected_fields, i, sep=",")
    }
    url <- paste(url, selected_fields, sep="")
  }

  return(url)
}

generate_anime_ranking_url <- function(ranking_type, limit, offset, fields) {
  url <- paste("https://api.myanimelist.net/v2/anime/ranking?ranking_type=",
               ranking_type,
               "&limit=",
               limit,
               "&offset=",
               offset,
               sep="")

  if (fields[1] == "all") {
    url <- paste(url,
                 "&fields=id,title,main_picture,alternative_titles,start_date,end_date,synopsis,mean,rank,popularity,num_list_users,num_scoring_users,nsfw,created_at,updated_at,media_type,status,genres,my_list_status,num_episodes,start_season,broadcast,source,average_episode_duration,rating,pictures,background,related_anime,related_manga,recommendations,studios,statistics",
                 sep=""
    )
  } else {
    selected_fields <- "&fields="
    for (i in fields) {
      selected_fields <- paste(selected_fields, i, sep=",")
    }
    url <- paste(url, selected_fields, sep="")
  }

  return(url)
}

generate_anime_seasonal_url <- function(year, season, sort, limit, offset, fields) {
  url <- paste("https://api.myanimelist.net/v2/anime/season/",
               year,
               "/",
               season,
               "?sort=",
               sort,
               "&limit=",
               limit,
               "&offset=",
               offset,
               sep="")

  if (fields[1] == "all") {
    url <- paste(url,
                 "&fields=id,title,main_picture,alternative_titles,start_date,end_date,synopsis,mean,rank,popularity,num_list_users,num_scoring_users,nsfw,created_at,updated_at,media_type,status,genres,my_list_status,num_episodes,start_season,broadcast,source,average_episode_duration,rating,pictures,background,related_anime,related_manga,recommendations,studios,statistics",
                 sep=""
    )
  } else {
    selected_fields <- "&fields="
    for (i in fields) {
      selected_fields <- paste(selected_fields, i, sep=",")
    }
    url <- paste(url, selected_fields, sep="")
  }

  return(url)
}

get_current_season <- function() {
  season_lookup <- c("01"="winter", "02"="winter", "03"="winter",
                     "04"="spring", "05"="spring", "06"="spring",
                     "07"="summer", "08"="summer", "09"="summer",
                     "10"="fall", "11"="fall", "12"="fall")
  return(season_lookup[format(Sys.Date(), "%m")])
}


#' Search for Anime
#'
#' This function is like using the search bar for anime on myanimelist.net.
#' @param search_term search anime by this term
#' @param limit limits number of results; defaults to 100
#' @param offset number of results to skip; defaults to 0
#' @param fields list of desired fields to include;
#' acceptable values include: \cr
#' id, title,main_picture, alternative_titles, start_date,
#' end_date, synopsis, mean, rank, popularity, num_list_users, num_scoring_users,
#' nsfw, created_at, updated_at, media_type, status, genres, my_list_status,
#' num_episodes, start_season, broadcast, source, average_episode_duration, rating,
#' pictures, background, related_anime, related_manga, recommendations, studios, statistics \cr \cr
#' use c("all") to include all fields; \cr
#' by default, "id", "title", and "main_picture" are always returned
#' @param client_id your MAL Client ID; \href{https://myanimelist.net/blog.php?eid=835707}{instructions to get a MAL Client ID}
#' @export
#' @examples
#' get_anime_search(search_term = "one punch", client_id = "your client id")
#'
#' get_anime_search(search_term = "one punch", fields = c("all"), client_id = "your client id")
#'
#' get_anime_search(search_term = "one punch",
#'                        limit = 10,
#'                       offset = 0,
#'                       fields = c("mean", "genres"),
#'                    client_id = "your client id")

get_anime_search <- function(search_term,
                             limit=100,
                             offset=0,
                             fields=c("id", "title", "main_picture"),
                             client_id){
  response <- httr::GET(url = generate_anime_search_url(search_term, limit, offset, fields), httr::add_headers("X-MAL-CLIENT-ID"=client_id))
  response_text <- httr::content(response, as="text")
  return(jsonlite::fromJSON(response_text))
}


#' Get Anime Details
#'
#' This function is like viewing an anime's page on myanimelist.net.
#' The ID can be obtained from either using the \code{\link{get_anime_search}} function or the url
#' (for example, the ID of \href{https://myanimelist.net/anime/11061/Hunter_x_Hunter_2011}{Hunter x Hunter (2011)} is 11061).
#' @param anime_id anime ID as a string
#' @param fields list of desired fields to include;
#' acceptable values include: \cr
#' id, title,main_picture, alternative_titles, start_date,
#' end_date, synopsis, mean, rank, popularity, num_list_users, num_scoring_users,
#' nsfw, created_at, updated_at, media_type, status, genres, my_list_status,
#' num_episodes, start_season, broadcast, source, average_episode_duration, rating,
#' pictures, background, related_anime, related_manga, recommendations, studios, statistics \cr \cr
#' use c("all") to include all fields; \cr
#' by default, "id", "title", and "main_picture" are always returned
#' @param client_id your MAL Client ID; \href{https://myanimelist.net/blog.php?eid=835707}{instructions to get a MAL Client ID}
#' @export
#' @examples
#' get_anime_details(anime_id = "11061", client_id = "your client id")
#'
#' get_anime_details(anime_id = "11061", fields = c("all"), client_id = "your client id")
#'
#' get_anime_details(anime_id = "11061",
#'                     fields = c("related_anime", "num_episodes"),
#'                  client_id = "your client id")

get_anime_details <- function(anime_id,
                              fields=c("id", "title", "main_picture"),
                              client_id){
  response <- httr::GET(url = generate_anime_details_url(anime_id, fields), httr::add_headers("X-MAL-CLIENT-ID"=client_id))
  response_text <- httr::content(response, as="text")
  return(jsonlite::fromJSON(response_text))
}


#' Get Anime Ranking
#'
#' This function is like viewing the \href{https://myanimelist.net/topanime.php}{Top Anime} section on myanimelist.net.
#' @param ranking_type ranking category; acceptable values include: \cr
#' all, airing, upcoming, tv, ova, movie, special, bypopularity, favorite \cr \cr
#' defaults to "all"
#' @param limit limits number of results; defaults to 100
#' @param offset number of results to skip; defaults to 0
#' @param fields list of desired fields to include;
#' acceptable values include: \cr
#' id, title,main_picture, alternative_titles, start_date,
#' end_date, synopsis, mean, rank, popularity, num_list_users, num_scoring_users,
#' nsfw, created_at, updated_at, media_type, status, genres, my_list_status,
#' num_episodes, start_season, broadcast, source, average_episode_duration, rating,
#' pictures, background, related_anime, related_manga, recommendations, studios, statistics \cr \cr
#' use c("all") to include all fields; \cr
#' by default, "id", "title", and "main_picture" are always returned
#' @param client_id your MAL Client ID; \href{https://myanimelist.net/blog.php?eid=835707}{instructions to get a MAL Client ID}
#' @export
#' @examples
#' get_anime_ranking(client_id = "your client id")
#'
#' get_anime_ranking(ranking_type = "airing", fields = c("all"), client_id = "your client id")
#'
#' get_anime_ranking(ranking_type = "upcoming",
#'                          limit = 10,
#'                         offset = 0,
#'                         fields = c("synopsis", "genres"),
#'                      client_id = "your client id")

get_anime_ranking <- function(ranking_type="all",
                              limit=100,
                              offset=0,
                              fields=c("id", "title", "main_picture"),
                              client_id) {
  response <- httr::GET(url = generate_anime_ranking_url(ranking_type, limit, offset, fields), httr::add_headers("X-MAL-CLIENT-ID"=client_id))
  response_text <- httr::content(response, as="text")
  return(jsonlite::fromJSON(response_text))
}


#' Get Seasonal Anime
#'
#' This function is like viewing the \href{https://myanimelist.net/anime/season}{Seasonal Anime} section on myanimelist.net.
#' @param year desired year as string or numeric; \cr
#' defaults to the current year
#' @param season desired season as string; acceptable values include: winter, spring, summer, fall \cr
#' defaults to the current season
#' @param sort how to order results (descending); acceptable values include: anime_score, anime_num_list_users \cr
#' defaults to "anime_score"
#' @param limit limits number of results; defaults to 100
#' @param offset number of results to skip; defaults to 0
#' @param fields list of desired fields to include;
#' acceptable values include: \cr
#' id, title,main_picture, alternative_titles, start_date,
#' end_date, synopsis, mean, rank, popularity, num_list_users, num_scoring_users,
#' nsfw, created_at, updated_at, media_type, status, genres, my_list_status,
#' num_episodes, start_season, broadcast, source, average_episode_duration, rating,
#' pictures, background, related_anime, related_manga, recommendations, studios, statistics \cr \cr
#' use c("all") to include all fields; \cr
#' by default, "id", "title", and "main_picture" are always returned
#' @param client_id your MAL Client ID; \href{https://myanimelist.net/blog.php?eid=835707}{instructions to get a MAL Client ID}
#' @export
#' @examples
#' get_seasonal_anime(client_id = "your client id")
#'
#' get_seasonal_anime(year = "2022",
#'                  season = "winter",
#'                  fields = c("all"),
#'               client_id = "your client id")
#'
##' get_seasonal_anime(year = "2022",
#'                   season = "winter",
#'                     sort = "anime_num_list_users",
#'                    limit = 10,
#'                   offset = 0,
#'                   fields = c("all"),
#'                client_id = "your client id")


get_seasonal_anime <- function(year=format(Sys.Date(), "%Y"),
                               season=get_current_season(),
                               sort="anime_score",
                               limit=100,
                               offset=0,
                               fields=c("id", "title", "main_picture"),
                               client_id) {
  response <- httr::GET(url = generate_anime_seasonal_url(year, season, sort, limit, offset, fields), httr::add_headers("X-MAL-CLIENT-ID"=client_id))
  response_text <- httr::content(response, as="text")
  return(jsonlite::fromJSON(response_text))
}
