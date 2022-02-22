generate_manga_search_url <- function(search_term, limit, offset, fields) {


  url <- paste("https://api.myanimelist.net/v2/manga?q=",
               gsub(" ", "%20", search_term),
               "&limit=",
               limit,
               "&offset=",
               offset,
               sep="")
  if (fields[1] == "all") {
    url <- paste(url,
                 "&fields=id,title,main_picture,alternative_titles,start_date,end_date,synopsis,mean,rank,popularity,num_list_users,num_scoring_users,nsfw,created_at,updated_at,media_type,status,genres,my_list_status,num_volumes,num_chapters,authors{first_name,last_name},pictures,background,related_anime,related_manga,recommendations,serialization",
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

generate_manga_details_url <- function(manga_id, fields) {

  url <- paste("https://api.myanimelist.net/v2/manga/",
               manga_id,
               sep="")
  if (fields[1] == "all") {
    url <- paste(url,
                 "?fields=id,title,main_picture,alternative_titles,start_date,end_date,synopsis,mean,rank,popularity,num_list_users,num_scoring_users,nsfw,created_at,updated_at,media_type,status,genres,my_list_status,num_volumes,num_chapters,authors{first_name,last_name},pictures,background,related_anime,related_manga,recommendations,serialization",
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

generate_manga_ranking_url <- function(ranking_type, limit, offset, fields) {
  url <- paste("https://api.myanimelist.net/v2/manga/ranking?ranking_type=",
               ranking_type,
               "&limit=",
               limit,
               "&offset=",
               offset,
               sep="")

  if (fields[1] == "all") {
    url <- paste(url,
                 "&fields=id,title,main_picture,alternative_titles,start_date,end_date,synopsis,mean,rank,popularity,num_list_users,num_scoring_users,nsfw,created_at,updated_at,media_type,status,genres,my_list_status,num_volumes,num_chapters,authors{first_name,last_name},pictures,background,related_anime,related_manga,recommendations,serialization",
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


#' Search for Manga
#'
#' This function is like using the search bar for manga on myanimelist.net.
#' @param search_term search manga by this term
#' @param limit limits number of results; defaults to 100
#' @param offset number of results to skip; defaults to 0
#' @param fields list of desired fields to include;
#' acceptable values include: \cr
#' id, title, main_picture, alternative_titles, start_date, end_date, synopsis,
#' mean, rank, popularity, num_list_users, num_scoring_users, nsfw, created_at,
#' updated_at, media_type, status, genres, my_list_status, num_volumes, num_chapters,
#' authors\{first_name,last_name\}, pictures, background, related_anime, related_manga,
#' recommendations, serialization \cr \cr
#' use c("all") to include all fields; \cr
#' by default, "id", "title", and "main_picture" are always returned
#' @param client_id your MAL Client ID; \href{https://myanimelist.net/blog.php?eid=835707}{instructions to get a MAL Client ID}
#' @export
#' @examples
#' get_manga_search(search_term = "dragon ball", client_id = "your client id")
#'
#' get_manga_search(search_term = "dragon ball", fields = c("all"), client_id = "your client id")
#'
#' get_manga_search(search_term = "dragon ball",
#'                        limit = 10,
#'                       offset = 0,
#'                       fields = c("popularity", "authors{first_name,last_name}"),
#'                    client_id = "your client id")
#'
get_manga_search <- function(search_term,
                             limit=100,
                             offset=0,
                             fields=c("id", "title", "main_picture"),
                             client_id){
  response <- httr::GET(url = generate_manga_search_url(search_term, limit, offset, fields), httr::add_headers("X-MAL-CLIENT-ID"=client_id))
  response_text <- httr::content(response, as="text")
  return(jsonlite::fromJSON(response_text))
}


#' Get Manga Details
#'
#' This function is like viewing a manga's page on myanimelist.net.
#' The ID can be obtained from either using the \code{\link{get_manga_search}} function or the url
#' (for example, the ID of \href{https://myanimelist.net/manga/51/Slam_Dunk}{Slam Dunk} is 51).
#' @param manga_id manga ID as a string
#' @param fields list of desired fields to include;
#' acceptable values include: \cr
#' id, title, main_picture, alternative_titles, start_date, end_date, synopsis,
#' mean, rank, popularity, num_list_users, num_scoring_users, nsfw, created_at,
#' updated_at, media_type, status, genres, my_list_status, num_volumes, num_chapters,
#' authors\{first_name,last_name\}, pictures, background, related_anime, related_manga,
#' recommendations, serialization \cr \cr
#' use c("all") to include all fields; \cr
#' by default, "id", "title", and "main_picture" are always returned
#' @param client_id your MAL Client ID; \href{https://myanimelist.net/blog.php?eid=835707}{instructions to get a MAL Client ID}
#' @export
#' @examples
#' get_manga_details(anime_id = "51", client_id = "your client id")
#'
#' get_manga_details(anime_id = "51", fields = c("all"), client_id = "your client id")
#'
#' get_manga_details(anime_id = "51",
#'                     fields = c("synopsis", "authors{first_name,last_name}"),
#'                  client_id = "your client id")
#'
get_manga_details <- function(manga_id,
                              fields=c("id", "title", "main_picture"),
                              client_id){
  response <- httr::GET(url = generate_manga_details_url(manga_id, fields), httr::add_headers("X-MAL-CLIENT-ID"=client_id))
  response_text <- httr::content(response, as="text")
  return(jsonlite::fromJSON(response_text))
}


#' Get Manga Ranking
#'
#' This function is like viewing the \href{https://myanimelist.net/topmanga.php}{Top Manga} section on myanimelist.net.
#' @param ranking_type ranking category; acceptable values include: \cr
#' all, manga, novels, oneshots, doujin, manhwa, manhua, bypopularity, favorite \cr \cr
#' defaults to "all"
#' @param limit limits number of results; defaults to 100
#' @param offset number of results to skip; defaults to 0
#' @param fields list of desired fields to include;
#' acceptable values include: \cr
#' id, title, main_picture, alternative_titles, start_date, end_date, synopsis,
#' mean, rank, popularity, num_list_users, num_scoring_users, nsfw, created_at,
#' updated_at, media_type, status, genres, my_list_status, num_volumes, num_chapters,
#' authors\{first_name,last_name\}, pictures, background, related_anime, related_manga,
#' recommendations, serialization \cr \cr
#' use c("all") to include all fields; \cr
#' by default, "id", "title", and "main_picture" are always returned
#' @param client_id your MAL Client ID; \href{https://myanimelist.net/blog.php?eid=835707}{instructions to get a MAL Client ID}
#' @export
#' @examples
#' get_manga_ranking(client_id = "your client id")
#'
#' get_anime_ranking(ranking_type = "novels", fields = c("all"), client_id = "your client id")
#'
#' get_anime_ranking(ranking_type = "doujin",
#'                          limit = 10,
#'                         offset = 0,
#'                         fields = c("start_date", "authors{first_name,last_name}"),
#'                      client_id = "your client id")
#'
get_manga_ranking <- function(ranking_type="all",
                              limit=100,
                              offset=0,
                              fields=c("id", "title", "main_picture"),
                              client_id) {
  response <- httr::GET(url = generate_manga_ranking_url(ranking_type, limit, offset, fields), httr::add_headers("X-MAL-CLIENT-ID"=client_id))
  response_text <- httr::content(response, as="text")
  return(jsonlite::fromJSON(response_text))
}
