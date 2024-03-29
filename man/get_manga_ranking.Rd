% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manga.R
\name{get_manga_ranking}
\alias{get_manga_ranking}
\title{Get Manga Ranking}
\usage{
get_manga_ranking(
  ranking_type = "all",
  limit = 100,
  offset = 0,
  fields = c("id", "title", "main_picture"),
  client_id
)
}
\arguments{
\item{ranking_type}{ranking category; acceptable values include: \cr
all, manga, novels, oneshots, doujin, manhwa, manhua, bypopularity, favorite \cr \cr
defaults to "all"}

\item{limit}{limits number of results; defaults to 100}

\item{offset}{number of results to skip; defaults to 0}

\item{fields}{list of desired fields to include;
acceptable values include: \cr
id, title, main_picture, alternative_titles, start_date, end_date, synopsis,
mean, rank, popularity, num_list_users, num_scoring_users, nsfw, created_at,
updated_at, media_type, status, genres, my_list_status, num_volumes, num_chapters,
authors\{first_name,last_name\}, pictures, background, related_anime, related_manga,
recommendations, serialization \cr \cr
use c("all") to include all fields; \cr
by default, "id", "title", and "main_picture" are always returned}

\item{client_id}{your MAL Client ID; \href{https://myanimelist.net/blog.php?eid=835707}{instructions to get a MAL Client ID}}
}
\description{
This function is like viewing the \href{https://myanimelist.net/topmanga.php}{Top Manga} section on myanimelist.net.
}
\examples{
get_manga_ranking(client_id = "your client id")

get_anime_ranking(ranking_type = "novels", fields = c("all"), client_id = "your client id")

get_anime_ranking(ranking_type = "doujin",
                         limit = 10,
                        offset = 0,
                        fields = c("start_date", "authors{first_name,last_name}"),
                     client_id = "your client id")

}
