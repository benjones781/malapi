## Introduction

This package accesses the [MyAnimeList API (beta ver.)
(2)](https://myanimelist.net/apiconfig/references/api/v2). It queries
myanimelist.net and returns anime/manga search results, details, and
rankings. It can also query information on seasonal animes. Currently
this package only accesses public information and does not require user
login. However, the MAL API still requires a MAL Client ID for usage.
You can learn how to obtain one
[here](https://myanimelist.net/blog.php?eid=835707).

``` r
devtools::install_github("benjones781/malapi")
```

## Examples

Results are returned as a list.

``` r
malapi::get_anime_details(anime_id = "11061",
                            fields = c("all"),
                         client_id = "yourclientid")

malapi::get_manga_details(manga_id = "51",
                            fields = c("authors{first_name,last_name}", "num_volumes"),
                         client_id = "yourclientid")

malapi::get_anime_ranking(ranking_type = "airing",
                                 limit = 10,
                                offset = 0,
                                fields = c("start_date", "synopsis", "mean"),
                             client_id = "yourclientid")

malapi::get_anime_search(search_term = "one",
                              fields = c("status", "genres"),
                           client_id = "yourclientid")

malapi::get_seasonal_anime(year = "2022",
                         season = "winter",
                           sort = "anime_score",
                          limit = 10,
                         fields = c("end_date", "popularity"),
                      client_id = "yourclientid")
```

## Parameters

The anime_id and manga_id values are most easily found in their url. For
example, [Hunter x Hunter
(2011)](https://myanimelist.net/anime/11061/Hunter_x_Hunter_2011) has
the id of 11061. <br/> <br/> Some parameters only accept certain string
values. These include: fields, ranking_type, season, and sort. A list of
all valid values can be found in each function’s documentation
(?get_anime_details) as well as the official [MyAnimeList API
site](https://myanimelist.net/apiconfig/references/api/v2).
