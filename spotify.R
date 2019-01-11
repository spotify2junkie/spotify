library(Rspotify)
library(httr)
library(jsonlite)
require(plyr)
library(glmnet)
spotifyEndpoint <- oauth_endpoint(NULL, "https://accounts.spotify.com/authorize", "https://accounts.spotify.com/api/token")
key = spotifyOAuth("yuhuiluo_datascience",client_id="812930b951c6479c8f59802d938a65ef",client_secret="1d73fa8b97454881b7ba43dddad2bcc6")



getAlbums_id=function(id,type="album",market="US",token){
  #input: "ids" for specific artist 
  #output: list of albums_id under artist
  require(dplyr)
  total<-httr::content(httr::GET(paste0("https://api.spotify.com/v1/artists/",id,"/albums?album_type=", type),
                                 httr::config(token = token)))$total
  req<-httr::GET(paste0("https://api.spotify.com/v1/artists/",id,
                        "/albums?offset=0&limit=",total,
                        "&album_type=",type,"&market=",market),
                 httr::config(token = token))
  return(bind_rows(lapply(httr::content(req)$items,
                          function(x) data.frame(
                            id = x$id,
                            stringsAsFactors = F))))
}

getFeatures_up<-function(spotify_ID,token){
  #
  #input is the id for spotify id,
  #token is the authorization key,
  #
  #output is the list of all 12 attriutes of a specific song 
  # end 
  
  req <- httr::GET(paste0("https://api.spotify.com/v1/audio-features/",spotify_ID), httr::config(token = token))
  json1<-httr::content(req)
  dados=data.frame(id=as.character(json1$id),
                   danceability=as.numeric(json1$danceability),
                   energy=as.numeric(json1$energy),
                   key=as.numeric(json1$key),
                   loudness=as.numeric(json1$loudness),
                   mode=as.numeric(json1$mode),
                   speechiness=as.numeric(json1$speechiness),
                   acousticness=as.numeric(json1$acousticness),
                   instrumentalness=as.numeric(json1$instrumentalness),
                   liveness=as.numeric(json1$liveness),
                   valence=as.numeric(json1$valence),
                   tempo=as.numeric(json1$tempo),
                   stringsAsFactors = F)
  d=unlist(dados[1,], use.names=FALSE)
  return(d)
}
getAlbum<-function(id,token){
  req<-httr::GET(paste0("https://api.spotify.com/v1/albums/",id,"/tracks"),httr::config(token = token))
  json1<-httr::content(req)
  json2<-jsonlite::fromJSON(jsonlite::toJSON(json1))$items
  json3=data.frame(id=unlist(json2$id),
                   name=unlist(json2$name))
  return(json3)
}

songs_albums_features=function(a){ 
  # input is the album_id
  #output all the stats of songs within a specific album
  
  album_info=data.frame(getAlbumInfo(a,key))
  album_artist=album_info$artist
  album_date=album_info$release_date
  #all the stats of songs in specific album 
  albums_frame=data.frame(getAlbum(a,key))  #this is the source of the problem
  tracks_ids=albums_frame$id
  tracks_matrix=matrix(0,nrow=length(tracks_ids),ncol=14)
  for(track in 1:length(tracks_ids)){
    tracks_matrix[track,]=c(getFeatures_up(tracks_ids[track],key),
                            as.character(album_date),
                            as.character(album_artist)
    )
  }
  tracks_all_info=data.frame(tracks_matrix)
  colnames(tracks_all_info)=c("id","danceability","energy","key","loudness",
                              "mode","speechiness","acousticness",
                              "instrumentalness", "liveness","valence",
                              "tempo","release_date","artist_name")
  tracks_all_info$track_name=albums_frame$name
  return(tracks_all_info)
  # returned a data-frame
}
combine_all_songs=function(ids){
  stats=list()
  for (i in 1:length(ids)){
    stats[i]=songs_albums_features(ids[i],key)
  }
}
get_all_works=function(artist_id){
  all_work=rbind.fill(lapply(getAlbums_id(artist_id,token=key)[,1],songs_albums_features)
  )
  all_work$id=as.character(all_work$id)
  all_work$danceability=as.numeric(all_work$danceability)
  all_work$energy=as.numeric(all_work$energy)
  all_work$key=as.numeric(all_work$key)
  all_work$loudness=as.numeric(all_work$loudness)
  all_work$mode=as.numeric(all_work$mode)
  all_work$speechiness=as.numeric(all_work$speechiness)
  all_work$acousticness=as.numeric(all_work$acousticness)
  all_work$instrumentalness=as.numeric(all_work$instrumentalness)
  all_work$liveness=as.numeric(all_work$liveness)
  all_work$valence=as.numeric(all_work$valence)
  all_work$tempo=as.numeric(all_work$tempo)
  all_work$release_date=as.character(all_work$release_date)
  all_work$artist_name=as.character(all_work$artist_name)
  all_work$track_name=as.character(all_work$track_name)
  return(all_work)
}


