rm(list=ls())
source("spotify.R")


# brockhampton albums
brock=get_all_works("1Bl6wpkWCQ4KVgnASpvzzA")

# #jcole albums
jcole=get_all_works("6l3HvQ5sa6mXTsMTB19rO5")
#
# #Tyler albums
tyler=get_all_works("4V8LLVI7PbaPR0K2TGSxFF")

# Drake 
amine=get_all_works("3Gm5F95VdRxW3mqCn8RPBJ")
#cluster analysis
my_favorite_rappers=rbind(brock,tyler,amine)

kend=get_all_works("2YZyLoL8N0Wb9xBt1NhZWg")
my_favorite_rappers=rbind(brock,jcole,tyler,amine,kend)

# To generate a density plot.
library(ggplot2)


p0=ggplot(my_favorite_rappers, aes(x=danceability, fill=artist_name)) +
  geom_density(alpha=0.4) +
  theme(plot.title = element_text(color="red", size=14, face="bold.italic"))

p1=ggplot(my_favorite_rappers, aes(x=energy, fill=artist_name)) +
  geom_density(alpha=0.4,show.legend = FALSE) +
  theme(plot.title = element_text(color="red", size=14, face="bold.italic"))

p2=ggplot(my_favorite_rappers, aes(x=key, fill=artist_name)) +
  geom_density(alpha=0.4,show.legend = FALSE) +
  theme(plot.title = element_text(color="red", size=14, face="bold.italic"))

p3=ggplot(my_favorite_rappers, aes(x=loudness, fill=artist_name)) +
  geom_density(alpha=0.4,show.legend = FALSE) +
  theme(plot.title = element_text(color="red", size=14, face="bold.italic"))

p4=ggplot(my_favorite_rappers, aes(x=mode, fill=artist_name)) +
  geom_density(alpha=0.4,show.legend = FALSE) +
  theme(plot.title = element_text(color="red", size=14, face="bold.italic"))

p5=ggplot(my_favorite_rappers, aes(x=speechiness, fill=artist_name)) +
  geom_density(alpha=0.4,show.legend = FALSE) +
  theme(plot.title = element_text(color="red", size=14, face="bold.italic"))

p6=ggplot(my_favorite_rappers, aes(x=acousticness, fill=artist_name)) +
  geom_density(alpha=0.4,show.legend = FALSE) +
  theme(plot.title = element_text(color="red", size=14, face="bold.italic"))

p7=ggplot(my_favorite_rappers, aes(x=instrumentalness, fill=artist_name)) +
  geom_density(alpha=0.4,show.legend = FALSE) +
  theme(plot.title = element_text(color="red", size=14, face="bold.italic"))

p8=ggplot(my_favorite_rappers, aes(x=liveness, fill=artist_name)) +
  geom_density(alpha=0.4,show.legend = FALSE) +
  theme(plot.title = element_text(color="red", size=14, face="bold.italic"))

p9=ggplot(my_favorite_rappers, aes(x=valence, fill=artist_name)) +
  geom_density(alpha=0.4,show.legend = FALSE) +
  theme(plot.title = element_text(color="red", size=14, face="bold.italic"))

p10=ggplot(my_favorite_rappers, aes(x=tempo, fill=artist_name)) +
  geom_density(alpha=0.4,show.legend = FALSE) +
  theme(plot.title = element_text(color="red", size=14, face="bold.italic"))

require(gridExtra)
grid.arrange(p0,p1,p2,p3,p4,p5,layout_matrix = rbind(c(0,1,2),c(3,4,5)))
grid.arrange(p0,p6,p7,p8,p9,p10,layout_matrix = rbind(c(0,6,7),c(8,9,10)))

