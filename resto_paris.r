library(RgoogleMaps)
library(ggplot2)
library(ggmap)
library(maps)
dt = read.csv("commercesparis.csv", header = T, row.names = NULL, sep = ";")
colnames(dt)
levels(dt$LIBELLE.ACTIVITE)
# [17] "Autre restaurant du monde"                                                  
# [29] "Brasserie - Restauration continue sans tabac"    
# [147] "Restaurant africain"                                                        
# [148] "Restaurant antillais"                                                       
# [149] "Restaurant asiatique"                                                       
# [150] "Restaurant central et sud américain"                                        
# [151] "Restaurant européen"                                                        
# [152] "Restaurant indien, pakistanais et Moyen Orient"                             
# [153] "Restaurant maghrébin"                                                       
# [154] "Restaurant traditionnel français"                                           
# [155] "Restauration rapide assise"                                                 
# [156] "Restauration rapide debout" 

resto = c("Brasserie - Restauration continue sans tabac","Restaurant africain","Restaurant antillais","Restaurant asiatique","Restaurant central et sud américain","Restaurant européen","Restaurant indien, pakistanais et Moyen Orient","Restaurant maghrébin","Restaurant traditionnel français","Restauration rapide assise","Restauration rapide debout")
which(dt$LIBELLE.ACTIVITE in resto)

restaurants = dt[which(dt$LIBELLE.ACTIVITE == "Autre restaurant du monde"),]
for (elem in resto){
    dt_temp = (dt[which(dt$LIBELLE.ACTIVITE == elem),])
    restaurants = rbind(restaurants,dt_temp)
}

lat = c()

for (i in seq(1,dim(restaurants)[1])){
    lat = append(lat, as.numeric(strsplit(as.character(restaurants$XY),", ")[[i]][1]))
    print(i)
}
long = c()
for (i in seq(1,dim(restaurants)[1])){
    long = append(long, as.numeric(strsplit(as.character(restaurants$XY),", ")[[i]][2]))
    print(i)
}


restaurants = cbind(restaurants, lat)
restaurants = cbind(restaurants, long)

rand_sample = sample(seq(1,dim(restaurants)[1]),150)
mymarkers = cbind(restaurants$long[rand_sample],restaurants$lat[rand_sample])
mymarkers = cbind(restaurants$long,restaurants$lat)
mymarkers = as.data.frame(mymarkers)
colnames(mymarkers) = c("lon","lat")

themap = get_map("Paris,France", zoom=12, scale = 2)
ggmap(themap) + geom_point(data=mymarkers, aes(x=lon, y=lat),size=1, col = "red",alpha=0.2)

