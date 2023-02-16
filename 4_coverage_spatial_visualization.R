#4: Combining dps data with coverage at clusters to visualize on visual map.

library("tigris") #for geo-join()
library("sf") #for read shape file ie st_read

#combine both files

gps_data=st_read("C:/Users/manoj/Documents/CTech/Project/dhs/nigeria/NGGE6AFL/NGGE6AFL.shp")

gps_data_join=geo_join(gps_data, subset_cluster, 'DHSCLUST', 'cluster')

#visualize the data in our sf object using the ggplot package
library("ggplot2")

# Sequential color scheme. 

visual <- ggplot() + 
  geom_sf(data = gps_data_join, size = 1, aes(col = coverage)) +
  ggtitle("Cluster points") + 
  coord_sf()

# Specify the colors for low and high ends of gradient + plot generation
visual + scale_color_gradient(low = "red", high = "darkgreen")
