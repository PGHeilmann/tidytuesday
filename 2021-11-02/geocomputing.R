library(sf)
library(raster)
library(dplyr)
library(spData)

library(spDataLarge) 
spDataLarge::nz_elev

library(rayshader)

elmat = raster_to_matrix(nz_elev)

#We use another one of rayshader's built-in textures:

dim(elmat)
(1450 - 1115)/2

empty.mat <- matrix(data = NA, nrow = 1450, ncol = 1450)
empty.mat[168:(1115+167),1:1450] <- elmat
elmat <- empty.mat

elmat[is.na(elmat)] <- -250

zealandshd = ray_shade(elmat, zscale = 250, lambert = FALSE)
zealamb = ambient_shade(elmat, zscale = 250)

elmat %>%
  sphere_shade(zscale = 30,texture = "imhof4") %>%
  add_shadow(zealandshd, 0.5) %>%
  #add_shadow(zealamb, 0) %>%
  plot_3d(elmat, zscale = 250, fov = 0, theta = -45, phi = 45,linewidth = 4,
          windowsize = c(1000, 800), zoom = 0.9, background = "white")
Sys.sleep(0.2)
render_snapshot(clear=TRUE)
render_highquality(filename = "C:/Users/sf8642/Desktop/Daten/tidytuesday/2021/2021-11-02/nz.png", clear = T, lightdirection = 160,lightaltitude = 60 )


spDataLarge::elevation
elmat2 = raster_to_matrix(elevation)

elmat2 %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat2), color = "desert") %>%
  add_shadow(ray_shade(elmat2,sunaltitude = 40,sunangle = 250), 0.5) %>%
  add_shadow(ambient_shade(elmat2), 0.2) %>%
  plot_3d(elmat2, zscale = 35, fov = 0, theta = 210, zoom = 0.8, phi = 50, windowsize = c(1000, 800))
render_snapshot(clear = T)


render_movie(filename = "C:/Users/sf8642/Desktop/Daten/tidytuesday/rotate.mp4", fps = 30, frames = 360, title_text = "Zion National Park")
fff <- magick::image_read_video("C:/Users/sf8642/Desktop/Daten/tidytuesday/rotate.mp4", fps = 30)
magick::image_write_gif(fff, "C:/Users/sf8642/Desktop/Daten/tidytuesday/rotate.gif")
magick::image_write_video(fff, "C:/Users/sf8642/Desktop/Daten/tidytuesday/rotate.gif", framerate = 30)

render_snapshot(filename = "C:/Users/sf8642/Desktop/Daten/tidytuesday/zion_park2.png",title_text = "Zion National Park, Utah", clear = T)

Sys.sleep(0.2)
render_highquality(filename = "C:/Users/sf8642/Desktop/Daten/tidytuesday/zion_park.png", clear = T, lightdirection = 160 )
