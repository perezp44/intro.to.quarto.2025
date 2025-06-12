#- script para hacer un resize de las imagenes de las slides
#- las fotos de los post de ejemplos eran de 640x426

#- para hacer imagenes: https://www.craiyon.com/    https://huggingface.co/spaces/dalle-mini/dalle-mini

#- cojo el post mas moderno (x fecha)
# carpetas_de_posts <- fs::dir_ls(here::here("posts"), type = "directory")
# carpeta <- carpetas_de_posts[1] #- la ruta a la carpeta más antigua
# carpeta <- carpetas_de_posts[length(carpetas_de_posts)] #- la ruta a la carpeta más moderna

my_thumbnail <- here::here("slides", "imagenes", "ss_04_img_01b.png")
my_thumbnail <- here::here("blog", "thumbnails", "thumbnail_66.png")


img <-  magick::image_read(my_thumbnail)
img_resized <- img |> magick::image_scale("740x426!")

#img_resized <- img |> magick::image_scale("680x444!")
#img_resized <- img |> magick::image_resize("680x270!")


#- guardo la foto reescalada 
my_thumbnail_z <- paste0(my_thumbnail)
#magick::image_write(img_resized, my_thumbnail)
magick::image_write(img_resized, my_thumbnail_z)
