# Code to create experimental stimuli involving custom images and text

# code author: Adrian Gadient-Bruegger
# last updated: 26 October 2022


# load packages (and install if not yet available)
if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse","readxl", "stringr","magick")


# #################################################
# ## Introduction / explanatory comments ----
# #################################################

# The final image will contain 3 elements with varying content.

# A) header with product name and origin
# B) product image
# C) label (or no label in the control condition)

# Because the height of the product image varies, the three parts cannot be directly put together. 
# To do that, it would be necessary to use base image / layer with a fixed size and the function image_composite 
# The solution now is to create the base part consisting of A) and B) and to then to separately create and add C).



#################################################
## prepare data ----
#################################################

df <- readxl::read_excel("../product_info.xlsx", sheet = "stimuli_data")


#df <- df %>% filter(product_de == "Bulgur")

# update path to folder
df$image_folder <- stringr::str_c("../../",df$image_folder,"/")

# create path to product images
df$image_path <- stringr::str_c(df$image_folder,df$image_product)
  
# update path to label
# version 1
#df$label_path <- stringr::str_c("../images/originals/labels/",df$label,".png")
# version 2
df$label_path <- stringr::str_c("../images/originals/labels/",df$label,"_V2_1.png")

# # TODO extend to whole image pool
# # only use meat and fish images
# df <- df %>% filter(!is.na(image_product)) %>% filter(category_en == "meat")

#### now obsolete, delete soon:
# # create background on which text and images can be placed
# background <- image_blank(width = 770, height = 800, color = "yellow")
# image_write(background, path = "../images/originals/background.png", format = "png")
# 
# # create background on which text and images can be placed
# background_text <- image_blank(width = 770, height = 140, color = "yellow")
# image_write(background_text, path = "../images/originals/background_text.png", format = "png")
# 
# # create background for feet
# background_feet <- image_blank(width = 770, height = 140, color = "green")
# image_write(background_feet, path = "../images/originals/background_feet.png", format = "png")




#################################################
## A) and B) create header  ----
#################################################

# create head / basic image consisting of product name (A) and image (B)
img_lst <- lapply(seq_along(df$image_path), function(i){
  background <- image_blank(width = 770, height = 160, color = "grey25")
  product_de <- df$product_de[[i]]
  product_en <- df$product_en[[i]]
  product_image <- image_read(df$image_path[[i]]) 
  base <- background %>% 
    image_annotate(paste0(df$product_de[[i]]), size = 82, color = "white", weight=700, 
                   location = "+6+2", font= "Helvetica") %>% 
    image_annotate("Produktionsland:", size = 48, color = "white", weight=500, 
                   location = "+6+96", font= "Helvetica") %>% 
    image_annotate(paste0(df$origin_de[[i]]), size = 48, color = "white", weight=500, 
                   location = "+380+96") 
  img <- c(base,product_image)
  image_append(image_scale(img, "770"), stack = TRUE) %>% 
    # img <- c(background,pic_1)
    # image_mosaic(img) %>%
    #image_annotate(df$description[i], size=20, color = "black", weight=700, location = "+1+100") %>%
    image_write(path = paste0("../images/final/", "control_",product_en, ".png"), format = "png") #, quality = 50) #, compression = 
})



#################################################
## C) prepare foot for experimental condition "combined" (feet with CO2)
#################################################

# do A - Labels
df_A <- df %>% filter(label=="A")
  
img_lst <- lapply(seq_along(df_A$label_path), function(i){
  product_en <- df_A$product_en[[i]]
  image_read(df_A$label_path[[i]]) %>% 
    image_annotate(paste0(sprintf("%.1f",round(df_A$co2[[i]], 1))), size = 70, color = "black", weight=700, 
                   location = "+40+120",font= "Helvetica") %>% 
    image_annotate(" kg\nCO2", size = 43, color = "black", weight=700, 
                   location = "+50+190",font= "Helvetica") %>% 
    image_background("grey25") %>% 
    image_border("grey25", "80x10") %>% # add border so that image will end up smaller in the final version
    image_crop(geometry="1200x435",repage=TRUE) %>% # crop bottom to make picture less high
    image_write(path = paste0("../images/combined/", product_en, ".png"), format = "png") #, quality = 50) #, compression = 
})


# do B - Labels

df_B <- df %>% filter(label=="B")

img_lst <- lapply(seq_along(df_B$label_path), function(i){
  product_en <- df_B$product_en[[i]]
  image_read(df_B$label_path[[i]])  %>% 
    image_annotate(paste0(sprintf("%.1f",round(df_B$co2[[i]], 1))), size = 70, color = "black", weight=700, 
                   location = "+164+120",font= "Helvetica") %>% 
    image_annotate(" kg\nCO2", size = 43, color = "black", weight=700, 
                   location = "+174+190",font= "Helvetica") %>% 
    image_background("grey25") %>% 
    image_border("grey25", "80x10") %>% # add border so that image will end up smaller in the final version
    image_crop(geometry="1200x435",repage=TRUE) %>% # crop bottom to make picture less high
    image_write(path = paste0("../images/combined/", product_en, ".png"), format = "png") #, quality = 50) #, compression = 
})



# C - Labels
df_C <- df %>% filter(label=="C")

img_lst <- lapply(seq_along(df_C$label_path), function(i){
  product_en <- df_C$product_en[[i]]
  image_read(df_C$label_path[[i]])  %>% 
    image_annotate(paste0(sprintf("%.1f",round(df_C$co2[[i]], 1))), size = 70, color = "black", weight=700, 
                   location = "+284+120",font= "Helvetica") %>% 
    image_annotate(" kg\nCO2", size = 43, color = "black", weight=700, 
                   location = "+294+190",font= "Helvetica") %>% 
    image_background("grey25") %>% 
    image_border("grey25", "80x10") %>% # add border so that image will end up smaller in the final version
    image_crop(geometry="1200x435",repage=TRUE) %>% # crop bottom to make picture less high
    image_write(path = paste0("../images/combined/", product_en, ".png"), format = "png") #, quality = 50) #, compression = 
})


# D - Labels

df_D <- df %>% filter(label=="D")

img_lst <- lapply(seq_along(df_D$label_path), function(i){
  product_en <- df_D$product_en[[i]]
  image_read(df_D$label_path[[i]])  %>% 
    image_annotate(paste0(sprintf("%.1f",round(df_D$co2[[i]], 1))), size = 70, color = "black", weight=700, 
                   location = "+424+120",font= "Helvetica") %>% 
    image_annotate(" kg\nCO2", size = 43, color = "black", weight=700, 
                   location = "+433+190",font= "Helvetica") %>% 
    image_background("grey25") %>% 
    image_border("grey25", "80x10") %>% # add border so that image will end up smaller in the final version
    image_crop(geometry="1200x440",repage=TRUE) %>% # crop bottom to make picture less high
    image_write(path = paste0("../images/combined/", product_en, ".png"), format = "png") #, quality = 50) #, compression = 
})


# E - Labels
df_E <- df %>% filter(label=="E")

img_lst <- lapply(seq_along(df_E$label_path), function(i){
  product_en <- df_E$product_en[[i]]
  image_read(df_E$label_path[[i]])  %>% 
    image_annotate(paste0(sprintf("%.1f",round(df_E$co2[[i]], 1))), size = 70, color = "black", weight=700, 
                   location = "+549+120",font= "Helvetica") %>% 
    image_annotate(" kg\nCO2", size = 43, color = "black", weight=700, 
                   location = "+559+190",font= "Helvetica") %>% 
    image_background("grey25") %>% 
    image_border("grey25", "80x10") %>% # add border so that image will end up smaller in the final version
    image_crop(geometry="1200x445",repage=TRUE) %>% # crop bottom to make picture less high
    image_write(path = paste0("../images/combined/", product_en, ".png"), format = "png") #, quality = 50) #, compression = 
})

# overwrite  products with co2 >=10
df_E <- df %>% filter(label=="E") %>% filter(co2 >=10)

img_lst <- lapply(seq_along(df_E$label_path), function(i){
  product_en <- df_E$product_en[[i]]
  image_read(df_E$label_path[[i]])  %>% 
    image_annotate(paste0(sprintf("%.1f",round(df_E$co2[[i]], 1))), size = 70, color = "black", weight=700, 
                   location = "+520+120",font= "Helvetica") %>% 
    image_annotate(" kg\nCO2", size = 43, color = "black", weight=700, 
                   location = "+559+190",font= "Helvetica") %>% 
    image_background("grey25") %>% 
    image_border("grey25", "80x10") %>% # add border so that image will end up smaller in the final version
    image_crop(geometry="1200x445",repage=TRUE) %>% # crop bottom to make picture less high
    image_write(path = paste0("../images/combined/", product_en, ".png"), format = "png") #, quality = 50) #, compression = 
})

# # check if all have same proportions
# image_read("../images/combined/Tofu.png") %>% image_info()
# image_read("../images/combined/Beef.png") %>% image_info()
# image_read("../images/combined/Pork.png") %>% image_info()
# image_read("../images/combined/Seitan.png") %>% image_info()
# image_read("../images/combined/Chicken.png") %>% image_info()
# image_read("../images/combined/Salmon.png") %>% image_info()
# # not perfect but good enough

# combine image and foot (all combined images)
img_lst <- lapply(seq_along(df$image_path), function(i){
  product_en <- df$product_en[[i]]
  header <- image_read(path = paste0("../images/final/control_", df$product_en[[i]], ".png")) 
  label  <- image_read(path = paste0("../images/combined/", df$product_en[[i]], ".png")) 
  # product_image <- image_read(df_A$image_path[[i]]) 
  # base <- background %>% 
  #   image_annotate(paste0(df_A$product_de[[i]]), size = 80, color = "black", weight=700, location = "+6+1", font= "Helvetica") %>% 
  #   image_annotate("Produktionsland:", size = 40, color = "black", weight=500, location = "+6+90", font= "Helvetica") %>% 
  #   image_annotate(paste0(df_A$origin_de[[i]]), size = 40, color = "black", weight=500, location = "+310+90") 
  img <- c(header,label)
  image_append(image_scale(img,"770"), stack = TRUE) %>% 
    #image_border(color = "black", geometry = "3x3") %>% 
    #   # img <- c(background,pic_1)
    #   # image_mosaic(img) %>%
    #   #image_annotate(df_A$description[i], size=20, color = "black", weight=700, location = "+1+100") %>%
    image_write(path = paste0("../images/final/", "combined-", product_en, ".png"), format = "png") #, quality = 50) #, compression = 
})

# # check if all have same width (height varies because of product photos)
# image_read("../images/final/combined-Tofu.png") %>% image_info()
# image_read("../images/final/combined-Beef.png") %>% image_info()
# image_read("../images/final/combined-Pork.png") %>% image_info()
# image_read("../images/final/combined-Seitan.png") %>% image_info()
# image_read("../images/final/combined-Chicken.png") %>% image_info()
# image_read("../images/final/combined-Salmon.png") %>% image_info()


###########################################################################
## C) prepare foot for experimental condition "traffic" (feet without CO2) 
###########################################################################

# do A - Labels
df_A <- df %>% filter(label=="A")

img_lst <- lapply(seq_along(df_A$label_path), function(i){
  product_en <- df_A$product_en[[i]]
  image_read(df_A$label_path[[i]]) %>% 
    image_background("grey25") %>% 
    image_border("grey25", "80x10") %>% # add border so that image will end up smaller in the final version
    image_crop(geometry="1200x435",repage=TRUE) %>% # crop bottom to make picture less high
    image_write(path = paste0("../images/traffic/", product_en, ".png"), format = "png") #, quality = 50) #, compression = 
})


# do B - Labels

df_B <- df %>% filter(label=="B")

img_lst <- lapply(seq_along(df_B$label_path), function(i){
  product_en <- df_B$product_en[[i]]
  image_read(df_B$label_path[[i]])  %>% 
    image_background("grey25") %>% 
    image_border("grey25", "80x10") %>% # add border so that image will end up smaller in the final version
    image_crop(geometry="1200x435",repage=TRUE) %>% # crop bottom to make picture less high
    image_write(path = paste0("../images/traffic/", product_en, ".png"), format = "png") #, quality = 50) #, compression = 
})



# C - Labels
df_C <- df %>% filter(label=="C")

img_lst <- lapply(seq_along(df_C$label_path), function(i){
  product_en <- df_C$product_en[[i]]
  image_read(df_C$label_path[[i]])  %>% 
    image_background("grey25") %>% 
    image_border("grey25", "80x10") %>% # add border so that image will end up smaller in the final version
    image_crop(geometry="1200x435",repage=TRUE) %>% # crop bottom to make picture less high
    image_write(path = paste0("../images/traffic/", product_en, ".png"), format = "png") #, quality = 50) #, compression = 
})


# D - Labels

df_D <- df %>% filter(label=="D")

img_lst <- lapply(seq_along(df_D$label_path), function(i){
  product_en <- df_D$product_en[[i]]
  image_read(df_D$label_path[[i]])  %>% 
    image_background("grey25") %>% 
    image_border("grey25", "80x10") %>% # add border so that image will end up smaller in the final version
    image_crop(geometry="1200x440",repage=TRUE) %>% # crop bottom to make picture less high
    image_write(path = paste0("../images/traffic/", product_en, ".png"), format = "png") #, quality = 50) #, compression = 
})


# E - Labels
df_E <- df %>% filter(label=="E")

img_lst <- lapply(seq_along(df_E$label_path), function(i){
  product_en <- df_E$product_en[[i]]
  image_read(df_E$label_path[[i]])  %>% 
    image_background("grey25") %>% 
    image_border("grey25", "80x10") %>% # add border so that image will end up smaller in the final version
    image_crop(geometry="1200x445",repage=TRUE) %>% # crop bottom to make picture less high
    image_write(path = paste0("../images/traffic/", product_en, ".png"), format = "png") #, quality = 50) #, compression = 
})


# combine image and foot (all traffic images)
img_lst <- lapply(seq_along(df$image_path), function(i){
  product_en <- df$product_en[[i]]
  header <- image_read(path = paste0("../images/final/control_", df$product_en[[i]], ".png")) 
  label  <- image_read(path = paste0("../images/traffic/", df$product_en[[i]], ".png")) 
  img <- c(header,label)
  image_append(image_scale(img,"770"), stack = TRUE) %>% 
    image_write(path = paste0("../images/final/", "traffic_", product_en, ".png"), format = "png") #, quality = 50) #, compression = 
})



###########################################################################
# create part for single foot ----
###########################################################################

img_lst <- lapply(seq_along(df$image_path), function(i){
  product_en <- df$product_en[[i]]
  image_read("../images/originals/labels/White_V2.png")  %>% 
    image_annotate(paste0(sprintf("%.1f",round(df$co2[[i]], 1))), size = 83, color = "black", weight=700, 
                   location = "+73+150",font= "Helvetica") %>% 
    image_annotate(" kg\nCO2", size = 55, color = "black", weight=700, 
                   location = "+89+230",font= "Helvetica") %>% 
    image_background("grey25") %>% 
    image_border("grey25", "400x0") %>% # add border so that foot will be similarly large as in traffic version
    image_write(path = paste0("../images/absolute/", product_en, ".png"), format = "png") #, quality = 50) #, compression = 
})

# overwrite  products with co2 >=10
df_big <- df %>% filter(co2 >=10)
img_lst <- lapply(seq_along(df_big$image_path), function(i){
  product_en <- df_big$product_en[[i]]
  image_read("../images/originals/labels/White_V2.png")  %>% 
    image_annotate(paste0(sprintf("%.1f",round(df_big$co2[[i]], 1))), size = 83, color = "black", weight=700, 
                   location = "+40+150",font= "Helvetica") %>% 
    image_annotate(" kg\nCO2", size = 55, color = "black", weight=700, 
                   location = "+89+230",font= "Helvetica") %>% 
    image_background("grey25") %>% 
    image_border("grey25", "400x0") %>% # add border so that foot will be similarly large as in traffic version
    image_write(path = paste0("../images/absolute/", product_en, ".png"), format = "png") #, quality = 50) #, compression = 
})

# # check if all have same proportions
# image_read("../images/absolute/Tofu.png") %>% image_info()
# image_read("../images/absolute/Beef.png") %>% image_info()
# image_read("../images/absolute/Pork.png") %>% image_info()
# image_read("../images/absolute/Seitan.png") %>% image_info()
# image_read("../images/absolute/Chicken.png") %>% image_info()
# image_read("../images/absolute/Salmon.png") %>% image_info()
# # not perfect but good enough

# combine image and foot (all absolute images)
img_lst <- lapply(seq_along(df$image_path), function(i){
  product_en <- df$product_en[[i]]
  header <- image_read(path = paste0("../images/final/control_", df$product_en[[i]], ".png")) 
  label  <- image_read(path = paste0("../images/absolute/", df$product_en[[i]], ".png")) 
  img <- c(header,label)
  image_append(image_scale(img,"770"), stack = TRUE) %>% 
  image_write(path = paste0("../images/final/", "absolute_", product_en, ".png"), format = "png") #, quality = 50) #, compression = 
})

# # check if all have same width (height varies because of product photos)
# image_read("../images/final/combined-Tofu.png") %>% image_info()
# image_read("../images/final/combined-Beef.png") %>% image_info()
# image_read("../images/final/combined-Pork.png") %>% image_info()
# image_read("../images/final/combined-Seitan.png") %>% image_info()
# image_read("../images/final/combined-Chicken.png") %>% image_info()
# image_read("../images/final/combined-Salmon.png") %>% image_info()






#############################################################################
############################################################################# 
# old parts
#############################################################################
#############################################################################

#  # this works but makes bottom ends of different lengths
# # create basic image consisting of product name and image
# # read images, annotate, append to list
# img_lst <- lapply(seq_along(df$image_path), function(i){
#   background <- image_read("../images/originals/background.png")
#   product_name <- df$product_de[[i]]
#   product_image <- image_read(df$image_path[[i]]) 
#   image_composite(image_scale(background), image_scale(product_image, "770"), offset = "+0+140") %>% 
#     image_annotate(paste0(df$product_de[[i]]), size = 80, color = "black", weight=700, location = "+6+1") %>% 
#     image_annotate("Produktionsland:", size = 40, color = "black", weight=500, location = "+6+90") %>% 
#     image_annotate(paste0(df$origin_de[[i]]), size = 40, color = "black", weight=500, location = "+310+90") %>% 
#   # img <- c(background,pic_1)
#   # image_mosaic(img) %>%
#     #image_annotate(df$description[i], size=20, color = "black", weight=700, location = "+1+100") %>%
#     image_write(path = paste0("../images/base/",product_name, ".png"), format = "png") #, quality = 50) #, compression = 
# })




















# #################################################
# ## create an example image ----
# #################################################
# 
# salmon <- image_read("../../1_choice task/Aktuellste Version Produktetabelle/Bilder_Produkte/FleischFisch/Lachs_AdobeStock_212143527.jpeg")
# label <- image_read("../images/originals/labels/D.png")
# 
# # create background on which text and images can be placed
# background <- image_blank(width = 770, height = 1075, color = "yellow")
# image_write(background, path = "../images/originals/background.png", format = "png")
# 
# # create composite image
# image_composite(image_scale(background), image_scale(salmon, "770"), offset = "+0+140") %>% 
#   image_composite(image_scale(label, "600"), offset = "+75+570") %>% 
#   image_annotate("Lachs", size = 80, color = "black", weight=700, location = "+6+1") %>% 
#   image_annotate("Produktionsland:", size = 40, color = "black", weight=500, location = "+6+90") %>% 
#   # image_annotate("Inhaltsstoffe:", size = 40, color = "black",weight=500, location = "+6+140") %>% 
#   image_border("gray28", "18x18") %>% 
#   image_write("../images/edited/salmon.png")
# 
# 
# # # create composite image
# # image_append(image_scale(background), image_scale(salmon, "770")) %>% 
# #   image_annotate("Lachs", size = 80, color = "black", weight=700, location = "+6+1") %>% 
# #   image_annotate("Produktionsland:", size = 40, color = "black", weight=500, location = "+6+90") %>% 
# #   # image_annotate("Inhaltsstoffe:", size = 40, color = "black",weight=500, location = "+6+140") %>% 
# #   image_append(image_scale(label, "600")) %>% 
# #   image_border("gray28", "18x18") %>% 
# #   image_write("../images/edited/salmon_appended.png")
# 












 # # old version that worked:
# # read images, annotate, append to list
# img_lst <- lapply(seq_along(df$image_path), function(i){
#   background <- image_read("images/originals/background.png")
#   #name <- tools::file_path_sans_ext(df$image_path[[i]])
#   name <- basename(df$image_path[[i]])
#   pic_1 <- image_read(df$image_path[[i]]) 
#   img <- c(background,pic_1)
#   #image_append(image_scale(img, "100"), stack = TRUE) %>% 
#   image_mosaic(img) %>% 
#     image_annotate(df$description[i], size=20, color = "black", weight=700, location = "+1+100") %>% 
#     image_write(path = paste0("images/",  "combined-",name))
# })









  # # collapse image list to image stack for easier visualization. 
# # otherwise each element of the list can be saved into individual file in the lapply above
# Reduce(`c`, img_lst) %>% 
#   image_montage(shadow = TRUE) %>% 
#   image_convert(format = "png") 
# 
