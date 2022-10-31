# Code to create experimental stimuli involving custom images, carbon footprint label, and text

# code author: Adrian Gadient-Bruegger
# last updated: 31 October 2022


# #################################################
# ## Introduction / explanatory comments ----
# #################################################

## This code creates stimuli for four treatment conditions for an experiment intersted in the effect of carbon labels
# 1. control group, not label 
# 2. absolute: shows foot (environmental footprint) and CO2 emissions
# 3. traffic: shows 4 small feet and 1 big foot illustrating the impact compared to similar products
# 4. combined: traffic + CO2 emissions


## The final image will contain 4 elements with varying content.

# A) header with product name and origin
# B) product image (from Adobe Stock)
# C) label (or no label in the control condition)
# D) text indicating the carbon emissions of the product (in condition "absolute" and "combined")

## Technical note
# Because the height of the product image varies, the three parts cannot be directly put together. 
# To do that, it would be necessary to use a base image / layer with a fixed size and the function image_composite 
# The solution now is to create the base part consisting of A) and B) and to then to separately create and add C) & D).



#################################################
# load packages (and install if not yet available)
if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse","readxl", "stringr","magick")
#################################################


#################################################
## import and prepare data ----
#################################################

df <- readxl::read_excel("../product_info.xlsx", sheet = "stimuli_data")

# create path to product images
df$image_path <- stringr::str_c("../images/originals/products/",df$image_product)
  
# update path to label
df$label_path <- stringr::str_c("../images/originals/labels/",df$label,"_V2_1.png")

#################################################
## A) and B) create header  ----
#################################################

# create head / basic image consisting of product name (A) and image (B)
# lapply applies the function defined below for all elements / rows in the column "image_path"
img_lst <- lapply(seq_along(df$image_path), function(i){
  # create white background
  background <- image_blank(width = 770, height = 160, color = "grey25")
  # get product name
  product_en <- df$product_en[[i]]
  # get image
  product_image <- image_read(df$image_path[[i]]) 
  # add text to background
  base <- background %>% 
    # add product name from variable "product_en"
    image_annotate(paste0(df$product_en[[i]]), size = 82, color = "white", weight=700, 
                   location = "+6+2", font= "Helvetica") %>% 
    # add fix text for place of production 
    image_annotate("Produced in:", size = 48, color = "white", weight=500, 
                   location = "+6+96", font= "Helvetica") %>% 
    # add origin from variable "origin_en"
    image_annotate(paste0(df$origin_en[[i]]), size = 48, color = "white", weight=500, 
                   location = "+300+96") 
  # create image consisting of base with text + image
  img <- c(base,product_image)
  # explain how to combine these elements
  image_append(image_scale(img, "770"), stack = TRUE) %>% 
    # save image
    image_write(path = paste0("../images/final/", "control_",product_en, ".png"), format = "png") #, quality = 50) #, compression = 
})



###########################################################################
## C) prepare foot for experimental condition "traffic" (feet without CO2) 
###########################################################################

img_lst <- lapply(seq_along(df$product_en), function(i){
  product_en <- df$product_en[[i]]
  image_read(df$label_path[[i]]) %>% 
    image_background("grey25") %>% 
    image_border("grey25", "80x10") %>% # add border so that image will end up smaller in the final version
    image_crop(geometry="1200x445",repage=TRUE) %>% # crop bottom to make picture less high
    image_write(path = paste0("../images/traffic/", product_en, ".png"), format = "png") #, quality = 50) #, compression = 
})

# combine image and foot 
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


# combine image and foot (all absolute images)
img_lst <- lapply(seq_along(df$image_path), function(i){
  product_en <- df$product_en[[i]]
  header <- image_read(path = paste0("../images/final/control_", df$product_en[[i]], ".png")) 
  label  <- image_read(path = paste0("../images/absolute/", df$product_en[[i]], ".png")) 
  img <- c(header,label)
  image_append(image_scale(img,"770"), stack = TRUE) %>% 
  image_write(path = paste0("../images/final/", "absolute_", product_en, ".png"), format = "png") #, quality = 50) #, compression = 
})


#################################################
## C) prepare foot for experimental condition "combined" (feet with CO2)
#################################################

# Different functions for each label because text is placed on different positions

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

# overwrite  products with co2 >=10 so that text appears in good place
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


# combine image and foot (all combined images)
img_lst <- lapply(seq_along(df$image_path), function(i){
  product_en <- df$product_en[[i]]
  header <- image_read(path = paste0("../images/final/control_", df$product_en[[i]], ".png")) 
  label  <- image_read(path = paste0("../images/combined/", df$product_en[[i]], ".png")) 
  img <- c(header,label)
  image_append(image_scale(img,"770"), stack = TRUE) %>% 
    image_write(path = paste0("../images/final/", "combined-", product_en, ".png"), format = "png") #, quality = 50) #, compression = 
})
