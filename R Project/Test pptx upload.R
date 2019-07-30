## Google upload

library(pacman)
p_load(ggplot2, rgoogleslides, googledrive, googleAuthR, googlesheets, officer
       )

# Authorization functions
googledrive::drive_auth()
rgoogleslides::authorize(client_id = "510732011705-1dlgh5d6u9upcm1bpo66inpot2t7gbmt.apps.googleusercontent.com", 
                         client_secret = "MwJSI4t8h2BJXzDEB6i1ImF7")
drive_find(n_max = 30)
gs_auth()
gs_user()
gs_new("test")


  
# Create and save plot
ggsave("portfolio comparison.png", test_plot)

# Determine the dimensions of the image
image <- png::readPNG("portfolio comparison.png")
dimension <- dim(image)
i_width <- dimension[1]/8
i_height <- dimension[2]/8

# Upload image to Google drive
id <- googledrive::drive_update("portfolio comparison.png")
image_id <- id$id

# Create a new googleslides presentation
slide_id <- rgoogleslides::create_slides(title = "Test2", full_response = FALSE)
slide_details <- rgoogleslides::get_slides_properties(slide_id)

# Obtain the slide page that the image is to be added to
slide_page_id <- slide_details$slide$objectId

# Get the position details of the element on the slide
page_element <- rgoogleslides::aligned_page_element_property(slide_page_id, 
                                                             image_height = i_height, image_width = i_width)
request <- rgoogleslides::add_create_image_request(url = image_id, page_element_property = page_element)
response <- rgoogleslides::commit_to_slides(slide_id, request)
commit_to_slides(slide_id, request)

#### Creating pptx
## To pptx 
doc <- read_pptx() %>%
  add_slide(layout = "Title and Content", master = "Office Theme") %>%
  ph_with("Hello world", location = ph_location_type(type = "body")) 

print(doc, target = "C:/Users/Paul/Desktop/Ubiqum/test.pptx") 

doc <- add_slide(doc, layout = "Title and Content",
                     master = "Office Theme")
gg_plot <- test_plot
doc <- ph_with(x = doc, value = gg_plot, location = ph_location_fullsize() )
print(doc, target = "C:/Users/Paul/Desktop/Ubiqum/190726_CEO Presentation.pptx") 
               