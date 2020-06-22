

library(readxl)

link2coded_data <- paste(tempfile(),".xlsx",sep = "")
download.file("https://uitno.box.com/shared/static/4s5aet6g7qnpk4mpsa64a1v75t7ksbla.xlsx", link2coded_data, mode = "wb")

codeddata1 <- read_excel(link2coded_data,sheet = 'CODING_template_1_1000')
codeddata2 <- read_excel(link2coded_data,sheet = 'CODING_template_1001_2000')
codeddata3 <- read_excel(link2coded_data,sheet = 'CODING_template_2001_3000')
codeddata4 <- read_excel(link2coded_data,sheet = 'CODING_template_3001_')


View(codeddata1)
View(codeddata2)
View(codeddata3)
View(codeddata4)
