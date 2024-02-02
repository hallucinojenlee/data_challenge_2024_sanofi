#install.packages("pdfsearch")
library(pdfsearch)
#install.packages("pdftools")
library(pdftools)
setwd("C:/Users/three/OneDrive - London School of Hygiene and Tropical Medicine/Neu Kasten/022 master degree LSHTM/LSHTM/013 Data Challenge/My Sanofi/Turkey data/downloaded_data_turkey")
getwd()


##get all the file names in a folder
pdf_names<-list.files(path=".", pattern=NULL, all.files=FALSE, 
           full.names=FALSE)# not including r.Data

pdf_names <- pdf_names[-which(pdf_names=="output")]# drop off the folder "output"

year_week<-sub(".pdf","",pdf_names)

#order
ordernumbers <- gsub(".*(\\d{4})_(\\d+).*", "\\1" , year_week)
ordernumbers <- sprintf("%s%02d", ordernumbers, as.numeric(gsub(".*(\\d{4})_(\\d+).*", "\\2", year_week)))

pdf_names<-pdf_names[order(ordernumbers,decreasing = TRUE)]
year_week<-year_week[order(ordernumbers,decreasing = TRUE)]

########################################
# search using grep & pdf_text functions
########################################

#empty df
df <- data.frame(
  flu_title = character(),  flu = integer(), 
  fluA_title = character(),  fluA = integer(), 
  fluB_title = character(),  fluB = integer(),
  RSV_title = character(),  RSV = integer(),  stringsAsFactors = FALSE) 
df1<-df


keyword1 <- "hastalardan alınan solunum yolu numuneleri sonuçları, Sentinel SARI Sürveyansı"
#keyword <- "Sentinel SARI Sürveyansı"
#keyword <- "hastalardan alınan solunum yolu numuneleri sonuçları, Sentinel SARI Sürveyansı|Sentinel SARI Sürveyansı"

#subset only those having table 3 
exclude_pdf <- c("2022_31-35.pdf", "2022_26-30.pdf", "2022_21-25.pdf",
                 "2020_6.pdf","2020_4.pdf","2020_3.pdf","2020_2.pdf","2020_1.pdf","2019_52.pdf","2019_21-26.pdf","2019_21-32.pdf","2019_21-39.pdf","2019_10.pdf","2018_46.pdf","2018_21-24.pdf","2018_21-28.pdf","2018_21-32.pdf","2018_21-39.pdf","2018_20.pdf") 
pdf_names_1 <- pdf_names[!pdf_names %in% exclude_pdf]
pdf_names_1<-pdf_names_1[1:which(pdf_names_1=="2021_12.pdf")]

#loop
for (i in pdf_names_1 ){
#find page of content table 
text_content <- pdf_text(i)
page_SARI <- grep(keyword1, text_content)

##content###############
rows_content<-scan(textConnection(text_content[page_SARI]),
                   what="character", sep = "\n") #page
#find the index number for the row
rownumber_disease<-c( grep("İnfluenza pozitif numune",rows_content),
   grep("İnfluenza A",rows_content)[1],
   grep("İnfluenza B",rows_content),
   grep("Respiratuar Sinsityal Virüs|RSV",rows_content))# or RSV

#extract content
rows_content_diseases <- unlist(strsplit( rows_content[rownumber_disease]  ," \\s+ "))

#get value
indice_flu <- which(rows_content_diseases == "İnfluenza pozitif numune") 
indice_fluA <- which(rows_content_diseases == "İnfluenza A") 
indice_fluB <- which(rows_content_diseases == "İnfluenza B") 
indice_RSV <- which(rows_content_diseases %in% c("RSV", "Respiratuar Sinsityal Virüs")) 

df1[i,]<- rows_content_diseases[c(indice_flu,
                                       indice_flu +1,
                                       indice_fluA,
                                       indice_fluA +1,
                                       indice_fluB,
                                       indice_fluB +1,
                                       indice_RSV,
                                       indice_RSV +1)]
}



##cover###############
# rows_cover<-scan(textConnection(text_content[1]),what="character", sep = "\n") #page 1, cover page
# rownumber_cover<-c( grep("20",rows_cover)) [c(1,2)] #keep first two
# list_cover <- strsplit( rows_cover[rownumber_cover]  ," \\s+ ")

#################
## search over using pdfsearch package
#################
# failed due to the default index of pages is different from  pdf_text method
# #directory is in the hidden appdata
# file <- system.file("pdf", "2024_1.pdf",package = 'pdfsearch')
# print(file)
# heading_search(file, 
#                headings = c("hastalardan alınan solunum yolu numuneleri sonuçları, Sentinel SARI Sürveyansı"),
#                path = TRUE)
# search_result <- keyword_search(file, 
#                           keyword = c('hastalardan alınan solunum yolu numuneleri sonuçları, Sentinel SARI Sürveyansı'),
#                           path = TRUE,
#                           surround_lines = 1)
# print(search_result$line_text)
# print(search_result$page_num)

