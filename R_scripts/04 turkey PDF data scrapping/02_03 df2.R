source("C:/Users/three/OneDrive - London School of Hygiene and Tropical Medicine/Neu Kasten/022 master degree LSHTM/LSHTM/013 Data Challenge/My Sanofi/Turkey data/02_02 df1.r")

#excluding manually pdfs
exclude_pdf <- c("2022_31-35.pdf", "2022_26-30.pdf", "2022_21-25.pdf",
                 "2020_6.pdf","2020_4.pdf","2020_3.pdf","2020_2.pdf","2020_1.pdf","2019_52.pdf","2019_21-26.pdf","2019_21-32.pdf","2019_21-39.pdf","2019_10.pdf","2018_46.pdf","2018_21-24.pdf","2018_21-28.pdf","2018_21-32.pdf","2018_21-39.pdf","2018_20.pdf") 
pdf_names_2 <- pdf_names[!pdf_names %in% exclude_pdf]

pdf_names_2<-pdf_names_2[which(pdf_names_2=="2020_9.pdf"):which(pdf_names_2=="2017_40.pdf")]


keyword_df2<- "GRİP SEZONU SENTİNEL SARI SÜRVEYANSI"
#keyword<- "3 2019-2020 GRİP SEZONU SENTİNEL SARI SÜRVEYANSI"
#keyword <-"SENTİNEL SARI SÜRVEYANSI"

df2<-df

for (i in pdf_names_2){
  print(i)
  text_content <- pdf_text(i)
  
  page_SARI <- grep(keyword_df2, text_content)
  page_SARI_trim <-page_SARI[1]
  
  rows_content<-scan(textConnection(text_content[page_SARI_trim]),
                     what="character", sep = "\n") 
  #find the index number for the row
  rownumber_disease<-c( grep("İnfluenza pozitif numune|İnfluenza Toplam Pozitiflik*",rows_content),
                        grep("İnfluenza A|İnf A",rows_content)[1],
                        grep("İnfluenza B|İnf B",rows_content)[1],
                        grep("Respiratuar Sinsityal Virüs|RSV",rows_content))
  
  
  #trimming
  if (length(rownumber_disease)!=4){
    rownumber_disease_trim <- rownumber_disease[ - grep("laboratuvarda",rows_content[rownumber_disease]) ]
  }
  rownumber_disease_trim<- rownumber_disease
  print(rownumber_disease_trim)
  
  list_disease  <- strsplit( rows_content[rownumber_disease_trim]," \\s+ ")
  
  length_element <- sapply(list_disease, function(el) length(el))
  print(length_element)
  
  value <- c(
    list_disease[[1]][c(1,length_element[1]-1)],
    list_disease[[2]][c(1,length_element[2]-1)],
    list_disease[[3]][c(1,length_element[3]-1)],
    list_disease[[4]][c(1,length_element[4]-1)]
   )
  
   df2[i,]<-value
}  
  



###############
##manualy entry
###############

#no data from exclude_pdf: 2022_31-35.pdf,2022_26-30.pdf,2022_21-25.pdf
print(exclude_pdf)
matrix_manual<-matrix(ncol=5,
                  byrow = TRUE,
                  data=c(
                    c("2020_6",28, 16, 11, 19),
                    c("2020_4",56 ,40 , 16, 9),
                    c("2020_3",89,55 ,16 , 10),
                    c("2022_2", 66, 52, 12,7 ),
                    c("2022_1",52 , 43, 5,6 ),
                    c("2019_52",45 ,36 ,9 , 5),
                    c("2019_21-26", 8,0 ,8 ,0 ),
                    c("2019_21-32", 10,0 ,10 ,1 ),
                    c("2019_21-39", 11, 0, 11,1 ),
                    c("2019_10",2 ,2 ,0 ,15 ),
                    c("2018_46", 0, 0, 0,4 ),
                    c("2018_21-24",0 ,0 ,0 ,1 ),
                    c("2018_21-28",1 , 0, 1,2 ),
                    c("2018_21-32",1 , 0, 1,4 ),
                    c("2018_21-39",6 ,4 ,2 ,4 ),
                    c("2018_20", 2, 0,2 ,1 )
                  ))
matrix_manual<-data.frame(matrix_manual)
colnames(matrix_manual)<-c("year_week","flu","fluA","fluB","RSV")

df_manual <-data.frame(
  flu_title = "İnfluenza Toplam Pozitiflik*",
  flu= matrix_manual$flu,
  fluA_title = "İnf A",
  fluA= matrix_manual$fluA,
  fluB_title = "İnf B",
  fluB= matrix_manual$fluB,
  RSV_title = "RSV",
  RSV= matrix_manual$RSV
)
rownames(df_manual)<-matrix_manual[,"year_week"]
## rbind df2 and munual matrix
df2   <-rbind(df2,df_manual)


