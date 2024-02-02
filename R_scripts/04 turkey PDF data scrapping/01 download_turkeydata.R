setwd("C:/Users/three/OneDrive - London School of Hygiene and Tropical Medicine/Neu Kasten/022 master degree LSHTM/LSHTM/013 Data Challenge/My Sanofi/Turkey data/downloaded_data_turkey")
getwd()
#install.packages("pdftools")
library(pdftools)
library(stringr)

#========================================
#Haftalık İnfluenza Raporları
#website:      https://grip.saglik.gov.tr/tr/2023-2024-haftal%C4%B1k-i%CC%87nfluenza-raporlar%C4%B1.html

list_links <-c(
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2024/Haftalik_Influenza_Grip_Surveyans_Raporu_2024_1._Hafta_37b1c.pdf" ,
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2024/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_52._Hafta_9c351.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2024/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_51._Hafta_9a822.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2024/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_50._Hafta_1c611.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2024/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_49._Hafta_29583.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2024/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_48._Hafta_a2bd4.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2024/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_47._Hafta_7d5bb.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2024/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_46._Hafta_c8d65.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2024/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_45._Hafta_40c0f.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2024/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_44._Hafta_22f5b.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2024/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_43._Hafta_cadf8.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2024/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_42._Hafta_ec779.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2024/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_41._Hafta_2f3d9.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_40._Hafta_9c46d.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_36-39._Hafta_c7e1a.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_31-35._Hafta_cf542.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_25-30._Hafta_ed994.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_21-24._Hafta_8d614.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_20._Hafta_e497d.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_19._Hafta_3fddf.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_18._Hafta_82514.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_17._Hafta_1018a.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_16._Hafta_26a54.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_15._Hafta_13e45.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_14._Hafta_58937.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_13._Hafta_b0f89.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_12._Hafta_79e99.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_11._Hafta_d558f.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_10._Hafta_07752.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_9._Hafta_bc386.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_8._Hafta_05c5e.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_7._Hafta_5b22f.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_Influenza_Grip_Surveyans_Raporu_2023_6._Hafta_cd1bf.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftal%C4%B1k%20I%CC%87nfluenza%20(Grip)%20Su%CC%88rveyans%20Raporu%202023%205.%20Hafta.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftal%C4%B1k%20I%CC%87nfluenza%20(Grip)%20Su%CC%88rveyans%20Raporu%202023%204.%20Hafta.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftal%C4%B1k%20I%CC%87nfluenza%20(Grip)%20Su%CC%88rveyans%20Raporu%202023%203.%20Hafta.pdf",
 "https://grip.gov.tr/depo/influenza-raporu/2023/Haftal%C4%B1k%20I%CC%87nfluenza%20(Grip)%20Su%CC%88rveyans%20Raporu%202023%202.%20Hafta.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftal%C4%B1k%20I%CC%87nfluenza(Grip)%20Su%CC%88rveyans%20Raporu%202023%201.%20Hafta.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_InfluenzaGrip_Surveyans_Raporu_2022_52._Hafta_220e8.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_InfluenzaGrip_Surveyans_Raporu_2022_51._Hafta_855c6.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_InfluenzaGrip_Surveyans_Raporu_2022_50._Hafta_3221c.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_InfluenzaGrip_Surveyans_Raporu_2022_49._Hafta_da176.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_InfluenzaGrip_Surveyans_Raporu_2022_48._Hafta_82440.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_InfluenzaGrip_Surveyans_Raporu_2022_47._Hafta_aa0f9.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_InfluenzaGrip_Surveyans_Raporu_2022_46._Hafta_55efb.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_InfluenzaGrip_Surveyans_Raporu_2022_45_Hafta_5fca5.pdf", ##
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_InfluenzaGrip_Surveyans_Raporu_2022_44._Hafta_ef0b4.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_InfluenzaGrip_Surveyans_Raporu_2022_43._Hafta_ec011.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_InfluenzaGrip_Surveyans_Raporu_2022_42_Hafta_e4b5f.pdf", ##
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_InfluenzaGrip_Surveyans_Raporu_2022_41._Hafta_540fe.pdf",
 "https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_InfluenzaGrip_Surveyans_Raporu_2022_40_Hafta_a06d1.pdf" ##
 
)

#list
list_year_week<-list()

for (i in seq_along(list_links)) {
  temp   <- str_extract(list_links[i], "Raporu_(.*?)(?=\\._Hafta)")
  list_year_week[i] <- gsub("Raporu_", "", temp)   
}
print(list_year_week)

# correcting some strange NA
list_year_week[which (list_links=="https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_InfluenzaGrip_Surveyans_Raporu_2022_40_Hafta_a06d1.pdf")] <-"2022_40"
list_year_week[which (list_links=="https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_InfluenzaGrip_Surveyans_Raporu_2022_42_Hafta_e4b5f.pdf")] <-"2022_42"
list_year_week[which (list_links=="https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftalik_InfluenzaGrip_Surveyans_Raporu_2022_45_Hafta_5fca5.pdf")] <-"2022_45"

list_year_week[which (list_links=="https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftal%C4%B1k%20I%CC%87nfluenza%20(Grip)%20Su%CC%88rveyans%20Raporu%202023%205.%20Hafta.pdf")] <-"2023_5"
list_year_week[which (list_links=="https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftal%C4%B1k%20I%CC%87nfluenza%20(Grip)%20Su%CC%88rveyans%20Raporu%202023%204.%20Hafta.pdf")] <-"2023_4"
list_year_week[which (list_links=="https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftal%C4%B1k%20I%CC%87nfluenza%20(Grip)%20Su%CC%88rveyans%20Raporu%202023%203.%20Hafta.pdf")] <-"2023_3"
list_year_week[which (list_links=="https://grip.gov.tr/depo/influenza-raporu/2023/Haftal%C4%B1k%20I%CC%87nfluenza%20(Grip)%20Su%CC%88rveyans%20Raporu%202023%202.%20Hafta.pdf")] <-"2023_2"
list_year_week[which (list_links=="https://grip.saglik.gov.tr/depo/influenza-raporu/2023/Haftal%C4%B1k%20I%CC%87nfluenza(Grip)%20Su%CC%88rveyans%20Raporu%202023%201.%20Hafta.pdf")] <-"2023_1"

print(list_year_week)


#download#############
#cannot dowload too much at a time

list_links_1<-list_links[1:25]
list_year_week_1<-list_year_week[1:25]

for (i in seq_along(list_year_week_1)) {
  download.file(list_links_1[i], paste0( list_year_week_1[i], ".pdf"), mode = "wb")
}


list_links_2<-list_links[26:51]
list_year_week_2<-list_year_week[26:51]

for (i in seq_along(list_year_week_2)) {
  download.file(list_links_2[i], paste0(list_year_week_2[i], ".pdf"), mode = "wb")
}

#manually download ..