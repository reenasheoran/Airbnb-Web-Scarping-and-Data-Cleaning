library(robotstxt) # For checking website scraping permission
library(rvest) # For scraping the data
library(XML) # For scraping the data
library(stringr) # For cleaning the data
library(dplyr) # For filtering or reordering the data
library(ggplot2) # For creating charts
library(plotly) # For making the charts interactive

#To check for scraping permission

paths_allowed("https://www.airbnb.ca/s/Calgary--AB/homes?tab_id=home_tab&refinement_paths%5B%5D=%2Fhomes&source=structured_search_input_header&search_type=filter_change&place_id=ChIJ1T-EnwNwcVMROrZStrE7bSY&checkin=2020-12-25&checkout=2020-12-31&adults=1")

#@@@@@@@@@@@@@@@@@@@@@@@@@#
###### Web Scraping #######
#@@@@@@@@@@@@@@@@@@@@@@@@@#

C_Url <- "https://www.airbnb.ca/s/Calgary--AB/homes?tab_id=home_tab&refinement_paths%5B%5D=%2Fhomes&source=structured_search_input_header&search_type=pagination&checkin=2020-12-25&checkout=2020-12-31&adults=1&place_id=ChIJ1T-EnwNwcVMROrZStrE7bSY&federated_search_session_id=c8711d0b-d7a6-40e5-8ab7-510a7e454bf6&items_offset="
T_Url <- "https://www.airbnb.ca/s/Toronto--ON/homes?tab_id=home_tab&refinement_paths%5B%5D=%2Fhomes&checkin=2020-12-25&checkout=2020-12-31&adults=1&source=structured_search_input_header&search_type=pagination&place_id=ChIJpTvG15DL1IkRd8S0KlBVNTI&federated_search_session_id=8d51a375-784d-4289-9cd7-71c814c57cd1&items_offset="
V_Url <- "https://www.airbnb.ca/s/Vancouver--BC/homes?tab_id=home_tab&refinement_paths%5B%5D=%2Fhomes&checkin=2020-12-25&checkout=2020-12-31&adults=1&source=structured_search_input_header&search_type=pagination&place_id=ChIJs0-pQ_FzhlQRi_OBm-qWkbs&federated_search_session_id=fb6e2dbe-e517-434d-a822-fc6643a88d42&items_offset="
M_Url <- "https://www.airbnb.ca/s/Montreal--QC/homes?tab_id=home_tab&refinement_paths%5B%5D=%2Fhomes&checkin=2020-12-25&checkout=2020-12-31&adults=1&source=structured_search_input_header&search_type=pagination&place_id=ChIJDbdkHFQayUwR7-8fITgxTmU&federated_search_session_id=a0a7abeb-74d5-467a-8085-1245530ced5d&items_offset="
O_Url <- "https://www.airbnb.ca/s/Ottawa--ON/homes?tab_id=home_tab&refinement_paths%5B%5D=%2Fhomes&checkin=2020-12-25&checkout=2020-12-31&adults=1&source=structured_search_input_header&search_type=pagination&place_id=ChIJrxNRX7IFzkwR7RXdMeFRaoo&federated_search_session_id=2ef4a49d-8c71-49ee-8d91-97ea1b8a3cc5&items_offset="
Q_Url <- "https://www.airbnb.ca/s/Quebec--QC--Canada/homes?tab_id=home_tab&refinement_paths%5B%5D=%2Fhomes&checkin=2020-12-25&checkout=2020-12-31&adults=1&source=structured_search_input_header&search_type=pagination&place_id=ChIJk4jbBYqWuEwRAzro8GMtxY8&federated_search_session_id=a915beca-84d6-4959-bf2d-d1efa9a680ee&items_offset="
S_Url <- "https://www.airbnb.ca/s/Saskatoon--SK/homes?tab_id=home_tab&refinement_paths%5B%5D=%2Fhomes&checkin=2020-12-25&checkout=2020-12-31&adults=1&source=structured_search_input_header&search_type=pagination&place_id=ChIJK5ntR7_2BFMRkCZ3lTKeBAU&federated_search_session_id=4f5f38d6-028d-4403-9fa9-c773dee9f9af&items_offset="
Last_Url <- "&section_offset=3"
Calgary_Data <- vector()
Toronto_Data <- vector()
Vancouver_Data <- vector()
Montreal_Data <- vector()
Ottawa_Data <- vector()
Quebec_Data <- vector()
Saskatoon_Data <- vector()
for (i in seq(from=0,to=280,by=20))
{
  C_Url <- paste0(C_Url,i,Last_Url)
  Calgary_Data <- c(Calgary_Data,read_html(C_Url) %>%
                 html_nodes("._tmwq9g ") %>% 
                 html_text())
  T_Url <- paste0(T_Url,i,Last_Url)
  Toronto_Data <- c(Toronto_Data,read_html(T_Url) %>%
                 html_nodes("._tmwq9g ") %>% 
                 html_text())
  V_Url <- paste0(V_Url,i,Last_Url)
  Vancouver_Data <- c(Vancouver_Data,read_html(V_Url) %>%
                 html_nodes("._tmwq9g ") %>% 
                 html_text())
  M_Url <- paste0(M_Url,i,Last_Url)
  Montreal_Data <- c(Montreal_Data,read_html(M_Url) %>%
                 html_nodes("._tmwq9g ") %>% 
                 html_text())
  O_Url <- paste0(O_Url,i,Last_Url)
  Ottawa_Data <- c(Ottawa_Data,read_html(O_Url) %>%
                 html_nodes("._tmwq9g ") %>% 
                 html_text())
  Q_Url <- paste0(Q_Url,i,Last_Url)
  Quebec_Data <- c(Quebec_Data,read_html(Q_Url) %>%
                 html_nodes("._tmwq9g ") %>% 
                 html_text())
  S_Url <- paste0(S_Url,i,Last_Url)
  Saskatoon_Data <- c(Saskatoon_Data,read_html(S_Url) %>%
                 html_nodes("._tmwq9g ") %>% 
                 html_text())
  
  print(i)
}

#Combining the data of all cities vector into one vector
Data<-vector()
Data <- c(Calgary_Data,Toronto_Data,Vancouver_Data,Montreal_Data,Ottawa_Data,Quebec_Data,Saskatoon_Data)

#Creating City vector
City <- character()
for(i in 1:2100)
{  
  City[i]=case_when(
    i <= 300 ~ 'Calgary',
    i > 300 & i<=600 ~ 'Toronto',
    i > 600 & i<=900 ~ 'Vancouver',
    i > 900 & i<=1200  ~ 'Montreal',
    i > 1200 & i<=1500  ~ 'Ottawa',
    i > 1500 & i<=1800  ~ 'Quebec',
    i > 1800 & i<=2100  ~ 'Saskatoon' )
}

#Creating Property_Type vector
A<-str_split(Data," ")
B<-data.frame(First=character(),Second=character(),Third=character())
P_T<-data.frame(Property_Type=character())
for(i in 1:2100)
{
  B[i,] <- A[[i]][1:3]
}
P_T <- paste(B$First,B$Second,B$Third,sep=" ")
Property_Type <- (str_remove_all(P_T," in"))

#Creating Rating vector
R<-gsub(".*;", "", Data)
Rating<-as.numeric(substr(R,1,3))
Rating <- ifelse(is.na(Rating),2.5,Rating)

#Creating No_of_guests vector
X<- str_extract(Data,'.*guest.')
No_of_guests <- sub("(\\d)[^0-9]+$", "\\1", X)
No_of_guests <- as.integer(substr(No_of_guests, nchar(No_of_guests), nchar(No_of_guests)))
No_of_guests <- ifelse(No_of_guests==0,10,No_of_guests)

#Creating Bedrooms vector
Y<- str_extract(Data,'.*bedroom.')
Bedrooms <- sub("(\\d)[^0-9]+$", "\\1", Y)
Bedrooms <- as.integer(substr(Bedrooms, nchar(Bedrooms), nchar(Bedrooms)))
Bedrooms <- ifelse(is.na(Bedrooms),0,Bedrooms)

#Creating Beds vector
Z<- str_extract(Data,'.*bed.')
Beds <- sub("(\\d)[^0-9]+$", "\\1", Z)
Beds <- as.integer(substr(Beds, nchar(Beds), nchar(Beds)))
Beds <- ifelse(is.na(Beds),1,Beds)

#Creating Baths vector
W<- str_remove_all(str_remove_all(str_extract(Data,'.*bath.'),'shared'),'private')
W1 <- str_extract(W,'.{5}bath.')
Baths<-as.numeric(str_extract(W1, "\\d+\\.*\\d*"))
Baths<- ifelse(is.na(Baths),1,Baths)

#Creating Price vector
V<- str_extract(Data,'.\\$...')
Price<-as.numeric(str_remove_all(str_extract(V, "\\d+"),"D"))
Price <- ifelse(is.na(Price),0,Price)
Price <- ifelse(Price==0,mean(Price),Price)

#Creating Wifi vector
M<-str_detect(Data,"Wifi")
Wifi <- ifelse(M==TRUE,1,0)

#Creating Kitchen vector
K<-str_detect(Data,"Kitchen")
Kitchen <- ifelse(K==TRUE,1,0)

#Creating Free_Parking vector
FP <-str_detect(Data,"Free parking") 
Free_Parking <- ifelse(FP==TRUE,1,0)

#Creating Free_Cancellation vector
FC <-str_detect(Data,"Free cancellation") 
Free_Cancellation <- ifelse(FC==TRUE,1,0)

#Creating Self_Check_in vector
SC <-str_detect(Data,"Self check-in") 
Self_Check_In <- ifelse(SC==TRUE,1,0)

#Creating Heating vector
H <-str_detect(Data,"Heating") 
Heating <- ifelse(H==TRUE,1,0)

#Creating Washer vector
Wa <-str_detect(Data,"Washer") 
Washer <- ifelse(Wa==TRUE,1,0)

#Creating Dryer vector
Dr <-str_detect(Data,"Dryer") 
Dryer <- ifelse(Dr==TRUE,1,0)

#Creating Gym vector
Gm <-str_detect(Data,"Gym") 
Gym <- ifelse(Gm==TRUE,1,0)

#Creating Pool vector
Pl <-str_detect(Data,"Pool") 
Pool <- ifelse(Pl==TRUE,1,0)

#Combining all vectors together to form a dataframe
FDF <- data.frame(cbind(City,Property_Type,No_of_guests,Bedrooms,Beds,Baths,Rating,Wifi,Kitchen,Free_Parking,Free_Cancellation,Self_Check_In,Heating,Washer,Dryer,Pool,Gym,Price))

#Save the data frame as .rda file if want to visualization on different platform 
#save(FDF,file="C:/Users/Desktop/FDF.Rdata")

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
###### Data Visualization ######
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#


#Question 1:	What is the range of price in different cities of Canada?

col_names <- c("Calgary", "Montreal", "Ottawa","Quebec","Saskatoon","Toronto","Vancouver")
Summary_chart <- FDF %>% group_by(City,Property_Type)%>% summarise(Price = as.numeric(Price),.groups="drop") %>%
  ggplot(aes(x=City, y=Price, fill=City)) + 
  geom_boxplot(alpha=0.3) +scale_x_discrete(labels= col_names)+
  xlab("City")+ylab("Price(CAD/night)")+ggtitle("City based price range")+
  theme(plot.title = element_text(hjust = 0.5))+theme(legend.position="none")
ggplotly(Summary_chart)

#Question 2:	What is the average price of each type of property (listed in Airbnb) in different cities?

col_names <- c("Calgary", "Montreal", "Ottawa","Quebec","Saskatoon","Toronto","Vancouver")
FDF1 <- FDF %>% group_by(City,Property_Type)%>% summarise(Average_Price = mean(as.numeric(Price)),.groups="drop")%>%
ggplot( aes(x=City, y=Average_Price, fill=Property_Type))+
  geom_bar(stat="identity", position = "dodge")+scale_x_discrete(labels= col_names)+
  xlab("City")+ylab("Average Price(CAD/night)")+ggtitle("Property based Average Price in cities")+
  theme(legend.text=element_text(size=8),legend.margin = margin(1, 1, 1, 1))+ guides(fill = guide_legend(title=NULL))+ theme(plot.title = element_text(hjust = 0.5))
  ggplotly(FDF1)

#@@@@@@@@@@@@@@@@@@@@@@@@@#  
###### Data Analysis ######
#@@@@@@@@@@@@@@@@@@@@@@@@@#

#Question 3:	What factors are significant in predicting the renting price of a property for Airbnb listing?
  
City<-as.factor(City)
Property_Type <- as.factor(Property_Type)
Reg_FDF <- data.frame(cbind(City,Property_Type,No_of_guests,Bedrooms,Beds,Baths,Rating,Wifi,Kitchen,Free_Parking,Free_Cancellation,Self_Check_In,Heating,Washer,Dryer,Pool,Gym,Price))
fit <- lm(Price~.,Reg_FDF)
summary(fit)

#Removing the variables showing NA in the summary of above regression.
fit <- lm(Price~.-Dryer-Wifi,Reg_FDF)
summary(fit)