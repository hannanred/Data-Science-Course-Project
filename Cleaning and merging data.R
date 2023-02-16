setwd("C:/DataScienceProject")

patentdata<-list() #This will generate a list to store the patent data
y = 1
begin<-"Data/patentdata"  
end<-".txt"      #In order to loop through the different years of files, we used basic string manipulation
for (x in 1990:2014){
  mid = as.character(x)
  patentdata[[y]]<-read.delim(paste(begin,mid,end,sep=""),header=FALSE,sep="|",fill=TRUE,na.strings=TRUE)[,(1:8)]
patentdata[[y]]<-na.omit( patentdata[[y]]) #na.omit was used to remove NA values
  names(patentdata[[y]])<-c("Patent year","Patent Number","Assignee Name","City of first Inventor","Stateandzipcode of first inventor","Country of first inventor","class","subclass")
  y=y+1
}

#Now we will make a merged table from the list
DF<-do.call("rbind",patentdata)

#We will now add another variable, the different sectors in the US patents

list_of_fields<-read.csv("Data/list of 56 fields.csv",header=TRUE) #this .csv was made by us using the given .doc file list of 56 fields.doc
colnames(list_of_fields)[1] <- "FOOD.and.TOBACCO.PRODUCTS"
View(list_of_fields)
library(reshape2)

list_of_fields<-melt(list_of_fields,na.rm=TRUE)
names(list_of_fields)<-c("Sector","class")
View(list_of_fields)
list_of_fields=data.frame(list_of_fields)
library(dplyr)
list_of_fields<- list_of_fields %>% mutate(across(where(is.factor), as.character)) 
is.factor(list_of_fields[1])
CompiledPatentsTextWithSectors<-DF
CompiledPatentsTextWithSectors$Sector<-list_of_fields$Sector[match(CompiledPatentsTextWithSectors$class, list_of_fields$class)]
View(CompiledPatentsTextWithSectors)

#Next, we remove the columns for the assignee names, subclass and state and zip code since we will not be using them
CompiledPatentsTextWithSectors<-select(CompiledPatentsTextWithSectors,-c("Patent Number","subclass","class","Assignee Name","City of first Inventor","Stateandzipcode of first inventor"))
colnames(CompiledPatentsTextWithSectors)[2] <- "Country"
sum(is.na(CompiledPatentsTextWithSectors))
#There are some rows with NA values, so we will remove them
CompiledPatentsTextWithSectors<-na.omit(CompiledPatentsTextWithSectors)
CompiledPatentsTextWithSectors$"Country"<-as.factor(CompiledPatentsTextWithSectors$"Country")


DevelopedNations<-subset(CompiledPatentsTextWithSectors,Country == "NO" |Country == "IE"|Country == "CH"|Country == "DE"|Country == "SE"|Country == "AU"|Country == "NL"|Country == "DK"|Country == "FI"|Country == "GB"|Country == "SG"|Country == "BE"|Country == "NZ"|Country == "CA"|Country == "US"|Country == "SK"|Country == "IL"|Country == "JP"|Country == "FR"|Country == "IT") 
DevelopedNations<-subset(DevelopedNations, Sector == "PHARMACEUTICALS.AND.BIOTECHNOLOGY")

View(DevelopedNations)

listnations<-list("NO", "IE", "CH","DE","SE", "AU", "NL", "DK", "FI","GB","SG", "BE", "NZ", "CA", "US", "SK", "IL","JP", "FR","IT") 
m<-data.frame(matrix(ncol=3,nrow=500))
colnames(m)[1:3]<-c("Country","Year","Patents")
z=1
for(x in 1990:2014){
  for(y in 1:20){
    m[z,1] <- listnations[[y]]
m[z,3]<-nrow(filter(DevelopedNations, Country == listnations[[y]] & `Patent year`==x))
m[z,2]<- x
z=z+1
  }
}
DevelopedNations<-m


DevelopingNations<-subset(CompiledPatentsTextWithSectors,Country == "IN" |Country == "BD"|Country == "PK"|Country == "NP"|Country == "ZW"|Country == "UG"|Country == "NG"|Country == "AF"|Country == "KE"|Country == "LY"|Country == "LB"|Country == "KW"|Country == "MG"|Country == "KH"|Country == "CM"|Country == "SY"|Country == "PG"|Country == "NA"|Country == "BT"|Country == "CN") 
DevelopingNations<-subset(DevelopingNations, Sector == "PHARMACEUTICALS.AND.BIOTECHNOLOGY") 

listnations<-list("IN","BD", "PK", "NP", "ZW", "UG", "NG", "AF", "KE", "LY", "LB", "KW", "MG", "KH", "CM", "SY", "PG", "NA","BT","CN") 
m<-data.frame(matrix(ncol=3,nrow=500))
colnames(m)[1:3]<-c("Country","Year","Patents")
z=1
for(x in 1990:2014){
  for(y in 1:20){
    m[z,1] <- listnations[[y]]
    m[z,3]<-nrow(filter(DevelopingNations, Country == listnations[[y]] & `Patent year`==x))
    m[z,2]<- x
    z=z+1
  }
}

DevelopingNations<-m
View(DevelopedNations)
#Infant mortality Data

setwd("C:/DataScienceProject")
infantmortality<-read.csv("Data\\mortalityinfant\\mortalityinfant.csv",check.names = FALSE) #Contains data regarding infant mortality with country codes
View(infantmortality)
colnames(infantmortality)[1] <- "Country"
colnames(infantmortality)[2]<-"Country Code"
alphacodes<-"http://en.wikipedia.org/wiki/ISO_3166-1_alpha-2"
library(rvest)
codes<-read_html(alphacodes)%>%
  html_table(fill=TRUE)%>%       #We scrape the wikipedia page for the table which has country codes along with names
  .[[3]] 

infantmortality$"Country Code"<-codes$Code[match(infantmortality$Country,codes$`Country name (using title case)`)]


  
#For the rest, we will manually add the country codes

infantmortality[24,2] = "BS"
infantmortality[29,2] = "BO"
infantmortality[42,2] = "CI"
infantmortality[44,2] = "CD"
infantmortality[45,2] = "CG"
infantmortality[52,2] = "CW"
infantmortality[55,2] = "CZ"
infantmortality[68,2] = "EG"
infantmortality[80,2] = "FM"
infantmortality[82,2] = "GB"
infantmortality[87,2] = "GM"
infantmortality[97,2]="HK"
infantmortality[113,2] = "IR"
infantmortality[123,2] = "KG"
infantmortality[126,2] = "KN"
infantmortality[127,2] = "KR"
infantmortality[130,2] = "LA"
infantmortality[134,2] = "LC"
infantmortality[147,2] = "MO"
infantmortality[148,2]="MF"
infantmortality[151,2] = "MD"
infantmortality[172,2] = "NA"
infantmortality[194,2]="KP"
infantmortality[222,2] = "SK"
infantmortality[247,2] = "TZ"
infantmortality[252,2] = "US"
infantmortality[254,2] = "VC"
infantmortality[255,2] = "VE"
infantmortality[256,2] = "VG"
infantmortality[258,2] = "VN"
infantmortality[262,2]="XK"
infantmortality[263,2] = "YE"
infantmortality[267,2] = "TW"


infantmortality<-na.omit(infantmortality)


infantmortality$"Country Code" <- as.factor(infantmortality$"Country Code")

library(reshape2)
infantmortality<-melt(infantmortality,na.rm=FALSE)
colnames(infantmortality)[3:4]<-c("Year","Rate")
Developinginfant<-subset(infantmortality,infantmortality$'Country Code' == "IN" |infantmortality$'Country Code' == "BD"|infantmortality$'Country Code' == "PK"|infantmortality$'Country Code' == "NP"|infantmortality$'Country Code' == "ZW"|infantmortality$'Country Code' == "UG"|infantmortality$'Country Code' == "NG"|infantmortality$'Country Code' == "AF"|infantmortality$'Country Code' == "KE"|infantmortality$'Country Code' == "LY"|infantmortality$'Country Code' == "LB"|infantmortality$'Country Code' == "KW"|infantmortality$'Country Code' == "MG"|infantmortality$'Country Code' == "KH"|infantmortality$'Country Code' == "CM"|infantmortality$'Country Code' == "SY"|infantmortality$'Country Code' == "PG"|infantmortality$'Country Code' == "NA"|infantmortality$'Country Code' == "BT"|infantmortality$'Country Code' == "CN")
Developedinfant<-subset(infantmortality,infantmortality$'Country Code' == "NO" |infantmortality$'Country Code' == "IE"|infantmortality$'Country Code' == "CH"|infantmortality$'Country Code' == "DE"|infantmortality$'Country Code' == "SE"|infantmortality$'Country Code' == "AU"|infantmortality$'Country Code' == "NL"|infantmortality$'Country Code' == "DK"|infantmortality$'Country Code' == "FI"|infantmortality$'Country Code' == "GB"|infantmortality$'Country Code' == "SG"|infantmortality$'Country Code' == "BE"|infantmortality$'Country Code' == "NZ"|infantmortality$'Country Code' == "CA"|infantmortality$'Country Code' == "US"|infantmortality$'Country Code' == "SK"|infantmortality$'Country Code' == "IL"|infantmortality$'Country Code' == "JP"|infantmortality$'Country Code' == "FR"|infantmortality$'Country Code' == "IT")


write.csv(Developinginfant,"Data\\Developinginfant.csv", row.names = FALSE)
write.csv(Developedinfant,"Data\\Developedinfant.csv", row.names = FALSE)

#mortalityneonatal table

mortalityneonatal<-read.csv("Data\\mortalityneonatal\\mortalityneonatal.csv", check.names=FALSE) #Contains data regarding neonatal mortality with country codes
colnames(mortalityneonatal)[1] <- "Country"
mortalityneonatal$"Country Code"<-codes$Code[match(mortalityneonatal$Country,codes$`Country name (using title case)`)]



#For the rest, we will manually add the country codes

mortalityneonatal[24,2] = "BS"
mortalityneonatal[29,2] = "BO"
mortalityneonatal[42,2] = "CI"
mortalityneonatal[44,2] = "CD"
mortalityneonatal[45,2] = "CG"
mortalityneonatal[52,2] = "CW"
mortalityneonatal[55,2] = "CZ"
mortalityneonatal[68,2] = "EG"
mortalityneonatal[80,2] = "FM"
mortalityneonatal[82,2] = "GB"
mortalityneonatal[87,2] = "GM"
mortalityneonatal[97,2]="HK"
mortalityneonatal[113,2] = "IR"
mortalityneonatal[123,2] = "KG"
mortalityneonatal[126,2] = "KN"
mortalityneonatal[127,2] = "KR"
mortalityneonatal[130,2] = "LA"
mortalityneonatal[134,2] = "LC"
mortalityneonatal[147,2] = "MO"
mortalityneonatal[148,2]="MF"
mortalityneonatal[151,2] = "MD"
mortalityneonatal[172,2] = "NA"
mortalityneonatal[194,2]="KP"
mortalityneonatal[222,2] = "SK"
mortalityneonatal[247,2] = "TZ"
mortalityneonatal[252,2] = "US"
mortalityneonatal[254,2] = "VC"
mortalityneonatal[255,2] = "VE"
mortalityneonatal[256,2] = "VG"
mortalityneonatal[258,2] = "VN"
mortalityneonatal[262,2]="XK"
mortalityneonatal[263,2] = "YE"
mortalityneonatal[267,2] = "TW"


mortalityneonatal<-na.omit(mortalityneonatal)

mortalityneonatal$"Country Code" <- as.factor(mortalityneonatal$"Country Code")
mortalityneonatal<-melt(mortalityneonatal,na.rm=FALSE)
colnames(mortalityneonatal)[3:4]<-c("Year","Rate")

Developingneonatal<-subset(mortalityneonatal,mortalityneonatal$'Country Code' == "IN" |mortalityneonatal$'Country Code' == "BD"|mortalityneonatal$'Country Code' == "PK"|mortalityneonatal$'Country Code' == "NP"|mortalityneonatal$'Country Code' == "ZW"|mortalityneonatal$'Country Code' == "UG"|mortalityneonatal$'Country Code' == "NG"|mortalityneonatal$'Country Code' == "AF"|mortalityneonatal$'Country Code' == "KE"|mortalityneonatal$'Country Code' == "LY"|mortalityneonatal$'Country Code' == "LB"|mortalityneonatal$'Country Code' == "KW"|mortalityneonatal$'Country Code' == "MG"|mortalityneonatal$'Country Code' == "KH"|mortalityneonatal$'Country Code' == "CM"|mortalityneonatal$'Country Code' == "SY"|mortalityneonatal$'Country Code' == "PG"|mortalityneonatal$'Country Code' == "NA"|mortalityneonatal$'Country Code' == "BT"|mortalityneonatal$'Country Code' == "CN")
Developedneonatal<-subset(mortalityneonatal,mortalityneonatal$'Country Code' == "NO" |mortalityneonatal$'Country Code' == "IE"|mortalityneonatal$'Country Code' == "CH"|mortalityneonatal$'Country Code' == "DE"|mortalityneonatal$'Country Code' == "SE"|mortalityneonatal$'Country Code' == "AU"|mortalityneonatal$'Country Code' == "NL"|mortalityneonatal$'Country Code' == "DK"|mortalityneonatal$'Country Code' == "FI"|mortalityneonatal$'Country Code' == "GB"|mortalityneonatal$'Country Code' == "SG"|mortalityneonatal$'Country Code' == "BE"|mortalityneonatal$'Country Code' == "NZ"|mortalityneonatal$'Country Code' == "CA"|mortalityneonatal$'Country Code' == "US"|mortalityneonatal$'Country Code' == "SK"|mortalityneonatal$'Country Code' == "IL"|mortalityneonatal$'Country Code' == "JP"|mortalityneonatal$'Country Code' == "FR"|mortalityneonatal$'Country Code' == "IT")

Developingneonatal
Developedneonatal
write.csv(Developingneonatal,"Data\\Developingneonatal.csv", row.names = FALSE)
write.csv(Developedneonatal,"Data\\Developedneonatal.csv", row.names = FALSE)


#Life expectancy

lifeexpectancy<-read.csv("Data\\lifeexpectancy\\lifeexpectancy.csv", check.names=FALSE) #Contains data regarding neonatal mortality with country codes
colnames(lifeexpectancy)[1] <- "Country"
lifeexpectancy$"Country Code"<-codes$Code[match(lifeexpectancy$Country,codes$`Country name (using title case)`)]



#For the rest, we will manually add the country codes

lifeexpectancy[24,2] = "BS"
lifeexpectancy[29,2] = "BO"
lifeexpectancy[42,2] = "CI"
lifeexpectancy[44,2] = "CD"
lifeexpectancy[45,2] = "CG"
lifeexpectancy[52,2] = "CW"
lifeexpectancy[55,2] = "CZ"
lifeexpectancy[68,2] = "EG"
lifeexpectancy[80,2] = "FM"
lifeexpectancy[82,2] = "GB"
lifeexpectancy[87,2] = "GM"
lifeexpectancy[97,2]="HK"
lifeexpectancy[113,2] = "IR"
lifeexpectancy[123,2] = "KG"
lifeexpectancy[126,2] = "KN"
lifeexpectancy[127,2] = "KR"
lifeexpectancy[130,2] = "LA"
lifeexpectancy[134,2] = "LC"
lifeexpectancy[147,2] = "MO"
lifeexpectancy[148,2]="MF"
lifeexpectancy[151,2] = "MD"
lifeexpectancy[172,2] = "NA"
lifeexpectancy[194,2]="KP"
lifeexpectancy[222,2] = "SK"
lifeexpectancy[247,2] = "TZ"
lifeexpectancy[252,2] = "US"
lifeexpectancy[254,2] = "VC"
lifeexpectancy[255,2] = "VE"
lifeexpectancy[256,2] = "VG"
lifeexpectancy[258,2] = "VN"
lifeexpectancy[262,2]="XK"
lifeexpectancy[263,2] = "YE"
lifeexpectancy[267,2] = "TW"


lifeexpectancy<-na.omit(lifeexpectancy)
lifeexpectancy$"Country Code" <- as.factor(lifeexpectancy$"Country Code")
lifeexpectancy<-melt(lifeexpectancy,na.rm=FALSE)
colnames(lifeexpectancy)[3:4]<-c("Year","Rate")

Developinglifeexpectancy<-subset(lifeexpectancy,lifeexpectancy$'Country Code' == "IN" |lifeexpectancy$'Country Code' == "BD"|lifeexpectancy$'Country Code' == "PK"|lifeexpectancy$'Country Code' == "NP"|lifeexpectancy$'Country Code' == "ZW"|lifeexpectancy$'Country Code' == "UG"|lifeexpectancy$'Country Code' == "NG"|lifeexpectancy$'Country Code' == "AF"|lifeexpectancy$'Country Code' == "KE"|lifeexpectancy$'Country Code' == "LY"|lifeexpectancy$'Country Code' == "LB"|lifeexpectancy$'Country Code' == "KW"|lifeexpectancy$'Country Code' == "MG"|lifeexpectancy$'Country Code' == "KH"|lifeexpectancy$'Country Code' == "CM"|lifeexpectancy$'Country Code' == "SY"|lifeexpectancy$'Country Code' == "PG"|lifeexpectancy$'Country Code' == "NA"|lifeexpectancy$'Country Code' == "BT"|lifeexpectancy$'Country Code' == "CN")
Developedlifeexpectancy<-subset(lifeexpectancy,lifeexpectancy$'Country Code' == "NO" |lifeexpectancy$'Country Code' == "IE"|lifeexpectancy$'Country Code' == "CH"|lifeexpectancy$'Country Code' == "DE"|lifeexpectancy$'Country Code' == "SE"|lifeexpectancy$'Country Code' == "AU"|lifeexpectancy$'Country Code' == "NL"|lifeexpectancy$'Country Code' == "DK"|lifeexpectancy$'Country Code' == "FI"|lifeexpectancy$'Country Code' == "GB"|lifeexpectancy$'Country Code' == "SG"|lifeexpectancy$'Country Code' == "BE"|lifeexpectancy$'Country Code' == "NZ"|lifeexpectancy$'Country Code' == "CA"|lifeexpectancy$'Country Code' == "US"|lifeexpectancy$'Country Code' == "SK"|lifeexpectancy$'Country Code' == "IL"|lifeexpectancy$'Country Code' == "JP"|lifeexpectancy$'Country Code' == "FR"|lifeexpectancy$'Country Code' == "IT")

write.csv(Developinglifeexpectancy,"Data\\Developinglifeexpectancy.csv", row.names = FALSE)
write.csv(Developedlifeexpectancy,"Data\\Developedlifeexpectancy.csv", row.names = FALSE)


#Merging the datasets
View(DPC_P)
setwd("C:/Datascienceproject")
#Developing nations
DPC_P<- read.csv("Data/DevelopingNations.CSV",check.names=FALSE)
#Neonatal:
DPC_N<- read.csv("Data/DevelopingNeonatal.CSV",check.names = FALSE)
#Life expectancy:
DPC_L<- read.csv("Data/DevelopingLifeexpectancy.CSV",check.names = FALSE)
#Infant mortality
DPC_I<- read.csv("Data/Developinginfant.CSV",check.names = FALSE)
DPC_P[is.na(DPC_P)]="NA"
DPC_N[is.na(DPC_N)]="NA"                 #Namibias country code was missing after reading the data as the alpha2 code is 'NA' which R reads as a NA value, so we manually entered it
DPC_L[is.na(DPC_L)]="NA"
colnames(DPC_N)[1]="Country Name"
colnames(DPC_N)[2]="Country"
Dev1<-merge(x=DPC_P,y=DPC_N,by=c("Year","Country"))
colnames(Dev1)[5]="NeonatalRate"
colnames(DPC_L)[1]="Country Name"
library(dplyr)
DPC_L<-select(DPC_L,-c("Country Name"))
colnames(DPC_L)[1]="Country"
Dev1<-merge(x=Dev1,y=DPC_L,by=c("Year","Country"))
colnames(Dev1)[6]="Lifeexpectancy"

DPC_I<-select(DPC_I,-c("Country"))
colnames(DPC_I)[1]="Country"
Dev1<-merge(x=Dev1,y=DPC_I,by=c("Year","Country"))
colnames(Dev1)[7]="InfantMortality"

DevelopingNations<-Dev1
View(DevelopingNations)
write.csv(DevelopingNations,"Data\\DevelopingNations.CSV", row.names = FALSE)

#Developed Nations

DDC_P<- read.csv("Data/DevelopedNations.CSV",check.names = FALSE)
#Neonatal:
DDC_N<- read.csv("Data/DevelopedNeonatal.CSV",check.names = FALSE)
#Life expectancy:
DDC_L<- read.csv("Data/DevelopedLifeexpectancy.CSV",check.names = FALSE)
#Infant mortality
DDC_I<- read.csv("Data/Developedinfant.CSV",check.names = FALSE)

colnames(DDC_N)[1]="Country Name"
colnames(DDC_N)[2]="Country"
Dev1<-merge(x=DDC_P,y=DDC_N,by=c("Year","Country"))
colnames(Dev1)[5]="NeonatalRate"
colnames(DDC_L)[1]="Country Name"
library(dplyr)
DDC_L<-select(DDC_L,-c("Country Name"))
colnames(DDC_L)[1]="Country"
Dev1<-merge(x=Dev1,y=DDC_L,by=c("Year","Country"))
colnames(Dev1)[6]="Lifeexpectancy"

DDC_I<-select(DDC_I,-c("Country"))
colnames(DDC_I)[1]="Country"
Dev1<-merge(x=Dev1,y=DDC_I,by=c("Year","Country"))
colnames(Dev1)[7]="InfantMortality"

DevelopedNations<-Dev1

write.csv(DevelopedNations,"Data\\DevelopedNations.csv", row.names = FALSE)

