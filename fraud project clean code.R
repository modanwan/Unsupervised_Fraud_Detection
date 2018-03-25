library(dplyr)
library(ggplot2)
library(stringr)

data=read.csv("NY.csv")
data12=data%>%
  mutate(rank1=0)



###### Use a small random subset of the data 
#sample=data[sample(nrow(data), 100000), ]


last=data%>%
  filter(RECORD>1000000 )


### select 12 main fileds 
data1=last%>%
  select(RECORD,FULLVAL,AVLAND,AVTOT,LTFRONT,LTDEPTH,BLDFRONT,BLDDEPTH,STORIES,TAXCLASS,BBLE,ZIP)


### create the BORO variable 
data1$BORO = str_extract(data1$BBLE, "[0-9]")


#### 1. missing values and zero values 

### deal with the missing value:
### only zip and stories has missing value(!is.na)
### But need to replace zero values in other variables(==0) 

###(1) Deal with the zero values in the specific condition
#If LTFRONT=LTDEPTH=0, set them to (30, 100)
#If BLDFRONT=BLDDEPTH=0, set them to (20, 40) 
#If STORIES=0, set it to average stories by TAXCLASS

non=data1%>%
  filter((LTFRONT==0)&(LTDEPTH==0))

for (i in seq(nrow(data1))) {
  if ((data1[i, "LTFRONT"]==0)&(data1[i, "LTDEPTH"]==0)){
    tc = as.character(data1[i, "TAXCLASS"])
    data1[i, "LTFRONT"] =30
    data1[i, "LTDEPTH"] =100
  }
  
}
#check for any zero values
non=data1%>%
  filter((BLDFRONT==0)&(BLDDEPTH==0))
###

for (i in seq(nrow(data1))) {
  if ((data1[i, "BLDFRONT"]==0)&(data1[i, "BLDDEPTH"]==0)){
    tc = as.character(data1[i, "TAXCLASS"])
    data1[i, "BLDFRONT"] =20
    data1[i, "BLDDEPTH"] =40
  }
  
}

##check for any zero values 
non=data1%>%
  filter((BLDFRONT==0)&(BLDDEPTH==0))
#(2)
#deal with the zero values in FULLVAL, AVLAND and AVTOT,LTFRONT,LTDEPTH,BLDFRONT,BLDDEPTH
# Use the field averages, and you can group by taxclass
# taxcalss's populated percent is 100
#a=data%>%
#group_by(TAXCLASS)%>%
#summarise(count=n())
#b=sum(a$count)

### calclate the average of variables group by taxclass( using total dataset)
average = data%>%
  group_by(TAXCLASS)%>%
  summarise(AVG_FULLVAL=mean(FULLVAL),
            AVG_AVLAND=mean(AVLAND),
            AVG_AVTOT=mean(AVTOT),
            AVG_LTFRONT=mean(LTFRONT),
            AVG_LTDEPTH=mean(LTDEPTH),
            AVG_BLDFRONT=mean(BLDFRONT),
            AVG_BLDDEPTH=mean(BLDDEPTH))
##FULLVAL
non_fullval=data1%>%
  filter(FULLVAL==0)

for (i in seq(nrow(data1))) {
  if ((data1[i, "FULLVAL"]==0)){
    tc = as.character(data1[i, "TAXCLASS"])
    data1[i, "FULLVAL"] = average[average$TAXCLASS == tc,]$AVG_FULLVAL
  }
  if ((data1[i, "AVLAND"]==0)){
    tc = as.character(data1[i, "TAXCLASS"])
    data1[i, "AVLAND"] = average[average$TAXCLASS == tc,]$AVG_AVLAND
  }
  if ((data1[i, "AVTOT"]==0)){
    tc = as.character(data1[i, "TAXCLASS"])
    data1[i, "AVTOT"] = average[average$TAXCLASS == tc,]$AVG_AVTOT
  }
  if ((data1[i, "LTFRONT"]==0)){
    tc = as.character(data1[i, "TAXCLASS"])
    data1[i, "LTFRONT"] = average[average$TAXCLASS == tc,]$AVG_LTFRONT
  }
  if ((data1[i, "LTDEPTH"]==0)){
    tc = as.character(data1[i, "TAXCLASS"])
    data1[i, "LTDEPTH"] = average[average$TAXCLASS == tc,]$AVG_LTDEPTH
  }
  if ((data1[i, "BLDFRONT"]==0)){
    tc = as.character(data1[i, "TAXCLASS"])
    data1[i, "BLDFRONT"] = average[average$TAXCLASS == tc,]$AVG_BLDFRONT
  }
  if ((data1[i, "BLDDEPTH"]==0)){
    tc = as.character(data1[i, "TAXCLASS"])
    data1[i, "BLDDEPTH"] = average[average$TAXCLASS == tc,]$AVG_BLDDEPTH
  }
}
#check for any zero value in specific variables
non1=data1%>%
  filter(BLDDEPTH==0)

#(3)deal with the missing value in the zip variables

# fill in missing values in ZIP with 00000
data1$ZIP = as.character(data1$ZIP)
data1[is.na(data1$ZIP),]$ZIP = "00000"
#(4) deal with the missing values in stories 

### calculate the stories average group by taxclass( using all dataset)

average_story = data %>%
  filter(!is.na(STORIES)) %>%
  group_by(TAXCLASS) %>%
  summarise(AVG_STORIES= mean(STORIES))

### fill the missing values in STORIES ( for subset)


for (i in seq(nrow(data1))) {
  if (is.na(data1[i,'STORIES'])){
    tc = as.character(data1[i, "TAXCLASS"])
    data1[i, 'STORIES'] = average_story[average_story$TAXCLASS == tc,]$AVG_STORIES
  }
}


# check for any NA values in STORIES
data2=data1%>%
  filter(is.na(STORIES))

##### No zero values in STORIES
non1=data%>%
  filter(STORIES==0)

#2. build variables 

#lotarea = LTFRONT * LTDEPTH
#bldarea = BLDFRONT * BLDDEPTH
#bldvol = bldarea * STORIES


data2=data1%>%
  mutate(lotarea=LTFRONT * LTDEPTH)%>%
  mutate(bldarea=BLDFRONT * BLDDEPTH)%>%
  mutate(bldvol=bldarea * STORIES)

### all zip has length of 5. So treat ZIP as ZIP5
data3=data1%>%
  group_by(ZIP)%>%
  summarise(count=n())

### create the zip3 variable 
data2=data1%>%
  mutate(ZIP3 = str_extract(data1$ZIP, "[0-9]{3}"))


saveRDS(total_cleaned_data,  "total_cleaned_data.rds")

non1=total_cleaned_data%>%
  filter(FULLVAL==0)

result=data%>%
  filter(RECORD %in% c(78804,5393,22921,294061,1046264,376243,508286,246251,81046,447396))

write.csv(result,"reslut.csv")


bldvol= BLDFRONT * BLDDEPTH * STORIES
lotarea=LTFRONT * LTDEPTH


data_clean=readRDS("NY_clean.rds")


