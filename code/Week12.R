install.packages('ggmap')

library('tidyverse')
library('ggmap')
library(readxl)

register_google(key='yourGoggleKey')   # 구글키  등록
has_google_key()


columns <- c( "상가업소번호", "상호명", "상권업종대분류명", "상권업종중분류명", 
              "상권업종소분류명", "시군구명", "행정동명", "경도", "위도")     

seoul_com <- NULL
filename <- paste("seoul_", "201712", ".xlsx", sep="")    # 파일 이름 만들기
seoul_com <- read_excel(filename)                            # 엑셀 파일 읽기
seoul_total <- data.frame(seoul_com)                       # 데이터프레임으로 변환
seoul_df <- seoul_total[,columns] 


samsung_df <- subset(seoul_df, seoul_df$행정동명 == "삼성동")


cen <- c(mean(samsung_df$경도),mean(samsung_df$위도))
cen

map <- get_googlemap(center=cen, 		        # 지도의 중심점 좌표
                     zoom=15, 				        # 지도 확대 정도
                     size=c(640,640), 			# 지도의 크기
                     maptype="roadmap")        # 지도의 유형
gmap <- ggmap(map)                              # 지도를 저장
gmap + geom_point(data = samsung_df, 
                  aes(x=경도, y=위도, color=상권업종대분류명), size=2, alpha=0.7) +
  labs(x = "Longitude", y = "Latitude",
       title="삼성동 업종별 점포",  color = "업종")





names <- c('부산 롯데월드', '죽성 성당', '젖병등대', '아홉산숲', '해동용궁사',
             '대변항', '연화리 해녀촌', '광안리해수욕장', '해운대 해수욕장', '부평시장')



adr <- c('부산 기장군 기장읍 동부산관광로 42', '부산광역시 기장군 기장읍 죽성리 134-7',
          '부산 기장군 기장읍 연화리 297-5', '부산 기장군 철마면 미동길 37-1', '부산 기장군 기장읍 용궁길 86',
         '부산 기장군 기장읍 대변리', '부산 기장군 기장읍 연화리 152-1', '부산 수영구 광안해변로 219',
         '부산 해운대구 우동', '부산광역시 중구 부평3길 29-16')

gc <- geocode(enc2utf8(adr))

df <- data.frame(name=names,
                 lon=gc$lon,
                 lat=gc$lat)

cen <- c(mean(df$lon),mean(df$lat))

map <- get_googlemap(center=cen, 	        
                     maptype="roadmap", 	
                     zoom=11, 			       
                     size=c(640,640), 		
                     marker=gc) 			     
ggmap(map) 		


gmap <- ggmap(map)

gmap + geom_text(data=df, 		      
                 aes(x=lon,y=lat), 	
                 size=5, 		        
                 label=df$name) 	  



songpa <- read.csv("project_songpa_data.csv", header=T)

g_m <- get_map("songpagu", zoom=13, maptype="roadmap")

song.map <- ggmap(g_m) + geom_point(data=songpa, aes(x=LON, y=LAT), size=2, 
                                    alpha=0.7, color="blue") +
  labs(title='송파구 CCTV 현황')
song.map





gc <- geocode(enc2utf8("한림대학교")) 		
cen <- as.numeric(gc) 

gm <- geocode(enc2utf8("강원 춘천시 삭주로66번길 9"))

map <- get_googlemap(center = cen, zoom=15,maptype = "roadmap", markers = gm)
ggmap(map)

gmap <- ggmap(map)

	  






















