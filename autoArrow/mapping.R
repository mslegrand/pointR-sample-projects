library(svgR)
library(tidyverse)
WH<-c(800,600)

# 1. ensure that the attribute preprocessor for circle$id
#    is set to 'fromtolinking' 
# 2. When a new circle is created via a mouse click, 
#     an id will be generated for that circle
# 3. To create an arrow connecting a two circles: 
#     i. Select the id of first circle while pressing the 'a' key
#     ii. Select the id of second circle while pressing the 'b' key
#  note: if step ii is not done, the arrow will point to the last to circle

# Defined by mouse: edit with care!
ptR<-list(
  circle= tribble(
      ~id,         ~fill,       ~cxy,
      'pkztfnpg',  '#EAFF00',   matrix( c(c(527,130)), 2),
      'tqtyjzpm',  '#0CC9C9',   matrix( c(c(420,158)), 2),
      'uykypkap',  '#0CC9C9',   matrix( c(c(299,159)), 2),
      'udrtlyzw',  '#186332',   matrix( c(c(352,63)), 2),
      'chmnkvnq',  '#FFAE00',   matrix( c(c(508,293)), 2),
      'rdkqglru',  '#FF0000',   matrix( c(c(164,77)), 2),
      'pkztfnpg',  '#0011FF',   matrix( c(c(79,261)), 2)
  ),
  links= tribble(
      ~fromId,     ~toId,       ~points,
      'pkztfnpg',  'udrtlyzw',  matrix(0,2,0),
      'uykypkap',  'tqtyjzpm',  matrix(0,2,0)
  )
)

arrs<-ptR$links %>% 
  inner_join( ptR$circle, by=c('fromId'='id') )%>%
  inner_join( ptR$circle, by=c('toId'='id') ) %>%
  select(c(5,7)) %>% 
  rename(p1=cxy.x, p2=cxy.y) 
 

R=40
 

arrow%<c-%function(p1, p2, R=50){
  v=p2-p1
  L=sqrt(sum(v^2))
  xy1=p1 + v*(R/L)
  xy2=p2 - v*((10+R)/L)
  line(xy1=xy1, xy2=xy2, 
    stroke='black', stroke.width=2, 
    marker.end=marker(viewBox=c(0, 0, 10, 10), refXY=c(1,5), 
      markerWidth=6, markerHeight=6, orient="auto",
      path( d=c("M", 0, 0, "L", 10, 5, "L", 0, 10, "z") )
    ) 
  )
}


svgR(wh=WH,
    pmap(ptR$circle, circle, r=R, opacity=.5),
    pmap(arrs, arrow )
)

