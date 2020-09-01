library(svgR)
library(tidyverse)
WH<-c(800,800) 

source('develObjects.R') 

#Defined by mouse: edit with care!
ptR<-list(
  source= tribble(
      ~label,    ~points,
      'source',  matrix( c(c(120,110),c(214,210)), 2),
      'source',  matrix( c(c(118,483),c(218,583)), 2)
  ),
  observer= tribble(
      ~points,
      matrix( c(c(594,496),c(673,411)), 2),
      matrix( c(c(581,255),c(681,105)), 2)
  ),
  expression= tribble(
      ~points,
      matrix( c(c(358,434),c(497,499)), 2),
      matrix( c(c(327,250),c(425,316)), 2)
  ),
  arrows= tribble(
      ~points,
      matrix( c(c(227,161),c(263,161),c(266,282),c(321,285)), 2),
      matrix( c(c(226,526),c(285,526),c(277,463),c(356,465)), 2),
      matrix( c(c(226,526),c(285,526),c(277,463),c(356,465)), 2),
      matrix( c(c(422,280),c(465,179),c(578,179)), 2),
      matrix( c(c(421,286),c(563,314),c(611,418)), 2),
      matrix( c(c(500,462),c(554,483),c(591,458)), 2),
      matrix( c(c(501,448),c(530,336),c(585,212)), 2)
  ),
  block= tribble(
      ~points,
      matrix( c(c(49,46),c(742,650)), 2)
  ),
  callOuts= tribble(
      ~txt,           ~pt,            ~points,
      'hello world',  1,              matrix( c(c(177,43),c(315,72)), 2)
  )
)

svgR(wh=WH ,
    #your custom code goes here 
    pmap(ptR$callOuts, callout),
    pmap(ptR$source, reactiveSource, fill='yellow'),
    pmap(ptR$observer, reactiveObserver),
    pmap(ptR$expression, reactiveExpression),
    pmap(ptR$arrows, arrow),
    pmap(ptR$block, block)
    
)





