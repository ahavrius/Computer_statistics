

N<-60*8
n<-10
X<-sample(1:N, n)
M<-cbind(X%/%8+1, X%%8)
colnames(M)<-c("block","house")
rownames(M)<-1:n



Data<-data.frame(test_sample$V3,test_sample$V11, test_sample$V9, strtoi(test_sample$V19), stringsAsFactors = F)

1 2 3 4(1-13)       5 6 7           8    9     10 11     1  1 1 1 1  1  18
                                                         2  3 4 5 6  7
1 1 5 0002111000000 5 5  305600     0   400     0 306000 1  7 1 2 10 5 456000    .  442 4 1 3 4  1  1 52 200000 2 2 1 5  7  1 52  37000 
1 2 8 1001023100000 4 5  205105 85306   767     0 291178 1  5 1 2  9 5 374000    .  550 5 1 3 2  2  1 52  42367 2 2 3 2  9  1  .  42000 
1 3 4 0002011000000 3 4  182302 81698 10200     0 274200 1  2 1 1 10 4 430000    . 2218 4 1 1 7  2  1 52 200000 1 2 1 2  . 13  .  45000 
2 1 3 0100011000000 1 1  199600     0   400     0 200000 1  7 1 1 10 5 453000    . 2218 3 1 3 7  8  1 45 200000 1 2 3 3  . 13  .      0 
2 2 5 0300011000000 3 3  191990  1132  1200     0 194322 1  1 1 1  9 4 444000    . 2218 3 1 1 6  3  1 52 189504 1 2 1 3 10  1  4   2118 
2 3 4 0110011000000 2 2  174055  1024   800     0 175879 4  5 1 1  9 3 445000    . 2218 3 1 1 7  2  1 52 133853 1 2 1 5 10  1 52  42026 
3 1 3 0000021000000 3 3  156667     0     0     0 156667 1  2 1 2  7 3 390000    .  408 4 1 3 6  8  1 52  70757 1 2 3 3  8  1 52  70757 
3 2 5 0010022000000 4 4  149600   750   400     0 150750 1  6 1 1  8 3 230000    . 1153 3 1 1 4  3  1 52  58685 1 2 1 6 10  1 52  39350 
3 3 5 2000012000000 2 3   93000 55000   433     0 148433 1  3 1 2 10 5 388000    .  542 3 1 1 7  2  1 52  85433 1 2 1 7  3  1 26  50000 
4 1 4 0200011000000 2 2  142000     0   800     0 142800 1  2 1 1  8 4 380000    . 2218 3 1 3 7  2  1 52  72800 1 2 3 7 11  1 52  70000 
4 2 3 0001011000000 2 2  140800     0   100     0 140900 1  7 1 2  7 5 384000    .  383 4 1 1 7  1  1 52 140100 1 2 1 6  . 13  .      0 
4 3 2 0000011000000 1 1  140000     0     0     0 140000 1  8 1 2 10 3 275000    .  301 5 1 1 3  4  1 52 140000 2 2 1 5  . 13  .      0 
5 1 5 0120011000000 2 2  126306   200  1200     0 127706 1  5 1 1 10 4 195000    . 1320 3 2 1 3 10  1 52  24900 1 1 1 3  1  1 52 102806 
5 2 4 0011011000000 4 4  126000     0  3400     0 129400 1  7 1 1  9 4 240000    . 1391 4 1 1 5  2  1 52  50400 1 2 1 5  2  1 52  50000 
5 3 7 0310011000000 2 2  124391   718  2000     0 127109 1  5 1 1 10 5 365000    .  997 3 1 3 7  1  1 52  91768 1 2 5 7  3 11 36  35341 
6 1 1 0000001000000 1 1  120000     0     0     0 120000 2  6 1 2  8 2 451000    .  513 3 2 1 7  3 11 48 120000 . . . .  .  .  .      . 
6 2 3 1000011000000 2 2  119993     0   400     0 120393 1  4 1 1 10 3 170000    . 1051 2 1 1 7  2  1 48  46000 1 2 1 7  3  1 38  74393 
6 3 4 0110011000000 1 2   71370 47851   800     0 120021 1  5 1 1  9 4 428000    .  989 3 1 1 6 11  1 40  73599 1 2 1 7  . 13  .  46422 


INFILE statvil missover;
INPUT block unit
hhsize (hhpera hhperb1 hhperb2 hhperd1 hhperd2 hhpere1 hhpere2 hhperf1 hhperf2
        hhperg1 hhperg2 hhperh1 hhperh2) (1.0)                /* people in the household */
  nuempinh nuirh empinch invsth govinch otinch totinch  /* income variables */
  dtypeh builth tenurh morgh roomh broomh valueh grosrth omph /* house characteristic */
  hmage  hmsex  hmmtn  hmhlos  hmocc91  hmlfact  hmwkswk  hmempin 
shmage shmsex shmmtn shmhlos shmocc91 shmlfact shmwkswk shmempin;
LABEL HHSIZE    = 'Number of Persons in the Household'                           
HHPERA    = 'Number of Persons <5 Years of Age in the'                     
HHPERB1   = 'Number of Males in the Household at Home'                    
HHPERB2   = 'Num. of Females in the Household at Home'                    
HHPERD1   = 'Number of Males in the Household at Home'                    
HHPERD2   = 'Num. of Females in the Household at Home'                    
HHPERE1   = 'Number of Males in the Household at Home'                    
HHPERE2   = 'Num. of Females in the Household at Home'                    
HHPERF1   = 'Number of Males in the Household at Home'                    
HHPERF2   = 'Num. of Females in the Household at Home'                    
HHPERG1   = 'Number of Males in the Household at Home'                    
HHPERG2   = 'Num. of Females in the Household at Home'                    
HHPERH1   = 'Number of Males in the Household at Home'                    
HHPERH2   = 'Num. of Females in the Household at Home'                    
NUEMPINH  = 'Num. of Employment Income Recipients in'                    
NUIRH     = 'Number of Income recipients in Household'                      
EMPINCH   = 'Total Household Employment Income'                           
INVSTH    = 'Total Investment Income of Household'                         
GOVINCH   = 'Total Household Government Transfer Paym'                    
OTINCH    = 'All Other Household Income'                                   
TOTINCH   = 'Total Household Income'                                      
DTYPEH    = 'Structural Type of Dwelling'                                  
BUILTH    = 'Period of Construction'                                       
TENURH    = 'Tenure'                                                       
MORGH     = 'Presence of Mortgage'                                          
ROOMH     = 'Number of Rooms'                                               
BROOMH    = 'Number of Bedrooms'                                           
VALUEH    = 'Value of Dwelling'                                            
GROSRTH   = 'Monthly Gross Rent'                                          
OMPH      = 'Owner s Major Payments (Monthly)'                               
HMAGE     = 'Age of Primary Household Maintainer'                           
HMSEX     = 'Sex of Primary Household Maintainer'                           
HMMTN     = 'Mother Tongue of Primary Household Maint'                      
HMHLOS    = 'Highest Level of Schooling of Primary Hh'                     
HMOCC91   = 'Occupation (1991 SOC) of Primary Hhold M'                    
HMLFACT   = 'Labour Force Activity of Primary Hhold M'                    
HMWKSWK   = 'Weeks Worked in 1990 by Primary Hhold Ma'                    
HMEMPIN   = 'Total Employment Income of Primary Hhld'                     
SHMAGE    = 'Age of Spouse or C-L Partner of Primary'                      
SHMSEX    = 'Sex of Spouse or C-L Partner of Primary'                      
SHMMTN    = 'Mother Tongue of Spouse or CLP of Primar'                     
SHMHLOS   = 'Highest Level of Schooling of Spouse or'                     
SHMOCC91  = 'Occupation (1991 SOC) of Spouse or CLP o'                   
SHMLFACT  = 'Labour Force Activity of Spouse or CLP o'                   
SHMWKSWK  = 'Weeks Worked in 1990 by Spouse or CLP of'                   
SHMEMPIN  = 'Total Employment Income of Spouse or CLP';