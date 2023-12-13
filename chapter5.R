5-2
#读取数据
y<-c(8.2,7.5,6.8,4.9,6.5,3.6,9.0,6.2,5.3,4.7,5.4,3.9,7.8,6.6,5.8,5.8,6.3,4.0,8.0,
     7.5,5.4,5.1,6.7,3.2,9.0,5.6,6.3,4.6,6.3,3.4)
x<-c(rep(c("A","B","C","D","E","F"),5))
df1<-data.frame(x,y)

# 进行方差分析
model2 <- aov(y~x, data = df1)
result1 <- summary(model2)

# 进行多重比较
install.packages("agricolae")
library(agricolae)
result2<-LSD.test(model1,"y")

# 输出结果
方差分析<-result1
多重比较<-result2

方差分析
Df Sum Sq Mean Sq F value   Pr(>F)    
x            5  64.54  12.909    38.9 9.52e-11 ***
  Residuals   24   7.96   0.332                     
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

多重比较
$statistics
MSerror Df Mean       CV
0.3318333 24 5.98 9.632939

$parameters
test p.ajusted name.t ntr alpha
Fisher-LSD      none      y  24  0.05

$means
y std r        se      LCL      UCL Min Max Q25 Q50 Q75
3.2 3.2  NA 1 0.5760498 2.011092 4.388908 3.2 3.2 3.2 3.2 3.2
3.4 3.4  NA 1 0.5760498 2.211092 4.588908 3.4 3.4 3.4 3.4 3.4
3.6 3.6  NA 1 0.5760498 2.411092 4.788908 3.6 3.6 3.6 3.6 3.6
3.9 3.9  NA 1 0.5760498 2.711092 5.088908 3.9 3.9 3.9 3.9 3.9
4   4.0  NA 1 0.5760498 2.811092 5.188908 4.0 4.0 4.0 4.0 4.0
4.6 4.6  NA 1 0.5760498 3.411092 5.788908 4.6 4.6 4.6 4.6 4.6
4.7 4.7  NA 1 0.5760498 3.511092 5.888908 4.7 4.7 4.7 4.7 4.7
4.9 4.9  NA 1 0.5760498 3.711092 6.088908 4.9 4.9 4.9 4.9 4.9
5.1 5.1  NA 1 0.5760498 3.911092 6.288908 5.1 5.1 5.1 5.1 5.1
5.3 5.3  NA 1 0.5760498 4.111092 6.488908 5.3 5.3 5.3 5.3 5.3
5.4 5.4   0 2 0.4073287 4.559315 6.240685 5.4 5.4 5.4 5.4 5.4
5.6 5.6  NA 1 0.5760498 4.411092 6.788908 5.6 5.6 5.6 5.6 5.6
5.8 5.8   0 2 0.4073287 4.959315 6.640685 5.8 5.8 5.8 5.8 5.8
6.2 6.2  NA 1 0.5760498 5.011092 7.388908 6.2 6.2 6.2 6.2 6.2
6.3 6.3   0 3 0.3325825 5.613583 6.986417 6.3 6.3 6.3 6.3 6.3
6.5 6.5  NA 1 0.5760498 5.311092 7.688908 6.5 6.5 6.5 6.5 6.5
6.6 6.6  NA 1 0.5760498 5.411092 7.788908 6.6 6.6 6.6 6.6 6.6
6.7 6.7  NA 1 0.5760498 5.511092 7.888908 6.7 6.7 6.7 6.7 6.7
6.8 6.8  NA 1 0.5760498 5.611092 7.988908 6.8 6.8 6.8 6.8 6.8
7.5 7.5   0 2 0.4073287 6.659315 8.340685 7.5 7.5 7.5 7.5 7.5
7.8 7.8  NA 1 0.5760498 6.611092 8.988908 7.8 7.8 7.8 7.8 7.8
8   8.0  NA 1 0.5760498 6.811092 9.188908 8.0 8.0 8.0 8.0 8.0
8.2 8.2  NA 1 0.5760498 7.011092 9.388908 8.2 8.2 8.2 8.2 8.2
9   9.0   0 2 0.4073287 8.159315 9.840685 9.0 9.0 9.0 9.0 9.0

$comparison
NULL

$groups
y groups
9   9.0      a
8.2 8.2     ab
8   8.0    abc
7.8 7.8    abc
7.5 7.5     bc
6.8 6.8    bcd
6.7 6.7   bcde
6.6 6.6   bcde
6.5 6.5   cdef
6.3 6.3    def
6.2 6.2   defg
5.8 5.8   defg
5.6 5.6  defgh
5.4 5.4  defgh
5.3 5.3 defghi
5.1 5.1 efghij
4.9 4.9 fghijk
4.7 4.7 ghijkl
4.6 4.6 ghijkl
4   4.0  hijkl
3.9 3.9   ijkl
3.6 3.6    jkl
3.4 3.4     kl
3.2 3.2      l

attr(,"class")
[1] "group"

5-3
#读取数据
conc<-c(rep(c("M1","M2","M3","M4"),3))
type<-c(rep(c("P1"),4),rep(c("P2"),4),rep(c("P3"),4))
gr<-c(89.3,92.1,94.7,96.8,94.2,96.1,97.3,95.7,91.1,93.5,95.9,96.2)
df2<-data.frame(conc,type,gr)

#进行方差分析
model2<-aov(gr~conc+type,data = df2)
result1<-summary(model2)

# 进行多重比较
result2<-LSD.test(model2,"conc")

# 输出结果
方差分析<-result1
多重比较<-result2$groups

方差分析
Df Sum Sq Mean Sq F value Pr(>F)  
conc         3  42.85  14.283   8.029 0.0160 *
  type         2  13.85   6.923   3.892 0.0825 .
Residuals    6  10.67   1.779                 
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

多重比较
gr groups
M4 96.23333      a
M3 95.96667      a
M2 93.90000     ab
M1 91.53333      b

5-4
#读取数据
cs<-c(rep(c("A1","A2","A3","A4"),12))
pot<-c(rep(c("B11","B12","B13","B21","B22","B23","B31","B32","B33","B41","B42",
             "B43"),4))
y<-c(50,35,45,50,55,55,85,65,70,60,60,65,55,35,40,45,60,45,60,70,70,55,85,65,40,
     30,40,50,50,65,90,80,70,35,45,85,35,40,50,45,50,55,85,65,70,70,75,75)
df3<-data.frame(cs,pot,y)

#进行方差分析
model3<-aov(y~cs+pot+cs*pot,data = df3)
result1<-summary(model3)

# 输出结果
方差分析<-result1

方差分析
Df Sum Sq Mean Sq F value  Pr(>F)    
cs           3   1839   613.0   6.883 0.00088 ***
  pot          8   6550   818.8   9.193 8.7e-07 ***
  Residuals   36   3206    89.1                    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 

5-5
#读取数据
breed<-c(rep(c("A1"),9),rep(c("A2"),9),rep(c("A3"),9))
density<-c(rep(c("B1"),3),rep(c("B2"),3),rep(c("B3"),3),rep(c("B1"),3),rep(c("B2
          "),3),rep(c("B3"),3),rep(c("B1"),3),rep(c("B2"),3),rep(c("B3"),3))
block<-c(rep(c("R1","R2","R3"),9))
y<-c(8,8,8,7,7,6,6,5,6,9,9,8,7,9,6,8,7,6,7,7,6,8,7,8,10,9,9)
df4<-data.frame(block,breed,density,y)

#方差分析
model3<-aov(y~block+breed+density+breed:density,df4)
result1<-summary(model3)
result2<-LSD.test(model3,"breed")
result3<-LSD.test(model3,"density")

#输出结果
方差分析<-result1

方差分析
Df Sum Sq Mean Sq F value   Pr(>F)    
block          2  2.889   1.444   2.971  0.07991 .  
breed          2  6.222   3.111   6.400  0.00907 ** 
  density        3  1.639   0.546   1.124  0.36892    
breed:density  3 22.139   7.380  15.181 6.11e-05 ***
  Residuals     16  7.778   0.486                     
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

5-6
#读取数据
breed<-c(rep(c("A"),15),rep(c("B"),15),rep(c("C"),15),rep(c("D"),15))
pot<-c(rep(c("p1","p2","p3"),5),rep(c("p4","p5","p6"),5),rep(c("p7","p8","p9"),
      5),rep(c("p10","p11","p12"),5))
y<-c(50,55,40,35,35,30,45,40,40,55,40,35,35,30,40,50,45,50,55,60,50,55,45,65,
     45,50,45,60,50,50,85,60,90,65,70,80,70,70,70,60,90,85,70,80,65,60,55,35,
     60,85,45,65,65,85,55,35,70,85,45,75)
df5<-data.frame(breed,pot,y)
install.packages("HH")
library(HH)

#方差分析
model4<-aov(y~breed+pot+breed*pot,data=df5)
result1<-summary(model4)

#输出结果
方差分析<-result1
方差分析
Df Sum Sq Mean Sq F value   Pr(>F)    
breed        3   9208  3069.4  23.129 2.09e-09 ***
  pot          8    470    58.8   0.443    0.889    
Residuals   48   6370   132.7                     
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

5-7
#读取数据
place<-c(rep(c("first"),15),rep(c("second"),15),rep(c("third"),15))
breed<-c(rep(c("A","B","C","D","E"),3),rep(c("A","B","C","D","E"),3),rep(c("A"
               ,"B","C","D","E"),3))
block<-c(rep(c("one"),5),rep(c("two"),5),rep(c("three"),5),rep(c("one"),5),rep
         (c("two"),5),rep(c("three"),5),rep(c("one"),5),rep(c("two"),5),rep(c(
           "three"),5))
y<-c(19.7,28.6,20.3,27.9,22.3,31.4,38.3,27.5,40.0,30.8,29.6,43.5,32.6,46.1,31.1,
     40.8,44.4,44.6,39.8,71.5,29.4,34.9,41.4,39.2,47.6,30.2,33.9,26.2,29.1,55.4,
     34.7,28.8,29.8,27.2,43.0,29.1,28.7,38.4,27.6,32.7,35.1,21.0,28.0,20.4,32.0)
df6<-data.frame(place,breed,block,y)
#方差分析
model5<-aov(y~block+place+breed+place:breed,df6)
result1<-summary(model5)
result2<-LSD.test(model5,"place")
result3<-LSD.test(model5,"breed")

#输出结果
方差分析<-result1

方差分析
Df Sum Sq Mean Sq F value   Pr(>F)    
block        2   31.4    15.7   0.313 0.733635    
place        2  944.1   472.1   9.414 0.000747 ***
  breed        4  523.0   130.7   2.607 0.057034 .  
place:breed  8 1306.9   163.4   3.258 0.009481 ** 
  Residuals   28 1404.0    50.1                     
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
 
5-8
#读取数据
block<-c(rep(c("one"),12),rep(c("two"),12),rep(c("three"),12))
time<-c(rep(c("A1"),4),rep(c("A2"),4),rep(c("A3"),4),rep(c("A1"),4),rep(c("A2")
        ,4),rep(c("A3"),4),rep(c("A1"),4),rep(c("A2"),4),rep(c("A3"),4))
measure<-c(rep(c("B1","B2","B3","B4"),9))
y<-c(29,37,18,17,30,31,15,16,28,31,13,13,28,32,14,16,29,28,13,12,27,28,14,15,32,
     31,17,15,25,29,10,12,26,31,11,13)
df7<-data.frame(time,measure,block,y)

#方差分析
model7<-aov(y~block+time+measure+time*measure,data = df7)
result1<-summary(model7)

#输出结果
方差分析<-result1

方差分析
Df Sum Sq Mean Sq F value   Pr(>F)    
block         2   32.7    16.3   5.557 0.011133 *  
  time          2   72.0    36.0  12.247 0.000266 ***
  measure       3 2179.7   726.6 247.179  < 2e-16 ***
  time:measure  6    6.0     1.0   0.340 0.908024    
Residuals    22   64.7     2.9                     
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

5-9
#读取数据
feed<-c(rep(c("A1"),8),rep(c("A2"),8),rep(c("A3"),8))
inital_weight<-c(15,13,11,12,12,16,14,17,17,16,18,18,21,22,19,18,22,24,20,23,25,
                 27,30,32)
final_weight<-c(85,83,65,76,80,91,84,90,97,90,100,95,103,106,99,94,89,91,83,95,
                100,102,105,110)
df8<-data.frame(feed,inital_weight,final_weight)
#方差分析
df8[,1]<-as.factor(df8[,1])
model9<-ancova(final_weight~inital_weight+feed,data.in=df8)
result1<-summary(model9)

#输出结果
方差分析<-result1

方差分析
Analysis of Variance Table

Response: final_weight
Df  Sum Sq Mean Sq F value    Pr(>F)    
inital_weight  1 1621.12 1621.12 142.445 1.496e-10 ***
  feed           2  707.22  353.61  31.071 7.322e-07 ***
  Residuals     20  227.61   11.38                      
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1