#习题2.2
#c(3,2,1)表示重复的对象，times函数后的表示重复的次数
x <- rep(c(3, 2, 1), times = c(3, 4, 5))
show(x)
#output:[1] 3 3 3 2 2 2 2 1 1 1 1 1
#习题2.3
#从0开始，i遍历1到100,如果i能整除3或5,即余数为0,则使用cat函数取出i，通过sum函数计算总加数值，最后
用print函数输出结果
sum<- 0 
for (i in 1:100) {
  if (i %% 3 == 0 || i %% 5 == 0) {
    cat(i," ")
    sum<-sum+i
  }
}
print(sum)
#output：3  5  6  9  10  12  15  18  20  21  24  25  27  30  33  35  36  39  40  42 45  48  50  51  54  55  57  60  63  65  66  69  70  72  75  78  80  81  84  85  87  90  93  95  96  99  100  > print(sum)
[1] 2418

#习题2.4
#2.4.1
#调用自带的iris数据集
data(iris)
#用seq_len函数量取iris的行数，并赋给ID列
iris$ID<-seq_len(nrow(iris))
#2.4.2
#用subset函数提取iris数据集中满足这里两个条件的数据，赋给a
a<-subset(iris,Species == "setosa" & Sepal.Length>5)
#展示数据
show(a)
#2.4.3
#用order函数对a数据集中的Sepal.Length排序，并赋值给b
b<-a[order(a$Sepal.Length), ]
#输出结果
print(b)
#2.4.4
#提取iris数据集中的同时满足Species == "setosa" & Sepal.Width>3的数据
c<-subset(iris,Species == "setosa" & Sepal.Width>3)
print(c)
#2.4.5
#用merge函数合并b,c数据集，设置以ID来合并
d<-merge(b,c,by="ID",all = TRUE)
#output:  ID Sepal.Length.x Sepal.Width.x Petal.Length.x Petal.Width.x Species.x Sepal.Length.y Sepal.Width.y
1  1            5.1           3.5            1.4           0.2    setosa            5.1           3.5
2  3             NA            NA             NA            NA      <NA>            4.7           3.2
3  4             NA            NA             NA            NA      <NA>            4.6           3.1
4  5             NA            NA             NA            NA      <NA>            5.0           3.6
5  6            5.4           3.9            1.7           0.4    setosa            5.4           3.9
6  7             NA            NA             NA            NA      <NA>            4.6           3.4
Petal.Length.y Petal.Width.y Species.y
1            1.4           0.2    setosa
2            1.3           0.2    setosa
3            1.5           0.2    setosa
4            1.4           0.2    setosa
5            1.7           0.4    setosa
6            1.4           0.3    seto
#习题2.5
#2.5.1
#创建一个数据集，为成绩表并用rowSums函数和rowMeans函数分别给出总成绩和平均值，分别作为单独的列加入到成绩表

grade_table <- data.frame(
  Math = c(93, 77, 91, 88, 92, 97, 92, 60, 76, 60),
  Chinese = c(71, 94, 88, 84, 79, 79, 90, 82, 80, 87),
  English = c(64, 83, 65, 96, 77, 64, 96, 65, 83, 66)
  )
grade_table$total_grades<-rowSums(grade_table[,c("Math","Chinese","English")])
grade_table$average_grades<-rowMeans(grade_table[,c("Math","Chinese","English")])
head(grade_table)
#output:  Math Chinese English total_grades average_grades
1   93      71      64          228       76.00000
2   77      94      83          254       84.66667
3   91      88      65          244       81.33333
4   88      84      96          268       89.33333
5   92      79      77          248       82.66667
6   97      79      64          240       80.00000
#2.5.2
# 设置一个自定义函数rank，用if结构判断成绩情况，由于题意不清，给出以下评级方案，有一门等于60作为不合格
grade_rank<-function(rank) {
  if (all(rank>80)){
    return("good")
  } else if (all(rank>=60) && any(rank<80)) { 
    return("passed")
  } else {
    return("failed")
  }
}
grade_table$Grade <- apply(grade_table, 1,grade_rank)
head(grade_table)
#output：
maths chinese en  Grade
1    93      71 64 passed
2    77      94 83 passed
3    91      88 65 passed
4    88      84 96   good
5    92      79 77 passed
6    97      79 64 passed