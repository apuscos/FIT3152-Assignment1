# FIT3152-Assignment1
Personal assignment

## Team member
* Gao Wang
* Yulu Chen

## Data attributes explaination
1. PostID  
  每一个发布都有的一个特殊的ID
2. ThreadID  
  一组Post(类似一个主题的ID), 一个ThreadID有多数PostID而且PostID在一个小范围内连续
3. AuthorID  
  发布者的ID 同一个发布者可以有多个Post(PostID) 所以一个Thread可以有一个作者发布的多个Post
4. Date     
  Post日期
5. Time     
  Post日期对应的时间
6. WC        
  Post对应的字数 (Word count)
7. Analytic        
  低Analytic表明写和想用更叙事的语言。高Analtic表明写和想用更学术
8. Clout        
  通过言谈表现出来的社会地位，自信，领导力（权利，力量和影响力）
9. Authentic    
  可靠性，真实性
10. Tone  
  值越高，表达的感情(对事物)音色就越正面
11. ppron   
  所有的人称代词占比
12. i, we, you, shehe, they   
  第一人称单数,第一人称复数，第二人称单数和复数，第三人称单数，第三人称复数的占比。这些数值加起来等于ppron或约等于（近似误差）
13. numbers   
  Post中数字文字的数量
14. affect  
  情绪用词的占比
15. posemo, negemo    
  正面情绪的占比和负面情绪的占比 affect约等于posemo + negemo
16. anx, anger, social, family, friend, work, leisure, home, money, relig, swear   
  表达焦虑，生气，社会性，家人，朋友，工作，空闲，家，金钱，宗教，宣誓的词汇占比，其中部分可归类为posemo和negemo
17. QMark   
  问号的数量
  
## Target
  验证相互交流时人们采用相似模式的语言在在线论坛
  
## Questions
* 在同一个thread人们是不是用相似的语言交流
* 在一个thread用一种模式交流，在另一个thread是否同样用相似的模式
* 使用的语言（各个方面）是否随时间改变
  
## The process of data analysis
  1. Data requirments. See above
  2. Data collection. Download from moodle
  3. Data processing.  using "read.csv" to import data
  4. Data cleaning. Data is cleaning
  5. Exploratory data analysis




