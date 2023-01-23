

# Tidy Data in R

In this lesson we will cover the basics of data in R and will do so from a somewhat opinionated viewpoint of "Tidy Data".  There are other paradigms and other ways to work with data in R, but focusing on Tidy Data concepts and tools (a.k.a., The Tidyverse) gets people to a productive place the quickest.  For more on data analysis using the Tidyverse, the best resource I know of is [R for Data Science](http://r4ds.had.co.nz).  The approaches we will cover are very much inspired by this book.

## Lesson Outline
- [Data in R: The data frame](#data-in-r-the-data-frame)
- [Reading in data](#reading-in-data)

## Exercises
- [Homework 3.1](#homework-31)

## Data in R: The data frame

Simply put, a data structure is a way for programming languages to handle storing information.  Like most languages, R has several structures (vectors, matrix, lists, etc.).  But R was originally built for data analysis, so the data frame, a spreadsheet like structure with rows and columns, is the most widely used and useful to learn first.  In addition, the data frame (or is it data.frame) is the basis for many modern R pacakges (e.g. the tidyverse) and getting used to it will allow you to quickly build your R skills.

*Note:* It is useful to know more about the different data structures such as vectors, lists, and factors (a weird one that is for catergorical data).  But that is beyond what we have time for.  The best source on this information, I think, is Hadley Wickham's [Data Structures Chapter in Advanced R](http://adv-r.had.co.nz/Data-structures.html).  

*Another note:* Data types (e.g. numeric, character, logcial, etc.) are important to know about too, but details are more than we have time for.  Take a look at the chapter on vectors in R for Data Science, in particular [Section 20.3](https://r4ds.had.co.nz/vectors.html#important-types-of-atomic-vector).  

*And, yet another note:* Computers aren't very good at math.  Or at least they don't deal with floating point data they way many would think.  First, any value that is not an integer, is considered a "Double."  These are approximations, so if we are looking to compare to doubles, we might not always get the result we are expecting.   Again, R4DS is a great read on this: [Section on Numeric Vectors](https://r4ds.had.co.nz/vectors.html#numeric).  But also see [this take from Revolution Analytics](https://blog.revolutionanalytics.com/2009/11/floatingpoint-errors-explained.html) and [techradar](https://www.techradar.com/news/computing/why-computers-suck-at-maths-644771/2). 

<details>
<summary>Does` 1.0 == 1.001`?</summary>
  

```r
1.0 == 1.001
```

```
## [1] FALSE
```

</details>  

<details>
<summary>Does` 1.0 == 1.0000000000000001`?</summary>
  

```r
1.0 == 1.0000000000000001
```

```
## [1] TRUE
```

</details>     
  

*Last note, I promise:* Your elementary education was wrong, at least about rounding. 

<details>
<summary>What do you get when you round `3.5`?</summary>
  

```r
round(3.5)
```

```
## [1] 4
```

</details>

<details>
<summary>What do you get when you round `2.5`?</summary>
  

```r
round(2.5)
```

```
## [1] 2
```

</details>

<details>
<summary>In other words...</summary>
In other words, if the last digit is a 5, and the digit we are rounding is even, the we round down. But, if the last digit is a 5, and the digit we around rounding is odd, the we round up. There are actually good reasons for this (think bias).  Read up on the IEEE 754 standard [rules on rounding](https://en.wikipedia.org/wiki/IEEE_754#Rounding_rules).  
</details>

### Build a data frame
Best way to learn what a data frame is is to look at one.  Let's now build a simple data frame from scratch with the `data.frame()` function.  This is mostly a teaching excercise as we will use the function very little in the exercises to come.  

First, let's create a new script to work on for these tidy data lessons and call it, "tidy_data.R" and save it in the "R" folder.


```r
# Our first data frame

my_df <- data.frame(names = c("joe","jenny","bob","sue"), 
                    age = c(45, 27, 38,51), 
                    knows_r = c(FALSE, TRUE, TRUE,FALSE), 
                    stringsAsFactors = FALSE)
my_df
```

```
##   names age knows_r
## 1   joe  45   FALSE
## 2 jenny  27    TRUE
## 3   bob  38    TRUE
## 4   sue  51   FALSE
```

That created a data frame with 3 columns (names, age, knows_r) and four rows.  For each row we have some information on the name of an individual (stored as a character/string), their age (stored as a numeric value), and a column indicating if they know R or not (stored as a boolean/logical).

If you've worked with data before in a spreadsheet or from a table in a database, this rectangular structure should look somewhat familiar.   One way (there are many!) we can access the different parts of the data frame is like:


```r
# Use the dollar sign to get a column
my_df$age
```

```
## [1] 45 27 38 51
```

```r
# Grab a row with indexing
my_df[2,]
```

```
##   names age knows_r
## 2 jenny  27    TRUE
```

At this point, we have:

- built a data frame from scratch
- seen rows and columns
- heard about "rectangular" structure
- seen how to get a row and a column

The purpose of all this was to introduce the concept of the data frame.  Moving forward we will use other tools to read in data, but the end result will be the same: a data frame with rows (i.e. observations) and columns (i.e. variables).

### The tibble

Another, very closely related concept to a data frame is the "tibble."  The pithy definition, is that a tibble is the `tidyverse` version of data frame.  It is a data frame, but with some additional features added.

For instance, our old school data frame:













