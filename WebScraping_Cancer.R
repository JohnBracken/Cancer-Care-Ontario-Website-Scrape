#The following code performs a basic web scrape of the Cancer
#Care Ontario website.  The code extracts a list of the different
#kinds of cancer, in addition to the number of article links for
#each cancer type.  This information is then placed in a data frame
#and summarized concisely in a plot.


#Web scraping example from Ontario cancer statistics site.
#Load the rvest library.  Will also use the ggplot library
library(rvest)
library(ggplot2)

#URL of the Ontario Cancer site.
url <- 'https://www.cancercareontario.ca/en/cancer-facts'

#Read the HMTL from the site.
cancer_page <- read_html(url)

#Collect the cancer types and corresponding number of links
#from the item lists on the web page.  This item list
#identifier was determined using the CSS Selector Gadget,
#which is a free extension that can be installed in Google
#Chrome..
cancer_types<- html_nodes(cancer_page,
'.item-list')

#Convert all figure captions to text, so only take that.
text_cancers <- html_text(cancer_types)

#The list of cancer types is acutally only the first element
#in this character vector.
text_cancers <- text_cancers[1]

#Convert the list of cancers to a character vector,
#placing each kind of cancer in its own vector element.
cancer_list <- unlist(strsplit(text_cancers, "\n"))

#Remove all words after, and including, the word "Apply".
#Extraneous information we don't need.
cancer_list <- gsub("Apply.*", "", cancer_list)

#Seperate out the cancer types from the number of times a
#linked article is written for each type into separate 
#elements.
cancer_table_data <- unlist(strsplit(cancer_list, "\\("))

#We're going to need to sort out odd and even indices
#for the cancer table data.  The odd indices contain
#the cancer types, the even indexed elements contain
#the corresponding number of article links.  We're
#going to want to separate the cancer types from the
#number of article links into separate vectors.  These
#functions will be used to separate the odd and even
#indexed elements.
odd <- function(x) x%%2 != 0 
even <- function(x) x%%2 == 0 

#Get the odd index values.
odd_indices <- odd(1:length(cancer_table_data))

#Get the even index values.
even_indices <- even(1:length(cancer_table_data))

#The cancer types will be separate vector using the
#odd indices from the original cancer data.
cancer_types <- cancer_table_data[odd_indices]

#The number of article links will be a separate vector
#using the even indices from the original cancer data
#table.
article_counts <- cancer_table_data[even_indices]

#In the article link counts, we want to get rid of the remaining
#parentheses to be able to convert these values to numbers.
article_counts <- gsub("\\).*","", article_counts)

#Now convert the article link counts to integers.
article_counts <- as.integer(article_counts)

#Put the cancer types and corresponding article link counts
#into a data frame.
cancer_table <- data.frame(Cancer_Type = cancer_types, 
Article_Link_Counts = article_counts)

#Create and print a bar plot of the number of articles for each type of cancer.
cancer_plot<-ggplot(data=cancer_table, aes(reorder(Cancer_Type,-Article_Link_Counts),
Article_Link_Counts)) + geom_bar(stat="identity", col="black", fill = 'seagreen3') + 
labs(title = "Total Number of Articles per Cancer Type",x = "Cancer Type",
y= "Number of Articles") + theme(text = element_text(size=14),
axis.text.x = element_text(angle = 60, hjust = 1))

print(cancer_plot)