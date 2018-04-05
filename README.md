# Movies_Dashboard

Developed a Movies Dashboard featuring User-based Collaborative filtering recommender (CFR) system, 
Sentiment Analysis of Movies and information on top movies. 
1. User-based Collaborative filtering recommender (CFR) system for suggesting movies to the users. 
User-based collaborative filtering was used, ie, to predict a user’s opinion for an item using the opinion of similar users. 
Two datasets were used - Movies dataset consisting of over 9000 rows and contained information about movies Ratings dataset 
consisting more than 10,000 rows and contained the ratings of various users for movies. 
Steps involved - 
* Data preprocessing - The datasets were cleaned. Any missing values were omitted and data was organized in a way that if 
helped to search movies.
* Various methods to recommend movies were explored and User-based collaborative filtering was selected.
* Exploring Similarity data - Recommenderlab contains similarity function which was used to find similarity between users.
* Data preparation - The data was normalized.
* Prediction - Finally the prediction was done based on User-based collaborative filtering

2. Sentiment Analysis of Movies - 
* Extracted tweets about the movie which user gives as input using Twitter using twitteR package
* Cleaned the tweets from stop words, special characters and converted them to lower case for analysis using tm library
* Created word clouds for the movie to depict the words associated with it using wordcloud library
* Analyzed the tweets, whether they are positive or negative and calculated the sentiment score based on the number of positive 
and negative words using score sentiment function.
* Depicted the sentiment by plotting histogram using lattice library

3. Information on top movies - Displayed information on top movies ranging from year 2016 to 2010. Information contained genre, 
director name, actor name, ratings etc
