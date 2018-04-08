# Spring2018


# Project 3: Collaborative Filtering Implementation and Evaluation

----


### [Project Description](doc/)

Term: Spring 2018

+ Project title: Memory and model-based collaborative filtering
+ Team Number: 4
+ Team Members: Wanting Cheng, Kelvin Kai, Joo Kim, Kenny Warner 
+ Project summary: 

For this project, we implemented and evaluated the performance of memory and model-based collaborative filtering algorithms on two datasets: Microsoft webpage data and movie data. Microsoft dataset contains a log of user visits to webpages, while movie data contains a list of movies that users have rated from 1 to 6.

For the memory-based algorithm, the estimate of items to be recommended is largely based on how other users are rating these items. In particular, we calculate the similarity "score" between each pair of users using different methodologies (pearson correlation, spearman correlation, cosine similarity, entropy, squared difference, simrank) and estimate how the active user would rate a given item based on other similar user's ratings on the given item. Having computed the similarity weights, we incorporated **significance weight** (Idea: devalue users with few commonly rated items) and **variance weight** (Idea: devalue items with low variance).

Model-based collaborative filtering involves developing and optimizing models to fit the data and then use the said models to make the predictions without having to rely on the entire dataset every time it is asked to make a prediction for a new user. Thus, it is more computationally efficient than the memory-based method. 

We initially cluster users into five groups based on "hidden" variables that inform their common webpage visit history or movie score. The Expectation-Maximization (EM) algorithm, an iterative method for finding the maximum likelihood estimates of parameters, was used to give probability distributions of a user belonging to a group. Once the user groups are finalized, their votes on the items are considered independent. 

Finally, we evaluate the performance of our recommendation algorithms using **Mean Absolute Error (MAE)** and **Ranked Scoring** metrics. MAE basically calculates how far our predictions deviate from the outcome and is used on the movie dataset. Ranked scoring is applied on MS dataset and uses a utility function to evaluate how satisfied the user will be if she sees our ranked list of recommended items, based on the real outcome.


Contribution statement: 

Wanting Cheng:
Kelvin Kai:
Joo Kim:
Kenny Warner: 

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
