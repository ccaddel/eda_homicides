# Exploratory Data Analysis of Homicides

Exploratory Data Analysis (EDA) is the numerical and graphical examination of data characteristics and relationships before formal, rigorous statistical analyses are applied. EDA can lead to insights, which may uncover other questions, and eventually predictive models. It also is an important “line of defense” against bad data and is an opportunity to notice that your assumptions or intuitions about a data set are violated.

This report explores a dataset containing demographic information and other details of victims and perpetrators in about 640,000 homicides in the United States from 1980-2014. The data was obtained from the [Murder Accountability Project](http://www.murderdata.org/) which created this dataset from the FBI's Supplementary Homicide Report from 1976 to the present and Freedom of Information Act data on more than 22,000 homicides that were not reported to the Justice Department.

## Workflow Setup

Download and install R and R Studio, installing the following packages using the command line:
- `install.packages("ggplot2", dependencies = T)`
- `install.packages("knitr", dependencies = T)`
- `install.packages("dplyr", dependencies = T)`

Download the [Homicide Reports dataset](https://www.kaggle.com/murderaccountability/homicide-reports) and insert it into your working directory.

## Goals
- Understand the distribution of variables, checking for anomalies and outliers
- Quantifying and visualizing individual variables by using appropriate plots
- Explore variables to identify the most important ones and relationships within the dataset before building predictive models; calculate correlations, investigate conditional means
- Use powerful methods and visualizations for examining relationships among multiple variables, such as reshaping data frames and using aesthetics like color and shape to uncover more information


## Contribute

Find any typos? Have another resource you think should be included? Contributions are welcome.

First, fork this repository.

![](https://raw.githubusercontent.com/udacity/ud777-writing-readmes/master/images/fork-icon.png)

Next, clone this repository to your desktop to make changes.

`$ git clone https://github.com/ccaddel/eda_homicides.git`

`$ cd eda_homicides`

Once you've pushed changes to your local repository, you can issue a pull request by clicking on the green pull request icon.

![](https://raw.githubusercontent.com/udacity/ud777-writing-readmes/master/images/pull-request-icon.png)

Instead of cloning the repository to your desktop, you can also go to `README.md` in your fork on github.com, hit the Edit button (button with the pencil) to edit the file in your browser, then hit the `Propose file change` button, and finally make a pull request.
