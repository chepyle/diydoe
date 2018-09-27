# Do it yourself Design of Experiments : DIYDoE
## Jacob Albrecht June 2018

This app is a first attempt at an R/Shiny application to facilitate DoE planning.  The goal will be to specify the number and ranges of factors and number of possible experiments to run. Next, view the proposed design of experiments and upload the experimental results.  Finally, an empirical model is generated from the data and the results are plotted.  This app uses a default model to generate placeholder data, but real data can easily be entered into the application either directly from the web interface or by uploading a `csv` file.

Borrowed heavily from:

 -  https://github.com/rocker-org/shiny

 -  https://github.com/flaviobarros/shiny-wordcloud

 -  https://medium.com/@suomynonascitylana/getting-started-with-docker-to-do-data-science-and-deploy-a-shiny-app-46803f8a0a69

 -  https://www.r-bloggers.com/dockerizing-a-shiny-app/



Todos:
- Categorical variables!

- Bayesian feature selection!

- Model based experimental design!
 
How to work it: 

1. Build docker image with `docker build -t chepyle/shiny_diydoe .`

2. Deploy on digital ocean using docker app container and the command: `docker run -d -p 80:80 chepyle/shiny_diydoe &`
