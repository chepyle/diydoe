FROM rocker/shiny:latest

# setup packages for shiny app
RUN  echo 'install.packages(c("rhandsontable","AlgDesign", "plotly","reshape2","glmnet"), \
repos="http://cran.us.r-project.org", \
dependencies=TRUE)' > /tmp/packages.R \
  && Rscript /tmp/packages.R

EXPOSE 3838
CMD ["/usr/bin/shiny-server.sh"]