### Motivation
Target audience: Socioeconomic researchers and policy makers.

The fourth demographic transition has reached many of the most modern and developed economies. However, for those countries that are still developing, the transition is not at a homogeneous stage across the country. In plain terms, the number of children couples are having, the number of elderly (as well as their mean ages) and internal migration vary from region to region as an indicator of access to health services, quality of life, level of development of the place, etc. This app allows the user to view the data set of [INEGI’s 2020 Mexican Population Census](https://www.inegi.org.mx/app/descarga/ficha.html?tit=326108&ag=0&f=csv) to identify risk factors to anticipate and develop more effective policies. This dashboard allows visualizing different characteristics of the Mexican population with respect to its location.

Deployment: https://marcony1.shinyapps.io/2020_Mexico_Census/

### App description
[![Dashboard Demo](http://img.youtube.com/vi/390M7MEkWjI/0.jpg)](http://www.youtube.com/watch?v=390M7MEkWjI "2020 Mexico Census")


### Installation instructions
Perform the following steps to run the App on your desktop:

1. Clone the `repo`.
2. Open `RStudio` and verify if you have `renv` package installed, if not, run `install.packages("renv")` in R's console.
3. Go to the `root` of the project directory (the local copy) and open/execute `"DSCI_532_individual-assignment_marcony1.Rproj"`.
4. Once in RStudio, execute `renv::restore()` to install the necessary libraries/packages.
5. Go to `src/` and open `app.R`.
6. Run the `app`.