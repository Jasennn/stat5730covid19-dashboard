
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stat5730covid19-dashboard

<!-- badges: start -->

<!-- badges: end -->

This is an example COVID-19 dashboard for STAT 5730 based on R Shiny. It
requires the following R packages:

  - shiny
  - maps
  - tidyverse
  - RcppRoll
  - [stat5730covid19](https://github.com/vqv/stat5730covid19) – Make
    sure you use `remotes::install_github()` to install this package.

You can install of these packages with the following commands:

``` r
install.packages(c("shiny", "maps", "tidyverse", "RcppRoll", "devtools"))
devtools::install_github("vqv/stat5730covid19")
```

There is just a single source file: [`app.R`](app.R). You can view a
live version of the app at
[vincev.shinyapps.io/stat5730covid19-dashboard/](https://vincev.shinyapps.io/stat5730covid19-dashboard/)

You can clone this repository to your desktop, or even better, you can
login to [rstudio.cloud](https://rstudio.cloud) with your
**shinyapps.io** login credentials—they are shared between the two
services—and create a new project directly from this Github repository.
In \[rstudio.cloud\], click on the down arrow next to “New Project” and
select “New Project from Git Repo”. Then copy-and-paste the URL of this
web page. This makes it much easier to share with your group.

Once you have the `app.R` open in RStudio (or rstudio.cloud) and all the
required packages installed, you can click on the “Run App” button to
run a local instance of the app.

# Overview of design

The app has the following components:

**Inputs**

  - `selected_state`: Select the state of interest
  - `map_per_capita`: Display values as per 1M capita
  - `log_scale`: Use a log scale for the line chart of daily new deaths
  - `line_per_capita`: Display values in the line chart as per 1M capita
  - `line_background`: Display 7-day average of daily new deaths for
    other states in the background of the line chart

**Outputs**

  - `map_plot`: Plot of state counties filled by death values
  - `line_plot`: Plot of lines showing increase in deaths each day
  - `orders_table`: Table of state executive orders for the selected
    state

Some of the inputs, `log_scale`, `map_per_capita`, and `line_per_capita`
do not affect that data frames that the plots are based on. So the
`server()` introduces two reactive expressions that connect the other
inputs to functions for retrieving data. This is done to eliminate
re-fetching the data when inputs that do not affect the data are change.
The functions for retrieving the data for the plots and table are:

  - `get_state_deaths_map_data()`
  - `get_state_death_increase_line_data()`
  - `table_state_orders()`

These functions get data from several sources including data directly
connected to COVID-19 (`stat5730covid19::nyt_county`,
`stat5730covid19::covidtracking`, `stat5730covid19::state_orders`) and
demographic and cartographic data (U.S. Census population estimates and
county maps). The data retrieved by these functions is consumed by the
functions which produce plots or in the case of the table rendered
directly by:

  - `plot_deaths_map()`
  - `plot_death_increase_line()`

These plotting functions take additional arguments that control how the
plot is displayed. These are supplied by the other inputs.
