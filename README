# Getting started

Most of the works in this repository, especially the `R` scripts, should be directly reproducible. You'll need [`git`](https://git-scm.com/downloads), [`R`](https://www.r-project.org/), and more conveniently [RStudio IDE](https://posit.co/downloads/) installed and running well in your system. You simply need to fork/clone this repository using RStudio by following [this tutorial, start right away from `Step 2`](https://book.cds101.com/using-rstudio-server-to-clone-a-github-repo-as-a-new-project.html#step---2). In your RStudio command line, you can copy paste the following code to setup your working directory:

```
install.packages("renv") # Only need to run this step if `renv` is not installed
```

This step will install `renv` package, which will help you set up the `R` environment. Please note that `renv` helps tracking, versioning, and updating packages I used throughout the analysis.

```
renv::restore()
```

This step will read `renv.lock` file and install required packages to your local machine. When all packages loaded properly (make sure there's no error at all), you can proceed with:

```
install.packages("targets")
```

This step will install `targets`, which I use to track my analysis pipeline. The `targets` package will automate each analysis step, starting from cleaning the data up to rendering a report. Run the following command to complete the analysis:

```
targets::tar_make()
```

This step will read `_targets.R` file, where I systematically draft all of the analysis steps. Once it's done running, you will find the rendered document (either in `html` or `pdf`) inside the `render` directory.

# What's this all about?

Content to be updated.