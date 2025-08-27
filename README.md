<img src="emude_logo.png" width="216" height="250">

## emude: Environmental Modeling with Universal Dynamic Equations

This R package is intended for users who want to apply state-space universal dynamic equations (UDEs) to environmental data. It serves as a front-end for [UniversalDiffEq.jl](https://github.com/Jack-H-Buckner/UniversalDiffEq.jl), which performs the machine learning algorithms in Julia on the back-end. It also leverages the enhanced plotting features in ggplot2 to create publication-ready figures.

### Acknowledgements
The development of this package is supported by the National Science Foundation awards #2233982 and #2233983 on Model Enabled Machine Learning for Predicting Ecosystem Regime Shifts.

### Installation of Package
**Install Instructructions as of 8/27/2025**

**RStudio:**

```
install.packages("devtools")
install.packages("JuliaCall")
install.packages("deSolve")
library(devtools)
install_github("zechmeunier/emude")
```

**Computer’s Command Line:**  
Windows:

```shell
winget install --name Julia --id 9NJNWW8PVKMN -e -s msstore
```

Mac/Linux:

```shell
curl -fsSL https://install.julialang.org | sh
```

On Mac, the julia and juliaup commands will not be available until the terminal is refreshed.   
Try running the following command or opening a new tab in the terminal app.

```shell
. /User/**your user name**/.zshrc
```

Both:

```shell
juliaup add release
julia
```

After running the “julia” command in your shell, your computer should open the Julia REPL.  
**Julia REPL:**

```
using Pkg
Pkg.add("UniversalDiffEq")
Sys.BINDIR
```

After running “Sys.BINDIR” Julia will return the path to where it is installed on your computer. COPY THIS STRING. Note that this string will have double backslashes between each folder. If you receive an error when trying to run julia\_setup(), you may need to replace these with a single forward slash each to make the filepath interpretable for R.  
**RStudio:**

```
library(JuliaCall)
julia_setup(JULIA_HOME = "[PASTE THE STRING YOU COPIED HERE]")
library(emude)
emude_setup()
```