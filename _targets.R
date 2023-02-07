library(targets)
library(tarchetypes)
library(tidyverse)

# Set build options ------------------------------------------------------------
set.seed(2)
future::plan(future.callr::callr, workers = 5)

# GLOBAL VARIABLE
iris_species <- tibble(sp = c("setosa", "versicolor", "virginica"))


# TARGET BASED VARIABLE
data_input_targets <- tar_plan(
  tar_target(iris, iris |> mutate(Species = as.character(Species)))
  
  # tar_target(iris, rbind(iris |> mutate(Species = as.character(Species)), 
  #                        c(6.0, 3.0, 5.1, 1.8, "nathanii")))
)

static_branching <- tar_plan(
  tar_map(
    values = iris_species,
    names = sp, 
    
    tar_target(group_split, iris |> filter(Species == sp)),
    tar_target(apply, group_split |> mutate(Species = toupper(Species)))
  )
)


dynamic_branching <- tar_plan(
  
  # Iteration here generates a branch for each species group
  tar_target(group_split, 
             iris |> 
               group_by(Species) |> 
               tar_group(),
             iteration = "group"),
  
  tar_target(apply_combine, group_split |> mutate(Spieces = toupper(Species)),
             pattern = map(group_split),
             iteration = "vector")
  
  # Interactive questions time.
  # Q: What happens when we change iteration? Or leave it out? 
  # Q: How much gets re-done when we do this? All targets? None? Let's tar_read and find out.
  # Q: What happens with vector iteration with non-conformable results?
  # Q: What happens if I add, remove, or fix a row in sampling_schemes? How much gets redone? 
  # Q: Does it depend on where I do it?
)


# List targets -----------------------------------------------------------------

list(
  data_input_targets,
  static_branching,
  dynamic_branching
)
