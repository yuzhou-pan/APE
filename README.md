# Workflow

# Scripts

## Data Preperation

## Analysis

Output: 
1. Predictions (mean, sd, 95% lower/upper level)
2. Summaries of fixed paramters
3. Summaries of hyperparameters

|  | Objective | Sampling Method | Buffer Radius | Data Source |
|--|-----------|:---------------:|:-------------:|-------------|
| **script1** | Perform 10-fold CV | (Location, Day) | 100km | Specify 7 consecutive days |
| **script2** | Perform 10-fold CV | (Location, Day) | 100km | Divide 365 days into 13*4 7-day windows | 
| **script4** | Perform 10-fold CV | (Location) | 100km | Divide 365 days into 13*4 7-day windows |
| **script5** | Perform 10-fold CV | (Location) | 0km | Divide 365 days into 13*4 7-day windows |
| **script6** | Perform 10-fold CV | (Location) | 200km | Divide 365 days into 13*4 7-day windows |

## Generating Tables or Plots

Output:
1. RMSE by quarters
2. Coverage by quarters
3. Plots

|  | Objective | Data Source |
|--|-----------|-------------|
| **script3** | Create summary tables | Import outputs of script 2, 4, 5, 6 |
| **2.1** | Run our final model 2.1 predicting to all grid locations | |
| **2.1.plot** |  Create plots for our final model 2.1 | |


# Notes
1. Need to binary install INLA for HPC cluster
2. Library caret used to create folds does not work in cluster R sessions
   - soln: manually create folds using for loops
3. Coding error in defining buffers gets fixed
4. INLA crashes at some iterations (10th for script 1, many more for script 2) with errors reported as "fails to get good initial values"
   - soln: using pcmatern for SPDE, adding initial values for hyperparameters using script 1 results
6. Numerical issue: In script 2 inla.mesh.2d() takes forever to run in some job
   - problem caused by specifying boundary layer using max.edge = c(300, 600)
   - ~~soln: round loc argument to 5 digits~~
   - soln: tune max.edge, offset arguments in inla.mesh.2d(); define and specify boundary using all monitor locations


