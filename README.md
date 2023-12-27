# Scripts
Perform 10-fold cross-validation on 7-day data

|  | Objective | Sampling Method | Buffer Radius | Data Source |
|--|-----------|:---------------:|:-------------:|-------------|
| **script1** | Perform 10-fold CV | (Location, Day) | 100km | Specify 7 consecutive days |
| **script2** | Perform 10-fold CV | (Location, Day) | 100km | Divide 365 days into 13*4 7-day windows | 
| **script3** | Create summary tables | - | - | Import outputs of script 2, 4, 5, 6 |
| **script4** | Perform 10-fold CV | (Location) | 100km | Divide 365 days into 13*4 7-day windows |
| **script5** | Perform 10-fold CV | (Location) | 0km | Divide 365 days into 13*4 7-day windows |
| **script6** | Perform 10-fold CV | (Location) | 200km | Divide 365 days into 13*4 7-day windows |

Specify 7 consecutive days, perform 10-fold cross-validation; folds are created by
- sampling (location, day)
     - **script1:** radius = 100km

Divide 365 days into 13*4 7-day windows, perform 10-fold cross-validation; folds are created by
- sampling (location, day)
     - **script2:** radius = 100km
- sampling (location)
     - **script4:** radius = 100km
     - **script5:** radius = 0km (no buffer)
     - **script6:** radius = 200km
       
**script3:** create summary statistics using outputs from script 2, 4, 5, 6

# Tasks
- Model selection
- Prediction on 12km*12km grids
  + modify A matrix in "pred" stacked data

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


