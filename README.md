# Scripts
- **APE_script1.R:** specify 7 consecutive days
- **APE_script2.R:** divide 365 days into 7-day windows

# Notes
1. Need to binary install INLA for HPC cluster
2. Library caret used to create folds does not work in cluster R sessions
   - soln: manually create folds using for loops
3. Coding error in defining buffers gets fixed
4. **INLA crashes at some iterations (10th for script 1, many more for script 2) with errors reported as "fails to get good initial values"**
   - soln: using pcmatern for SPDE, adding initial values for hyperparameters using script 1 results
5. Numerical issue: In script 2 inla.mesh.2d() takes forever to run in some job
   - soln: round loc argument to 5 digits
