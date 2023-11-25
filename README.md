# APE

- **APE_script1.R:** specify 7 consecutive days
- **APE_script2.R:** divide 365 days into 7-day windows

## Notes
1. Need to binary install INLA for HPC cluster
2. Sbatch script 1 "fails to get good initial values" so INLA crashes at the 10th iteration.
    - Current soln: add verbose = TRUE argument and re-run the script
3. Mesh set-up:
   ```
   mesh.train <- inla.mesh.2d(loc = round(points.train, digits = 5),
                              cutoff = 12,
                              max.edge = c(300, 600))
   ```
