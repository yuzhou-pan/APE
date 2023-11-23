# APE

- **APE_script1.R:** specify 7 consecutive days
- **APE_script2.R:** divide 365 days into 7-day windows

## Notes (2023-11-23)
1. Need to binary install INLA for linux-based HPC cluster
2. On HPC cluster script 1 "fails to get good initial values" so INLA crashes at the 10th iteration.
    - Current soln: add verbose = TRUE argument and re-run the script
