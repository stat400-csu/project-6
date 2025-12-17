# Group 6 Project: Sequential Experimental Design for Predator-Prey Dynamics
STAT 400 @ CSU

This repository contains the code and analysis for evaluating the efficiency and stability of Sequential Monte Carlo (SMC) methods in the context of predator-prey functional response models.

# External Dependencies
This project builds upon the framework developed by Moffat (2020). Before running the local files, ensure you have downloaded the core utility scripts from the following source: [Moffat GitHub Repository
](https://github.com/haydenmoffat/sequential_design_for_predator_prey_experiments/tree/master/R/code)

# Move-step Experiment
To replicate the move-step experiment, navigate to "Jakes_File.Rmd" to run code. The first chunk loads in libraries, and the next 4 run each comparison of the move steps. At the top of each chunk, the true model and the amount of move steps is described in a comment. Make sure that all files ending in "Jake_Copy.R" are downloaded, since they will be pulled from in the code.  

# Particle Experiment
To replicate the particle experiments navigate to "stat 400 project.Rmd" to run code. The first chunk loads in libraries, the second reruns the original code as a loop, and the following chunks run the experiment with different N values which are labeled clearly. The final code chunks are to create the descriptive quantitative plots used in the presentation and paper. Ensure that all files ending in "Juliette_Copy.R" are downloaded, as they will be pulled from in the code.

# Presentation and Paper
To access the Presentation and Paper, access the files by going through "STAT 400 > project-6 > Paper and Presentation". The presentation is then accessed in the file "Presentation.html". For the paper, navigate to "STAT 400 > project-6 > Paper and Presentation". The paper is then accessed in the file "Paper-Draft.pdf". 
