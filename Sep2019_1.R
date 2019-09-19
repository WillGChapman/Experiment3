# stochastic simulation for curvature data

# simulate trajectories with decision model

#ranwalks function input variables no starting point variability in this one!
n_time_samples <- 500
noise_sd <- 1
n_trials <- 1000
drift_rate <- 1

walks <- ranwalks(n_trials=n_trials, n_time_samples = n_time_samples, drift_rate = drift_rate, noise_sd = noise_sd)

# make trajectories

trajectories(walks)

# calculate AUCs



# get a baseline mean condition from experimental data

# get an experimental condition from experimental data

# warp experimental condition with baseline mean

# calculate AUCs for experimental condition

# kernel density for simulation

# ensure KD integrates to 1

# interpolate data points on density curve

# -2*sum(log(density@warpedAUC)) will be likelihood of data|model1 (check this concept)

# -2*sum(log(density@warpedAUC)) will be likelihood of data|model2

# model comparison - which number is bigger?