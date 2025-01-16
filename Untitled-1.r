---
title: "Untitled"
author: "Sven v/d Bergh"
date: "2025-01-14"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
install.packages("zip")
```

```{r}
library(ggplot2)
library(zip)

# Create a directory for saving plots
dir.create("plots", showWarnings = FALSE)

# List of data frames
data_list <- list(data_run1, data_run2, data_run3, data_run4, data_run5, data_run6)
names(data_list) <- paste0("data_run", 1:6)

# Generate and save plots for each data frame
for (i in 1:6) {
  p <- ggplot(data_list[[i]], aes(x = `Time (s)`)) +
    geom_line(aes(y = `Angular velocity x (rad/s)`, color = "x")) +
    geom_line(aes(y = `Angular velocity y (rad/s)`, color = "y")) +
    geom_line(aes(y = `Angular velocity z (rad/s)`, color = "z")) +
    labs(title = paste("Angular Velocities -", names(data_list)[i]),
         x = "Time (s)", 
         y = "Angular Velocity (rad/s)") +
    scale_color_manual(name = "Axis", values = c("x" = "darkcyan", "y" = "darkslategray4", "z" = "darkseagreen")) +
    theme_minimal()
  
  # Save plot to the "plots" directory
  ggsave(filename = paste0("plots/plot_", names(data_list)[i], ".png"), plot = p, width = 7, height = 7)
}

# Compress all plots into a zip file
zip::zip(zipfile = "plots.zip", files = list.files("plots", full.names = TRUE))

# Provide a message for the user
cat("Plots have been saved and compressed into 'plots.zip'")
```

```{r}
# Initialize an empty data frame to store results
angular_velocity_stats <- data.frame(
  run = character(),
  `average angular velocity x` = numeric(),
  `average angular velocity y` = numeric(),
  `average angular velocity z` = numeric(),
  `sd angular velocity x` = numeric(),
  `sd angular velocity y` = numeric(),
  `sd angular velocity z` = numeric(),
  stringsAsFactors = FALSE
)

# List of data frames
data_list <- list(data_run1, data_run2, data_run3, data_run4, data_run5, data_run6)
names(data_list) <- paste0("data_run", 1:6)

# Loop through each data frame and calculate means and standard deviations
for (i in 1:6) {
  avg_x <- mean(data_list[[i]]$`Angular velocity x (rad/s)`)
  avg_y <- mean(data_list[[i]]$`Angular velocity y (rad/s)`)
  avg_z <- mean(data_list[[i]]$`Angular velocity z (rad/s)`)
  
  sd_x <- sd(data_list[[i]]$`Angular velocity x (rad/s)`)
  sd_y <- sd(data_list[[i]]$`Angular velocity y (rad/s)`)
  sd_z <- sd(data_list[[i]]$`Angular velocity z (rad/s)`)
  
   # Calculate precession time for x-axis
  precession_time_x <- 2 * pi / abs(avg_x)
  precession_time_error_x <- (2 * pi / abs(avg_x)^2) * sd_x
  
  # Combine results into the data frame
  angular_velocity_stats <- rbind(
    angular_velocity_stats, 
    data.frame(
      run = names(data_list)[i],
      `average angular velocity x` = avg_x,
      `sd angular velocity x` = sd_x,
      `average angular velocity y` = avg_y,
      `sd angular velocity y` = sd_y,
      `average angular velocity z` = avg_z,
      `sd angular velocity z` = sd_z,
      `precession_time_x` = precession_time_x,
      `precession_time_error_x` = precession_time_error_x
    )
  )
}

# Display the result
print(angular_velocity_stats)
View(angular_velocity_stats)
``` 

```{r}
# Add the new columns (avg_rotational_period_s and rp_error) to the existing data frame
angular_velocity_stats$avg_rotational_period_disc <- c(0.284, 0.492, 0.200, 0.392, 0.484, 0.150)
angular_velocity_stats$rp_error <- c(0.03, 0.03, 0.03, 0.03, 0.03, 0.03)

# Assuming angular_velocity_stats is already defined and contains avg_rotational_period_disc and rp_error
# Calculate angular frequencies
angular_velocity_stats$angular_frequency_disc <- 2 * pi / angular_velocity_stats$avg_rotational_period_disc

# Calculate the error in angular frequency using the formula: δω = 2πω(δT/T)
angular_velocity_stats$angular_frequency_error <- 2 * pi * angular_velocity_stats$angular_frequency_disc * angular_velocity_stats$rp_error / angular_velocity_stats$avg_rotational_period_disc

# Display the updated data frame
print(angular_velocity_stats)
View(angular_velocity_stats)
```

```{r}
#as the masses for mass_l1 and mass_l2 were initially incorrectly inputted, these had to be corrected through the following code.
#This is not required when using the data found in GitHub after 15/01/2025 16pm GMT +1

# Correct the mass and mass error values for mass_l1 and mass_l2 (rows 3 and 4)
lab_measurements[3, "mass (kg)"] <- lab_measurements[3, "mass (kg)"] / 10
lab_measurements[3, "m_error"] <- lab_measurements[3, "m_error"] / 10

lab_measurements[4, "mass (kg)"] <- lab_measurements[4, "mass (kg)"] / 10
lab_measurements[4, "m_error"] <- lab_measurements[4, "m_error"] / 10

# Verify the changes
lab_measurements[c(3, 4), c("mass (kg)", "m_error")]
```

```{r}
# Define gravitational acceleration
g <- 9.81  # m/s²

# Calculate gravitational torque using distance_axis (m)
lab_measurements$gravitational_torque <- lab_measurements$`mass (kg)` * lab_measurements$`distance_axis (m)` * g

# Calculate error in gravitational torque using error propagation
lab_measurements$torque_error <- lab_measurements$gravitational_torque * 
  sqrt((lab_measurements$m_error / lab_measurements$`mass (kg)`)^2 + 
       (lab_measurements$d_error / lab_measurements$`distance_axis (m)`)^2)

View(lab_measurements)
```

```{r}
# Compute moment of inertia for "disc"
lab_measurements$moment_of_inertia <- ifelse(
  lab_measurements$object == "disc",
  0.5 * lab_measurements$`mass (kg)` * lab_measurements$`radius (m)`^2,
  NA
)

# Compute error for moment of inertia using error propagation
lab_measurements$inertia_error <- ifelse(
  lab_measurements$object == "disc",
  0.5 * sqrt(
    (lab_measurements$r_error * 2 * lab_measurements$`radius (m)` * lab_measurements$`mass (kg)` )^2 +
    (lab_measurements$m_error * lab_measurements$`radius (m)`^2)^2
  ),
  NA
)

View(lab_measurements)
```

```{r}
# Compute precession frequency and period with errors for each run
results <- mapply(function(run, omega, omega_error) {
  # Select the appropriate mass and torque based on the run
  if (run %in% c("data_run1", "data_run2", "data_run6")) {
    gravitational_torque <- lab_measurements$gravitational_torque[4]  # mass_l2
    torque_error <- lab_measurements$torque_error[4]
  } else {
    gravitational_torque <- lab_measurements$gravitational_torque[3]  # mass_l1
    torque_error <- lab_measurements$torque_error[3]
  }
  
  # Moment of inertia and its error
  I <- lab_measurements$moment_of_inertia[5]
  inertia_error <- lab_measurements$inertia_error[5]
  
  # Compute precession frequency
  Omega <- gravitational_torque / (I * omega)
  
  # Compute error propagation for precession frequency
  delta_Omega <- Omega * sqrt((torque_error / gravitational_torque)^2 + 
                              (inertia_error / I)^2 + 
                              (omega_error / omega)^2)
  
  return(c(precession_frequency = Omega, delta_precession_frequency = delta_Omega))
}, angular_velocity_stats$run, angular_velocity_stats$angular_frequency_disc, angular_velocity_stats$angular_frequency_error)

# Convert results matrix into separate vectors and add to the data frame
angular_velocity_stats$precession_frequency <- results["precession_frequency",]
angular_velocity_stats$delta_precession_frequency <- results["delta_precession_frequency",]

# Compute precession period and its error
angular_velocity_stats$T_precession <- 2 * pi / angular_velocity_stats$precession_frequency
angular_velocity_stats$delta_T_precession <- (2 * pi / angular_velocity_stats$precession_frequency^2) * 
                                             angular_velocity_stats$delta_precession_frequency

# Display updated data frame
print(angular_velocity_stats)
View(angular_velocity_stats)
```

```{r}
#to check whether the values obtained above are accurate, the following string of code might be run as a sanity check
#In case the values obtained match, all incremental steps, such as the computation of the gravitational_toque on masses l1 and l2, or the precession frequency, have been excecuted correctly making the obtained values for T_p accurate.

# # Create a new data frame for the sanity check results
# sanity_check_results <- data.frame(run = names(data_list))
# 
# # Constants
# g <- 9.81  # Acceleration due to gravity (m/s²)
# pi_squared <- 4 * pi^2
# 
# # Fetching the correct values from rows instead of columns
# mass_l1_value <- lab_measurements$`mass (kg)`[3]
# mass_l2_value <- lab_measurements$`mass (kg)`[4]
# distance_l1 <- lab_measurements$`distance_axis (m)`[3]
# distance_l2 <- lab_measurements$`distance_axis (m)`[4]
# I_disc <- lab_measurements$moment_of_inertia[5]
# 
# # Calculate the precession periods using the corrected referencing
# sanity_check_results$precession_period_sanity <- c(
#   pi_squared * I_disc / (mass_l2_value * g * distance_l2 * angular_velocity_stats$avg_rotational_period_disc[1]),
#   pi_squared * I_disc / (mass_l2_value * g * distance_l2 * angular_velocity_stats$avg_rotational_period_disc[2]),
#   pi_squared * I_disc / (mass_l1_value * g * distance_l1 * angular_velocity_stats$avg_rotational_period_disc[3]),
#   pi_squared * I_disc / (mass_l1_value * g * distance_l1 * angular_velocity_stats$avg_rotational_period_disc[4]),
#   pi_squared * I_disc / (mass_l1_value * g * distance_l1 * angular_velocity_stats$avg_rotational_period_disc[5]),
#   pi_squared * I_disc / (mass_l2_value * g * distance_l2 * angular_velocity_stats$avg_rotational_period_disc[6])
# )
# 
# # Display the sanity check data frame
# sanity_check_results
```

```{r}
# Save the angular_velocity_stats data frame to a CSV file
write.csv(angular_velocity_stats, "angular_velocity_stats.csv", row.names = FALSE)

# Save the lab_measurements data frame to a CSV file
write.csv(lab_measurements, "lab_measurements.csv", row.names = FALSE)

```
