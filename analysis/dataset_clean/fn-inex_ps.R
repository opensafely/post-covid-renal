# Function to apply project-specific inclusion criteria

inex_ps <- function(
    input,
    flow,
    cohort,
    inex_criterion
) {
  ## Apply inclusion criteria to all cohorts --------------------------------------
  print('Apply project-specific inclusion criteria to all cohorts')
  
  input_flow <- subset(input, inex_ever_esrd == TRUE) # Subset input if ever had ESRD 
  input <- subset(input, inex_ever_esrd == FALSE) # Subset input if never had ESRD
  flow[nrow(flow) + 1, ] <- c("Exclusion criteria: Has ESRD prior to index date", nrow(input_flow)
  print(flow[nrow(flow), ])
  
  return(list(input = input, flow = flow))
}
