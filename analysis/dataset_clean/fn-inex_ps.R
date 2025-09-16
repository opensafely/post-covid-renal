# Function to apply project-specific inclusion criteria

inex_ps <- function(
    input,
    flow,
    cohort,
    inex_criterion
)
  {
  ## Apply inclusion criteria to all cohorts --------------------------------------
  print('Apply project-specific inclusion criteria to all cohorts')
  
  input <- subset(input, inex_ever_esrd == FALSE) # Subset input if never had ESRD
  flow[nrow(flow) + 1, ] <- c("Exclusion criteria: Has ESRD prior to index date", nrow(input))
  print(flow[nrow(flow), ])
  
  return(list(input = input, flow = flow))
}
 