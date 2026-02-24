
# Create the contingency matrix
arrest_data <- matrix(c(1261, 5324, 58410, 2426), 
                      nrow = 2, 
                      byrow = TRUE,
                      dimnames = list(
                        CrimeType = c("NARCOTICS", "THEFT"),
                        Arrest = c("False", "True")
                      ))

# Perform Chi-square test
chisq.test(arrest_data)
