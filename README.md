# Predict the arrival of CoronaVirus to Galapagos

This analysis is based on available data from several sources:
- WorldBank country population data
- PNG tourist visitation data (2017 report)
- Johns Hopkins University CoronaVirus data (package ‘coronavirus’)
- Worldometer.com CoronaVirus numbers for each country

In essence the analysis is based on a simulation in which every day a random group of tourists are selected from each country based on tourist visitation statistics for Galapagos. The proportion of actively infected individuals in that country affects the probability that one of those tourists is infected. Each day that passes the total number of infected individuals in each country increases, and so does the probability that one of those individuals will visit the Galapagos.

## Results and conclusions (last updated March 2, 2020):

Simulating different scenarios that vary from extreme growth of the virus to slower, more optimistic projections, still leads to the similar conclusion that the question is likely not _if_ COVID-19 reaches Galapagos, but _when_ it does. In addition, these estimates are likely conservative due to the much larger potential for undocumented or diagnosed cases as has been suggested. However, the time until the virus reaches Galapagos is largely dependent on the spread of the virus, and much is still unknown about this. I am not an epidemiologist, and though my models of the growth of the virus might not be fully accurate, the fact that several different growth functions led to similar conclusions is intriguing. I think that the simplicity of this analysis coupled with the results should encourage the Galapagos hospitals and community to prepare. If COVID-19 reaches the Galapagos, the effect would be devastating. I hope that some evidence demonstrating the likely potential for the virus reaching Galapagos is enough to help us take better action to prepare just in case. Assuming the best case scenario is not a risk we should take.

