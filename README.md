# Covid Plots
Outline of the project:

## Plot
Observations as points for daily deaths
Joined lines for cumulative deaths

### Later annotations
Add error bands at 0.5,1,2 se

The se comes from either

(a) Poisson fit to the log plot (for early data)
(b) Loess fit	

### Further annotations:
Key points such as estimated points where the log plot starts to fall off linear

Needs uncertainties around the 1st and 2nd derivative.

### The input options are (default in bold)
Cumulative or *Daily*

Cases or *Deaths*

Log (Poisson fit) or *Linear (Loess fit)*

*By date* or aligned on a configurable daily death threshold

Countries to have as options,
Initially plot UK with ability to add 1 more

*UK only*

+US

+Italy

+Spain

+France

+Sweden

+Switzerland

+Korea

+China

+Hong Kong

+Japan

+Iran??



Data source: 
* https://covid.ourworldindata.org/data/ecdc/total_deaths.csv 
* https://covid.ourworldindata.org/data/ecdc/total_cases.csv 


