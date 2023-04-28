# Distribution
The .ang file can be opened with an Aimsun software and a valid license. It is distributed and licensed with Creative Commons - Attribution 4.0 International - CC BY 4.0
The model and the available results should be referenced as Agriesti,S.; Anashin, P.; Roncoli, C.; Nahmias-Biran, B.-h.Integrating activity-based and traffic assignment models: Methodology and case study application. In MT-ITS 8th International Conference on Models and Technologies for Intelligent Transportation Systems, June 2023, Nice, France.

# The model key features
- Morning Peak (AM): 7:00-10:00
- ~ 133000 private vehicles simulated
- 82 centroids (1 for each subdistrict)
- 102 bus lines – schedule based
- Static routing: calibrated volume-delay and turn penalty functions
- Dynamic routing: 70% Macroscopic routes and 30% Stochastic Route Choice

Both the static and dynamic assignments are calibrated for the morning peak to match a GEH < 5 for at least 75% of the 146 detectors used.
The detector data is not available since it is not freely shareable. In the same way, the detector location is not shared.

# What is calibrated
Macro: Capacities per link type, desired speed distribution, volume delay functions per link type, turn penalty functions and junction delay functions

Meso: Reaction type factor, C-Logit SRC routing, node connections