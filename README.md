# FinestTwins - Modeling of the city of Tallinn
The data, models and material shared through this folder are licensed under a Creative Commons — Attribution 4.0 License (International) — CC BY 4.0: https://creativecommons.org/licenses/by/4.0/

The work has been carried out within the activities of the FinestTwins project (http://www.finesttwins.eu/)

# How to cite
Synthetic population data - Agriesti, S.; Roncoli, C.; Nahmias-Biran, B.-h. Assignment of a Synthetic Population for Activity-Based Modeling Employing Publicly Available Data. ISPRS Int. J. Geo-Inf. 2022, 11, 148. https://doi.org/10.3390/ijgi11020148

Bayesian Optimization algorithm - Agriesti, S.; Kuzmanovski, V.; Hollmén, J.; Roncoli, C.; & Nahmias-Biran, B. -h. A Bayesian Optimization approach for calibrating large-scale activity-based transport models. 2023. arXiv preprint arXiv:2302.03480.

SimMobility MT model - Agriesti,S.; Anashin, P.; Roncoli, C.; Nahmias-Biran, B.-h.Integrating activity-based and traffic assignment models: Methodology and case study application. In MT-ITS 8th International Conference on Models and Technologies for Intelligent Transportation Systems, June 2023, Nice, France.

Aimsun model - Agriesti,S.; Anashin, P.; Roncoli, C.; Nahmias-Biran, B.-h.Integrating activity-based and traffic assignment models: Methodology and case study application. In MT-ITS 8th International Conference on Models and Technologies for Intelligent Transportation Systems, June 2023, Nice, France.

# Dataset
The Tallinn dataset is provided as open source, it may be found in the folder "dataset" as .csv file. There, also some relevant distributions are reported as summary. The folders "ResidentialAssignment" and "WorkplaceAssignment" contain two ready-for-use examples to assign either the residence cell (500x500 m) or the workplace one to the synthetic population. The folder "SpatialMap" includes a .gpkg file as Qgis spatial map, some representative maps are also printed as .pdf.

The dataset is composed by 23 variables, briefly defined in the following:    

District* -->	District of residence

Household_id*	--> ID of the household

Household_size*	--> Size of the household

Age interval*	-->	Age intervals: - Discretized for privacy reasons

Gender*	-->	1 - Female; 2 - Male

Head_of_the_household*	-->	Boolean for the head of the household

ID*	-->	Individual_id

Subdistrict*	-->	Subdistrict of residence - numerical id (for the correspondance between ids and subdistricts, please check the dedicated description on github)

IFM**	-->	Average income per family member - 4 qualitative classes to preserve anonimity

Blicense**	-->	Boolean describing if the individual has (1) or does not have (0) a B driving license

CarOwnership***	-->	Number of cars owned within the household

Alicense**	-->	As in B license

MotoOwnership**	-->	Number of motorcycle owned within the household

C.Dlicense**	-->	As in B license

BikeOwnership**	-->	Number of bikes owned within the household

RCell***	-->	Numerical id for the cell of residence - the map of the cells is provided on a separate document on github

KDist***	-->	District of the kindergarden - for individuals with age < 6

SchDist***	-->	District of the educational institution (either basic, upper or university)

SchCell***	-->	Numerical id for the cell of the attended educational institution

Occupation***	-->	Employment role

EMTAK***	-->	EMTAK field of occupation (national equivalent of the NACE classification)

WorkDist***	-->	District of the workplace

WCell***	-->	Numerical id for the cell of the workplace

The asterisks near the variables represents how the variables were obtained. One asterisk indicates the variables produced through SimPop, they represent the household structures of the survey and their reliability is high, being calibrated against the city statistics. Two asterisks' variables are instead obtained through multinomial logistic regression, they reproduce faithfully the distributions present in the survey but could not be validated due to lack of public data. A notable exception is IFM, which was validated against the distribution obtained from the municipality. Three asterisks represent variables added on the basis of discrete probability distributions and, when relevant, land use information.


# Acknowledgments 
This research is funded by the FINEST Twins Center of Excellence (H2020 Europe-an Union funding for Research & Innovation grant 856602). 
The authors would like to thank Prof. Dago Antov from Taltech for sharing the travel survey exploited in this work. Moreover, the authors are grateful to the Tallinn Municipality and to all the related public bodies who supported this research by sharing data. Finally, the authors would like to thank all the partners and stakeholders involved in the FinestTwins Centre of Excellence. The authors also thank Aimsun SLU for the support provided in calibrating the Aimsun model.
