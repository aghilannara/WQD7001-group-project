Economic Impact Assessment using Input-Output (IO) framework
=============================================================
width: 1440
height: 900
date: 22nd June 2020
<br>
<br>

### Group Members (Group I):
- Aghilan Narayanan (17218898)
- Nurul Aswani (17218967)
- Nurarlisa Sulong (17220304)
- Najlaa Ramli (17219402)


Project Introduction
========================================================
### Problem Statement
Due to circumstances, there are substantial economic impact with demand changes in the Malaysia economy but there are no exploratory or visualization to showcase such impact.   


### Question / Objective
To determine the wider economic impact (i.e. direct, indirect and induced impact) with any demand changes in the Malaysian economy using exploratory method and visualize the impact. 

### Beneficiaries
The dashboard are set up for the end-user to explore the wider impact on Malaysian economy with selected demand changes. 

### Dataset Used
There are 4 type of datasets acquired from **DOSM Malaysia** being used in the project which are further explained in our Shiny dashboard under "Documentation". The Documentation also provides the calculation done on the data. Refer to the last slide for the link. 



Data Science Process
========================================================
As the project progressed, there are **SIX** (6) processes done in order to set up this project, 

1. **Asking Question** -- As explained in previous slide.
2. **Requirement Gathering** -- Requires several features which is list of economy sectors, employment count, GDP, output multiplier and value added multiplier. 
3. **Data Acquisition** -- Several datasets are acquired from **DOSM Malaysia**. 
4. **Data Cleansing and Transformation** -- Data are cleansed, filtered, mutated and combined for data analysis purpose.   
5. **Data Analysis** -- Calculations are performed accordingly as per documented formula in the **Documentation** part in our R Shiny Dashboard. Link are provided at the end of the slides.  
6. **Presenting Data** -- Data are visualized in such a way that the user can immediately see what direct, indirect, induced as well as total impact based on the selected demand changes. 

Shiny Dash Overview
========================================================


|  | |
| ----- | ---- |
| <img src="presentation-figure/1. menus.png" height="100" width="200"/> <p>1. Several menus available in our dashboard </p>| <img src="presentation-figure/2. dashboard.png" height="100" width="400"/> <p>2. Several input to set before calculating impacts </p>|
| <img src="presentation-figure/3. calculated.png" height="200" width="400"/> <p>3. Several outputs in term of numbers, sunburst and treemap will appear </p> | <img src="presentation-figure/4. breakdowns.png" height="200" width="400""/> <p>4. The breakdown of the calculated values and charts on the left</p> |
| <img src="presentation-figure/5. documentation.png"height="100" width="400"/> <p>5. Documentation of datasets, formulas used and others are in <b>Documentation</b> tab.</p> | <img src="presentation-figure/6. members.png" height="100" width="400"/> <p>6. List of group members involved.</p> |

Summary
=======================================================
### Experiences Gained
1. **Data Science Processes** done throughout the project taught us how to ask question, acquire appropriate datasets, cleansed, analyzed and visualize it.  
2. Using **R programming** language to do the calculations present a steep learning curve, but practices made it more understandable. 
3. Shiny App allows us to learn more on setting up Dashboard and link between the files, getting other interaction using **observeEvent** and **observe**.

### References

- (Github) https://github.com/aghilannara/WQD7001-group-project/
- (R Shiny App) https://aghilan.shinyapps.io/awan-dash/
- (Video Presentation) http://bit.ly/sasasasasa