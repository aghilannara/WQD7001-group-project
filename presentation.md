Economic Impact Assessment using Input-Output (IO) framework
=============================================================
width: 1440
height: 900
date: 22nd June 2020
<br>
<br>

### Group Members (Group I):
- Najlaa Ramli (17219402)
- Nurul Aswani (17218967)
- Aghilan Narayanan (17218898)
- Nurarlisa Sulong (17220304)


Project Introduction
========================================================
### Problem Statement
Due to recent circumstances, there are a lot of uncertainties in regard to the economic outlook. Although information on potential demand changes are available, there are no exploratory or visualization tools to showcase such impact in an easier and comprehensible manner.   


### Question / Objective
To gauge and illustrate the wider economic impacts (i.e. **direct**, **indirect**, and **induced** impacts) with any demand changes in the Malaysian economy using an established **Input-Output** framework

### Beneficiaries
The Shiny app serves as an exploratory and visualization tool for any user to explore the wider economic impacts with any demand changes in the Malaysian economy.

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
- (Video Presentation) https://drive.google.com/drive/folders/1fXIP3tKS4LpTVgrXMSAlqftW91vw6pZl?usp=sharing