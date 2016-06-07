# Welcome to EpiCOGs

This is a sandbox environment that allows users to visualize primarily public health data.
This project is currently being developed by @amcrisan (contact : acrisan@cs.ubc.ca), so come back often for updates.

If you want to demo a version of the tool you can do so here:
https://amcrisan.shinyapps.io/EpiCOGSDEMO/

This demo version does not allow you to load your own data, but otherwise contains all the functionality of EpiCOGs.

If you'd like to know more about my research, what motivated it, and where this project is going, you can check out the presentation that is linked below:

https://goo.gl/2Ek80Q

<<<<<<< HEAD
# Minimum Dataset requirements
At the very minimum, a dataset must have Latitude and Longituide Co-ordinates. By default, those variables are called "Lat" and "Long", but they can be changed via the uiControl. An example of an minimal dataset is available in the fakeData folder for reference.

The main reason that EpiCOGS doesn't do the co-ordinate conversaion for you is that there are policies for how to place to patients according to their geographic data when it's present or absent. A number of different conventions exist, and for the moment I've decided to leave it up to the user to add Lat and Long data rather than try guess and support all those different conventions. 

As an aside revgeocode is what I used for my datasets, it's available through ggmaps. It does have a limit of 2,500 queries per day. So if you have a larger dataset, it could take a bit of time to get all the co-ordinate data. I am thinking of developing a data prep companion to EPiCOGs specifically for geographic data and that can be customized according to different institutional convetions. 

# Have questions, comments, or concerns? 
**You've got two options:**

=======
**Have questions, comments, or concerns? You've got two options:**

>>>>>>> origin/master
1. File an issue in this repository!
2. Submit it as part of this google form: https://goo.gl/1Ee8dk
