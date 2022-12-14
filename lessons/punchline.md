

# The Punch Line First

For this first class we are starting at the end and working backwards.  As such this first lesson is going to be in a follow the leader mode.  This is a useful way to teach this as we get to see what is possible early on and then we can focus on the details of how we get there and you will have a working example of an R script that includes a full data analysis.  Let's get started:

## Create a new project in RStudio

We will use the RStudio "File:New Project" dialog to create a new project to house our work for this class.  In RStudio you need to find the New Project menu.

![rstudio_proj1](figures/rstudio_proj1.png)

Then select "New Diectory"

![new directory](figures/rstudio_project_new.jpg)


Then select "New Project"

![rstudio_proj1](figures/rstudio_project_new2.jpg)

The select "Create Project."  Name this project "acesd_intro_r_class". At this point you should now have a new, empty project.  We need to download the R Script and some data that we will be using for our workshop. Let's first create a folder for our R scripts and a folder for our data. Click on "New Folder" and create a folder called "R", then repeat that and create a folder called "data" 

![rstudio_new_folder](figures/rstudio_project_new_folder.jpg)


Now download the files into their appropriate folders.  Right click on each of the the links below and select "Save Link As".  In the window that opens up, browse to the location of the project and save the script in the "R" folder and the NLA data in the "data" folder. In the past, chrome has wanted to call the data file, a ".txt" file.  It shouldn't do that, but if it does, make sure that you change the ".txt" to ".csv" when you save the file. 

- [ACESD R Analysis](https://raw.githubusercontent.com/usepa/acesd_intro_r/master/lessons/acesd_analysis.R)
- [2017 National Lakes Assessment Water Chemistry Data](https://www.epa.gov/sites/default/files/2021-04/nla_2017_water_chemistry_chla-data.csv)


## Run the script 

You will need to make sure the script is open in RStudio.  You can either click on the file name in the RStudio file browser or go to File: Open File, browse to the R folder in your project and select the "acesd_analysis.R" file.

Once this is open, we can run the script by selecting all lines (e.g. `ctrl-a`) and then clicking on the `Run` button

![rstudio_knit](figures/rstudio_run.jpg)
