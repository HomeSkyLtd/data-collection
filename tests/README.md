#Tests

In this folder he have all the tests made to figure out how to clean the data in the ilumination problem.
Each folder contains some tests and represents a approach.

##raw
This folder contains the raw data. The columns are: light (the iluminance), presence and timestamp. 
The gathering of the sensor's values was made every five minutes.
There are three files only
- henrique.txt
    Contains data of 14 days. Includes an additional column: light_on, which represents the state of the lamp.
- ricardo.txt
    Contains data of 37 days.
- ricardo_lamp.txt
    It contains the same data as in ricardo.txt but adds the column light_on, which used the hour of the day and the
    iluminance in the room to be created (the light was on when it was between 18pm and 4am and the light was above 20).

##action
We added the column action to the raw data, which represents the action taken in the next measure. If it is -1,
the user turned the light off, if it is 1, the light was turned on, and finally, if it is 0 no action was taken.
We have the files: 
- henrique.txt and ricardo.txt 
    Contain the raw data plus the action column
- henrique_edge.txt and ricardo_edge.txt
    Contain only the points where there is an edge in the action column
- henrique_sample.txt, ricardo_sample.txt, henrique_sample_t1.txt, ricardo_sample_t1.txt, henrique_sample_t2.txt, ricardo_sample_t2.txt
    Contain the same number of points where there was action and where there was no action
- henrique_sample2.txt and ricardo_sample2.txt
    The same as the above files, but there are twice points when there is no action
- henrique_sample4.txt and ricardo_sample4.txt
    The same as the above files, but there are twice points when there is no action

##smooth
In these files, we smoothed the data, using the subsequent points. We used as input the files action/henrique.txt and action/ricardo.txt.
The smooth used the next 4 points.
We have the files:
- henrique.txt and ricardo.txt 
    The presence is smoothed here
- henrique_edge.txt and ricardo_edge.txt
    Contain only the points where there is an edge in the action column
- henrique_0.txt and ricardo_0.txt
    Contain only the points where there is not an edge in the action column
 henrique_sample.txt, ricardo_sample.txt, henrique_sample_t1.txt, ricardo_sample_t1.txt, henrique_sample_t2.txt, ricardo_sample_t2.txt
    Contain the same number of points where there was action and where there was no action