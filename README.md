As part of a research internship under the supervision of A/Prof Scott Heron, this script shows my work with sensor data provided by NOAA to investigate low tide events in Guam. This link (https://repository.library.noaa.gov/view/noaa/27292) will direct you to the published report from the project.

The challenge we were facing in the analysis was missing values in the long-term dataset of air pressure. I automated the interpolation of the values with loops to find the missing values. To account for edge cases like leap years or end-of-month transitions I implemented conditional logic. Linear regression estimates are used to fill in the gaps. Lastly, debugging elements such as period print statements are used for iterative testing.
