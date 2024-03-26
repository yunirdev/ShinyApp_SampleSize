# ShinyApp_SampleSize
This is the R Shiny app to help health care professions to estimating the sample size they will need to conduct chart reviews in order to achieve specified error boundary.
---------------------------------------------------------------------------------

Any algorithm will likely yield some level of errors. How frequent these errors are and where they occur is important for end-users to understand. Estimating the error rate can serve as a final validation of the algorithm before implementation or as a periodic review to confirm that the algorithm is still functioning adequately.  

Getting a data asset to TL3 requires validating the results of the SQL algorithm that generates the data asset through manual chart review a time-consuming process that may need to be repeated many times to sufficiently validate algorithm performance enterprise-wide.  Because this review can be a very time-consuming task, it is desirable to minimize the number of records to review to that necessary to provide sufficient confidence that the error rate is acceptable. What an acceptable error is will vary depending on the variable of interest and its application. For example, an acceptable error rate for one application, may not be acceptable for another. Thus, it is important to estimate and report the  of error rate for a data asset.  

Two questions were considered in this project for binary and numeric variable:

1. How large is your data set?

2. Do you expect the accuracy of your algorithm to differ across defined subpopulations? 
