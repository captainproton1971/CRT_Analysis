# Dugdale's Cathode Ray Tube Analysis
A shiny app for scaffolding analysis of CRT deflection vs. voltage ratio data.

Click [here](https://captainproton.shinyapps.io/CRT_analysis/) to use the online working example.

## Purpose
This app supports students in analyzing the deflection \(y\) vs. ratio \(\frac{V_\text{def}}{V_\text{acc}}\) of deflection potential \(V_\text{def}\) to accelerating potential \(V_\text{acc}\) for electrons in a cathode ray tube.  Theory predicts this relationship to be _proportional_ (i.e., linear with zero intercept), having slope \[y = \left(\frac{L^2}{4d}+\frac{LD}{2d}\right)\] where \(L\) is the length of the deflecting plates, \(d\) is the spacing between deflecting plates, and \(D\) is the distance from the deflecting plates to the screen.  Students are expected to derive this relationship.

## Scaffolding
The learning objectives I've assigned to this lab are:

  * interpreting results of statistical tests as rejecting, or failing to reject, a null hypothesis.
  * interpreting a graphical visualization of best-fit function vs. theoretical prediction involving uncertainties 
  
That is, the pedagogical purpose of this lab involves deciding whether or not the evidence supports the theory.  It is not about conducting the statistical tests, propagating the uncertainties, or generating the graph.

To focus students' analysis on interpreting the results, this app:

  * computes the predicted slope (students derive this as part of their introduction) and propagates its uncertainties,
  * performs regression of various flavours, reporting the form of the best-fit function, and
  * produces a scatterplot of the data, with a best fit line with uncertainty ribbon, and another ribbon indicating the range of the theoretical prediction.
  
## Details
Some details of the logic behind the analysis.

### Predicted slope
In the left-hand side-panel, students input their estimates for \(L\), \(D\) and \(d\) as well as their estimates of the respective measurement uncertainties.

From these, the app calculates the predicted slope and its propagated uncertainty.  It reports these at the bottom of the side-panel, retaining an appropriate number of significant figures.

### Regression
When students upload their data (top of the main panel), the best-fit function is investigated automatically.

The predicted relationship is of the form \(y=Bx\) where \(B\) is the predicted slope and \(x=\frac{V_\text{def}}{V_\text{acc}}\).

We wish to test that the form of the function supports the prediction.  To that end, we investigate the model \(y=Ax^2 + Bx + C\), and test the hypotheses that \(A=0\) and \(B=0\) using regression.  Small (\(p<0.5\)) p-values for the regression coefficients \(A\) and \(B\) indicate that the corresponding hypotheses are unlikely.  Otherwise, they are not statistically significant and can be ignored.

First, the full quadratic form is tested.

#### Case: significant quadratic term

If both \(A\) and \(C\) are significant, the regression stops and outputs the coefficients, standard errors (uncertainties), and p-values.

If \(A\) is significant, while \(C\) is not, the regression is recomputed, this time of the form \(y = Ax^2 + Bx\) in order to generate more consistent estimates of \(A\) and \(B\).

#### Case: no significant quadratic term

If \(A\) is found not to be significant, a linear regression \(y = Bx + C\) is performed.  \(B\) should always be significant (one of the reasons I have students submit their data for examination).  The test here is whether \(C\) is significant (this might have changed from the quadratic regression above).  Thus, \(B\)'s value and standard error are always computed.

If \(C\) is significant, its value, standard error, and p-value are reported.  Otherwise, just its p-value is reported.

#### Reporting of regression results
The cateogory (e.g., "Linear with significant intercept", "Quadratic with no significant intercept") is reported, along with coefficients and p-vales described above.  The adjusted \(R^2\) is also reported.

### Graph
Once student data are loaded, a scatterplot is generated along with an appropriate line of best fit and (if the side-panel has been filled in), a green translucent ribbon illustrating the predictions from theory.




