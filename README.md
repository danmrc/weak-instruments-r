# weak-instruments-r
Development of weak instruments package for R

This is a package under development that will bring some tests for instrumental variables estimation (IV) with weak instruments. The tests are, so far:

* Stock and Yogo (2005)
* Anderson and Rubin (1949)
* Conditional Likelihood ratio (Moreira (2003)) 

The Stock and Yogo test is a test to detect weak instruments in a case where you have more than one endogenous variable and more than one instrument. It can compute for at most 3 endogenous variables and up to 30 instruments. There are two modes for this test: one to limit bias up do a certain proportion of the bias of OLS and one to keep the tests size right. The second mode is not implemented, following a comment from Stock himself in NBER summer institute talk, since we have hypotesis tests that works even when the instruments are weak. 

The Anderson and Rubin (AR) and Conditional Likelihood Ratio (CLR) tests are tests for hypotesis that do not rely on the instruments. AR works with more than one endogenous variable, while CLR does not. On the other han, with one endogenous variable, CLR is more powerful than AR. 

Only the CLR has no been implemented yet. 

There are no examples yet.

I will also implement the confidence intervals based on the AR and CLR tests.

## References 

The articles above and:

Stock lecture in NBER 2008 Summer: http://www.nber.org/WNE/slides7-14-08/Lecture3.pdf

Article from the Stata journal with the implementation of Anderson Rubin and CLR: https://economics.mit.edu/files/1606
