nn=6;
s=Sum[(1+y)^Binomial[n, 2] x^n/n!, {n, 0, nn}]; (* Sum as n goes 0 to nn *)
Range[0, nn]!CoefficientList[Series[Log[ s]+1, {x, 0, nn}], {x, y}]//Grid
