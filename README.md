# Significant Digits library for Scala

Provides a way to do math with significant digits in scala.  To the basic 
numeric types like: Double, Long, Float and Int are added SDI, SDL, SDF,
and SDI which are like their Numeric cousins but carry an additional Int
recording the number of significant digits of accuracy the calculation
actually has.  Rounding and formatting of these new types are controlled via
and variable called SDContext.  Currently this variable is a
singleton set by the method setSDContext in the 
`org.sigfigs.SignificantDigits` object.  Which is why the test are not
run in parallel.  

It should be noted that changing this behavior is rare
and due to the exact nature of the Numeric API and how scala defines
subclassing it is not possible to pass this variable implicitly into operations 
of the Numeric API.  That's why its a singleton, when subclassing existing
APIs without a specific implicit variable, you can't add in that implicit
variable without a type conversion and you can't do that with a Numeric
since its used as an implicit variable.
