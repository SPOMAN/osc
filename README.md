# osc: Tools for the Organic Surface Chemistry group

This R package contains/will contain a set of useful tools for students and researchers at the [Organic Surface Chemistry](http://surfchem.dk) group at Aarhus University, Denmark. The package is under development, and probably will be for some time. Check the topics below to get an overview of what has been implemented so far. If you are missing features, then go ahead and create [an issue](https://github.com/SPOMAN/osc/issues).

## Electrochemistry

### Cyclic Voltammetry
* Cyclic Voltammograms from the CHI can be loaded with `df <- cv_read("file.txt", skip = int)`
* CVs can be plotted by `plot(df)`

## Raman Spectroscopy
* Curve-fitted Raman maps from Wire can be loaded with `df <- raman_curvefit_read("path/to/folder/")`
* A given feature from a curve-fitted map can be plotted with `plot(df, G-peak)`

## X-Ray Photoelectron Spectroscopy
Nothing so far
