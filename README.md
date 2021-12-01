# DasCombat fit operator

##### Description

The `dascombat_fit_operator` is a operator to obtain a fitted model based on the
COMBAT software implementation developed at PamGene. This fitted model can then be used to 
make predictions in the `dascombat_prediction_operator`.

##### Usage

Input projection|.
---|---
`y-axis`        | type, description 
`row`           | type, description 
`column`        | type, description 
`colors`        | type, description 

Output relations|.
---|---
`output_var`        | character, name of the COMBAT model (to be used with other dascombat_prediction_operator)

##### Details

Dascombat implements the ``empirical bayes methodology to adjust for batch effects`` that was developed by W.E. Johnson and co-workers

- [Johnson et al (2007)] https://pubmed.ncbi.nlm.nih.gov/16632515/
- [Zhang et al (2018)] https://bmcbioinformatics.biomedcentral.com/articles/10.1186/s12859-018-2263-6

The functionality to split in *fit* and *predict* has been added here, and allows for the Combat model to be calculated on a subset of samples (i.e. reference samples) and applied to another set of samples.

##### See Also

[template_shiny_operator](https://github.com/tercen/template_shiny_operator)
, [template_docker_operator](https://github.com/tercen/template_docker_operator)

