# DasCombat fit operator

##### Description

The `dascombat_fit_operator` is a operator to obtain a fitted model based on the
DasCOMBAT software developed at PamGene. This fitted model can then be used to 
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
`output_var`        | character, name of the DASCOMBAT model (to be used with other dascombat_prediction_operator)

##### Details

Details on the computation.

##### See Also

[template_shiny_operator](https://github.com/tercen/template_shiny_operator)
, [template_docker_operator](https://github.com/tercen/template_docker_operator)

