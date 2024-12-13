# Graham_etal_Novel_Behavioral_Assays
Code and data used in the manuscript "Novel Behavioral Assays Reveal Sex-Specific Behavioral Syndromes in Anemonefish"

Note, in the code or linear regressions, we did not include the ANOVA and eta_squared calculations, those were done like so:

car::Anova(model_name, type ='III')

effectsize::eta_squared(model_name)
