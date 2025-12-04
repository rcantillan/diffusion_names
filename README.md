# Project: Diffusion of names and social class

This repository contains code routines for the "diffusion of names and social classes in Chile" project. The project includes:

- Extract and relate historical data on the socioeconomic composition of the communes of Chile using census data and other alternative sources.

- Carry out descriptive analysis and modeling (based on Social Network Analysis) that account for historical stratification trends.

- **NEW:** Agent-Based Model (ABM) for simulating name diffusion between social strata (see below)

Repository under development...

## Agent-Based Model (ABM)

A complete Agent-Based Model has been implemented to simulate the diffusion of names between social strata from 1920 to 2010. The model incorporates theoretical microfoundations including:

- **Upward imitation** (aspirational exposure): Lower classes adopting names from upper classes
- **Downward saturation**: Upper classes abandoning names that become common in lower classes
- **5 social quintiles** (Q1-Q5) and **30 names** over **91 years** (1920-2010)

### Quick Start

```r
# Load the model
source("code/05_abm_name_diffusion.R")

# Run a complete example
results <- run_abm_example(seed = 42, verbose = TRUE)

# Access results
abm_results <- results$abm_results
metrics <- results$metrics

# Create visualizations
plot <- plot_name_evolution(abm_results, class_index = 5, top_n = 10)
```

### Files

- `code/05_abm_name_diffusion.R` - Main ABM implementation
- `code/05b_abm_example_usage.R` - Complete usage examples
- `code/05c_abm_validation.R` - Validation tests
- `code/ABM_README.md` - Detailed documentation

See `code/ABM_README.md` for complete documentation, mathematical formulation, and examples.


Random thought:

- We could analyse the transitivity of name difussion across social clases.
- A los Juanes, los oseses y las Marías les agregaría el segundo nombre y los trataría como nombre unitario. E.j., "Juan Pablo", "María Margarita".
- Los graficos con top names podrian ser reemplazados con top names cada década y flujos entre ellas, para comunas selectas


Papers:

https://www.pnas.org/doi/10.1073/pnas.1504811112



