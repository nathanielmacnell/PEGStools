######## exwas_workflow.R #######
# example of a potential exwas workflow
###

# A tidy workflow for fitting multiple models in an ExWAS approach is to:
# 1. create a models dataset containing all of the model fit information
# 2. define and map a model fitting function to this model dataset
# 3. reduce the results to a useful format
# 4. prepare reporting outputs

##### 0. Setup #####
# First, let's create an test dataset with known properties:
# 100 random Xi variables
# 100 semi-random Yi variables, influenced by the corresponding Xi 
dat <- data.frame(replicate(50,runif(10000))) |>
  dplyr::mutate(
    across(
      .cols=everything(),
      # make Y depend partially on X
      .fns= list(y=function(i) rbinom(size=1, n=10000, prob=0.2+0.3*i)),
    .names='{.fn}{.col}')
  ) |>
  # fix the y names so they don't have X
  dplyr::rename_with( ~ifelse(stringr::str_detect(.x,'y'), 
                      stringr::str_remove(.x,'X'),
                      tolower(.x)))

# Check by plotting correlations across X variables
# we should see the main diagonal stand out
dat |>
  cor() |>
  corrplot::corrplot()

##### 1. Define models #####

# expand_grid will create all combinations of the variables we specify
models <- tidyr::expand_grid(
  # select x variables
  x = dat |> 
    dplyr::select(dplyr::contains('x')) |>
    names(),
  # select y variables
  y = dat |>
    dplyr::select(dplyr::contains('y')) |>
    names(),
) |>
  dplyr::mutate(
    formula = paste0(y,'~',x)
  )

head(models)

##### 2. Define and apply fit function #####

# in this example, we'll use a logistic regression, but
# any function could be applied instead. You may need to add
# additional parameters to the models dataset for your function;
# using pmap() will enable you to set the variable names in the
# model dataset to the argument names for your function.

### 2.1 Define fit function

fit_function <- function(formula, ...) {
  
  # make sure the arguments of this function match what's available
  # in your model specification dataset. Use ... to capture the
  # remaining elements in your model dataset, in case you don't
  # need every element inside the function.
  
  glm(formula, data=dat, family=binomial('logit'))

}

### 2.2 Map fit function

# Notice that the results are in a list.
model_results_list <- purrr::pmap(models, fit_function)


##### 3. reduce model outputs to a useful format

# Let's process results into a tidy dataset
# we can keep the raw results around in a list, it is easy to process
# by using purrr::map() and lapply. For this exercise, we want to
# create some simple summaries, so we will build a dataset we can
# use for displaying some basic results

# First, work within the list to transform each model into a data frame
model_results <- lapply(X=model_results_list, broom::tidy) |>
  # then condense these data frames into one large set
  purrr::map_df(dplyr::bind_rows) |>
  # and remove the intercepts
  dplyr::filter(term != '(Intercept)')

# Finally, we can append to the model set to see the annotated results
results <- dplyr::bind_cols(models, model_results) |>
  # let's also apply a p-value correction because we ran many tests
  dplyr::mutate(p.adjusted = p.adjust(p.value, method='fdr'))

##### 4. Report results #####

# Let's create a heatmap comparing the results
library(ggplot2)

ggplot(results |>
         # restrict to significant results
         dplyr::filter(p.adjusted < 0.05) |>
         # because we are doing logistic regression the odds
         # ratio will be the coefficient exponentiated
         dplyr::mutate(odds_ratio = exp(estimate))) +
  geom_tile(aes(x=x, y=y, color=odds_ratio)) +
  labs(
    title='Simulated ExWAS Analysis',
    subtitle='Using logistic regression; FDR=0.05'
  )

ggsave('heatmap.pdf')

# Notice in this plot we have a few significant findings outside of
# the ones we designed. This is because we set the false discovery
# rate to 0.05, so we should expect to see a few false positives

# Create a table of the results
results_table <- results |>
  dplyr::filter(p.adjusted < 0.05) |>
  dplyr::mutate(
    `Exposure`=x,
    `Outcome`=y,
    `Odds Ratio`=exp(estimate),
    `p-value`=p.adjusted
  ) |>
  dplyr::select(
    `Outcome`,
    `Exposure`,
    `Odds Ratio`,
    `p-value`
  ) |>
  gt::gt()

# inspect result in RStudio
print(results_table)

# save result to a word document
gt::gtsave(results_table,'results.docx')

# In the table, we can see that the odds ratio for the false discoveries
# is close to 1. The other other odds ratio are around our simulated value:
exp(0.3)
  
  








