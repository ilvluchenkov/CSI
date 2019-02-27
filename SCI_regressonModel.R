
imp = tibble( variable = names( coef(m) )
              , coef = coef(m) ) %>%
  mutate( variable = map_chr( variable, function(x) unlist( str_split(x, '_woe') )[[1]]  ) ) %>%
  left_join( iv ) %>%
  mutate( imp = abs(coef) * info_value ) %>%
  arrange( desc(imp) ) 


knitr::kable( imp, align = 'lccc', digits = 2 )

#Interpreting individual predictions

data_relevant = data_woe[, names( coef(m) )[-1] ]

data_mult_logit = as_tibble( data_relevant * coef(m)[-1] ) 


#--------------------------------Dataframe with individual score values


data_mult_score = data_mult_logit %>%
  mutate_all( function(x) - factor * x ) %>%
  mutate( intercept = coef(m)[1]
          , intercept = offset - factor * intercept )



score = apply( data_mult_score, 1, sum ) 

data_mult_score$score = score

data_mult_score$score_ctrl = res$score

set.seed(1)

vars = names(ORIG)
vars = vars[ vars != 'CSI']

formula = as.formula( paste( 'CSI ~', paste( vars , collapse = '+') ) )


lasso = oetteR::f_train_lasso( data = ORIG
                               , p = NULL
                               , formula = formula
                               , k = 50
                               , family = 'binomial'
)

plotly::ggplotly( lasso$plot_mse )

p = lasso$plot_coef + theme( legend.position = 'none')
plotly::ggplotly( p, tooltip = c('x','y','color'))




