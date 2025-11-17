#' @title Estimate hypothesis testing for all pairwise comparisons of categorical predictor levels
#'
#' @description \code{contrasts} estimates hypothesis testing for all pairwise comparisons of the levels of a categorical in a brmsfit object.
#' @usage contrasts(fit = NULL, predictor, level.sep = " - ", xlab = "Effect size",
#' gsub.pattern = NULL, gsub.replacement = NULL, n.posterior = 2000,
#' fill = "#6DCD59FF", sort.levels = NULL, html.table  = FALSE,
#' read.file = NULL, plot = FALSE, plot.area.prop = 1, highlight = FALSE,
#' non.zero = FALSE)
#' @param fit A brmsfit object.
#' @param predictor The name of the categorical predictor in the model fit for which contrasts will be computed. Note that the predictor must have at least 3 levels.
#' @param level.sep A character string to separate the levels in the output.
#' @param xlab A character string with the horizontal axis label. Default is "Effect size".
#' @param gsub.pattern A vector with character strings to be replaced
#' @param gsub.replacement A vector with character strings to use for replacement.
#' @param n.posterior Number of posterior samples to use for plotting. Default is 2000.
#' @param fill Color for posterior distribution fill. Default is "#6DCD59FF".
#' @param sort.levels Character vector with the order to be used for levels in the predictor.
#' @param html.table Logical to control whether estimate tables are plotted in html format. Useful for creating dynamic reports (i.e. Rmd or quarto html reports). Is FALSE (default) the table is return as a data frame object. You might have to add 'results = 'as.is' to chunk options in dynamic reports.
#' @param read.file Character string with the name of the .rds file containing the model fit.
#' @param plot Logical to control if posterior distributions of estimates are plotted. Default is FALSE.
#' @param plot.area.prop Positive number to control de proportion of the plotting area of posterior distributions that will be included. Default is 1 (the default area included by \code{\link[ggplot2]{ggplot}}). Useful for adding or removing empty space around distributions.
#' @param highlight Logical to control if posterior estimates for which the 95\% credible intervals do not overlap with zero are highlighted. Default is FALSE.
#' @param non.zero Logical to determine if the predictor levels are compared against zero instead. Default is FALSE.
#' @return If \code{plot = TRUE} the function returns a ggplot object with the posterior distributions of the comparisons between predictor levels. If \code{html = FALSE} the function will return a data frame with estimates for each comparison, otherwise it will print the estimates in a table in html format.
#'
#' @export
#' @name contrasts
#' @details Estimates hypothesis testing for all pairwise comparisons of levels from a categorical predictor. The function \code{\link[brms]{hypothesis}} is used internally. Alternatively, if argument \code{non.zero = TRUE} the function evaluates whether each level of the predictor is different from zero.
#'
#' Note that comparisons (i.e. contrasts) of categorical predictor levels when additional predictors are also included in the model are computed at the baseline (categorical predictors) or 0 (continuous predictors) value of the additional predictors. Mean-centering on additional continuous predictors can be used to ensure that the mean value of continuous predictors is used as baseline instead (Schielzeth 2010). Avoid using '+' in the predictor level names.
#' @examples
#' {
#' # run model
#' mod <- brm(Petal.Length ~ Species, iris, chains = 1, iter = 500)
#'
#' # compute constrasts with plot
#' contrasts(fit = mod, predictor = "Species", html.table = TRUE, plot = TRUE)
#' # compute constrasts without plot
#' contrasts(fit = mod, predictor = "Species", html.table = TRUE, plot = FALSE)
#' }
#' @seealso \code{\link{extended_summary}}, \code{\link{read_summary}}
#' @author Marcelo Araya-Salas \email{marcelo.araya@@ucr.ac.cr})
#'
#' @references {
#' Araya-Salas (2022), brmsish: miscellaneous functions to customize brms bayesian regression models. R package version 1.0.0.
#'
#' Paul-Christian Buerkner (2017). brms: An R Package for Bayesian Multilevel Models Using Stan. Journal of Statistical Software, 80(1), 1-28. doi:10.18637/jss.v080.i01
#'
#' Schielzeth, H. (2010), Simple means to improve the interpretability of regression coefficients. Methods in Ecology and Evolution, 1: 103-113. https://doi.org/10.1111/j.2041-210X.2010.00012.x
#' }
brm_contrasts <- function(
    fit = NULL,
    predictor = "vviq_group_4",
    level.sep = " - ",
    xlab = "Effect size (TAS score difference)",
    gsub.pattern = NULL,
    gsub.replacement = NULL,
    n.posterior = 2000,
    fill = "#6DCD59FF",
    html.table  = FALSE,
    read.file = NULL,
    plot = TRUE,
    plot.area.prop = 1,
    highlight = FALSE,
    non.zero = FALSE,
    nudge_mult = 2.2,
    exp_mult = 0.4
) {
    variables <- posterior::variables(fit)
    incl_classes <- c(
      "b", "bs", "bcs", "bsp", "bmo", "bme", "bmi", "bm",
      brms:::valid_dpars(fit), "delta", "lncor", "rescor", "ar", "ma", "sderr",
      "cosy", "cortime", "lagsar", "errorsar", "car", "sdcar", "rhocar",
      "sd", "cor", "df", "sds", "sdgp", "lscale", "simo"
    )
    incl_regex <- paste0("^", brms:::regex_or(incl_classes), "(_|$|\\[)")

    variables <- variables[grepl(incl_regex, variables)]

    fit_levels <- grep(
      paste0("^Intercept$|", paste(predictor, collapse = "|")), 
      gsub("^b_", "", variables), value = TRUE)

    # fix  baseline level
    original_levels <-
      unlist(
        sapply(
          predictor, 
          function(w) gsub(" |&", "", paste0(w, unique(fit$data[, w])))))

    base_levels <- setdiff(original_levels, fit_levels)
    base_level <- paste(base_levels, collapse = ":")

    # get levels
    pred_levels <- 
      unlist(sapply(predictor, function(w) as.character(unique(fit$data[, w]))))

    # add predictor name
    pred_levels_list <- 
      lapply(predictor, function(w) as.character(unique(fit$data[, w])))

    names(pred_levels_list) <- predictor

    pred_levels <- lapply(
      seq_len(length(pred_levels_list)), 
      function(x) paste0(names(pred_levels_list)[x], pred_levels_list[[x]]))

    # if (length(pred_levels) > 1)
    pred_levels <- apply(expand.grid(pred_levels), 1, paste, collapse = ":")

    # create data frame with level pairs
    levels_df <- as.data.frame(t(utils::combn(pred_levels, 2)))

    # remove spaces and &
    levels_df$V1.nospace <- gsub(" |&", "", levels_df$V1)
    levels_df$V2.nospace <- gsub(" |&", "", levels_df$V2)

    # create contrasts in brms syntax
    contrsts <- paste(
      apply(
        levels_df[, c("V1.nospace", "V2.nospace")], 1, paste, collapse = " - "), 
      "= 0")
    
    # convert magnitude for those compare against baseline
    levels_df$sign <- 1
    
    if (!non.zero)
      levels_df$sign[grep(base_level, levels_df$V1)] <- -1
    
    names(contrsts) <- paste0(levels_df$V1, level.sep, levels_df$V2)

    contrsts <- gsub(paste0(base_level, " - "), "", contrsts)
    contrsts <- gsub(paste0(" - ", base_level), "", contrsts)

    names(contrsts) <- 
      gsub(paste(predictor, collapse = "|"), "", names(contrsts))

    levels_df$hypothesis <- names(contrsts)

    if (!is.null(gsub.pattern) & !is.null(gsub.replacement)) {
      if (length(gsub.pattern) != length(gsub.replacement))
        stop2("'gsub.replacement' and 'gsub.pattern' must have the same length")

      for (i in 1:length(gsub.pattern))
        names(contrsts) <-
          gsub(gsub.pattern[i], gsub.replacement[i], names(contrsts))
    }

    # evaluate hypothesis
    # hyps <- brms::hypothesis(fit, contrsts[c(1, 2, 4)])$hypothesis
    hyps <- lapply(seq_len(length(contrsts)), function(x) {
      
      hyp <- try(brms::hypothesis(fit, contrsts[x]), silent = TRUE)

      if (is(hyp, "try-error")){
        hyp_tab <- 
          data.frame(
            Hypothesis = contrsts[x], 
            Estimate = NA, 
            Est.Error = NA, 
            CI.Lower = NA, 
            CI.Upper = NA, 
            Evid.Ratio = NA, 
            Post.Prob = NA, 
            Star = NA
          )
        draws <- NULL
        } else {
          hyp_tab <- hyp$hypothesis
          draws <- hyp$samples
          colnames(draws) <- names(contrsts)[x]
        }

      return(list(hyp_tab = hyp_tab, draws = draws))
    })

    hyp_table <- do.call(rbind, lapply(hyps, "[[", 1))
    draws <- do.call(cbind, lapply(hyps, "[[", 2))

    hyp_table <-
      hyp_table[
        ,
        c(
          "Hypothesis",
          "Estimate",
          "Est.Error",
          "CI.Lower",
          "CI.Upper",
          "Evid.Ratio"
          # "Star"
        )]

    hyp_table$Estimate <- hyp_table$Estimate * levels_df$sign
    hyp_table$`l-95% CI` <- hyp_table$CI.Lower * levels_df$sign
    hyp_table$`u-95% CI` <- hyp_table$CI.Upper * levels_df$sign
    hyp_table$CI.Lower <- hyp_table$CI.Upper <- NULL
    
    hyp_table$Estimate <- round(hyp_table$Estimate, digits = 3)
    hyp_table$Est.Error <- round(hyp_table$Est.Error, digits = 3)
    hyp_table$CI_low <- round(hyp_table$`l-95% CI`, digits = 3)
    hyp_table$CI_high <- round(hyp_table$`u-95% CI`, digits = 3)
    hyp_table$BF_10 <- 
      round(1 / hyp_table$Evid.Ratio, digits = 3)
    # hyp_table$`log(BF_10)` <- 
      # round(log(1 / hyp_table$Evid.Ratio), digits = 4)
    hyp_table$`l-95% CI` <- hyp_table$`u-95% CI` <- 
      hyp_table$`Evid.Ratio` <- NULL
    
    if (plot) {
      # subsample posteriors
      xdrws <- brms::as_draws(draws)
  
      # only apply thinning if length of posterior < n.posterior
      xdrws <-
        posterior::subset_draws(x = xdrws, variable = names(contrsts))
  
      if (round(length(xdrws[[1]][[1]]) / n.posterior, 0) >= 2)
        xdrws <-
        posterior::thin_draws(
          xdrws, 
          round(length(xdrws[[1]][[1]]) / n.posterior, 0))
  
      merged_xdrws <- posterior::merge_chains(xdrws)
      sub_posts <- as.data.frame(merged_xdrws)[, names(contrsts)]
      names(sub_posts) <- names(contrsts)
  
      out <-
        lapply(names(contrsts), function(y)
          data.frame(
            variable = y,
            value = 
              sort(sub_posts[, colnames(sub_posts) == y], decreasing = FALSE)
          ))
  
      posteriors <- do.call(rbind, out)
      posteriors$Hypothesis <-
        factor(posteriors$variable, levels = sort(unique(posteriors$variable)))
      
      hyp_table$value <- hyp_table$Estimate
      hyp_table$significance <-
        ifelse(hyp_table$CI_low * hyp_table$CI_high > 0, "sig", "non-sig")
      hyp_table$significance <-
        factor(hyp_table$significance, levels = c("non-sig", "sig"))
      
      posteriors$significance <-
        sapply(posteriors$Hypothesis, function(x)
          hyp_table$significance[hyp_table$Hypothesis == x])
  
      posteriors$sign <- if (!non.zero)
        sapply(posteriors$Hypothesis, function(x)
          levels_df$sign[levels_df$hypothesis == x]) else 1
  
      posteriors$value <- posteriors$value * posteriors$sign
  
      posteriors$Hypothesis <- 
        factor(
          posteriors$Hypothesis, 
          levels = hyp_table$Hypothesis[nrow(hyp_table):1])
      
      gg_distribution <-
        ggplot2::ggplot(
          data = posteriors,
          ggplot2::aes(
            y = Hypothesis, 
            x = value
          )
        ) +
        ggplot2::geom_vline(
          xintercept = 0,
          # xintercept = bayestestR::rope_range(fit),
          color = viridis::viridis(100)[1],
          linetype = "dashed",
          linewidth = 0.2
        ) +
        # ggplot2::annotate(
        #   x = 0,
        #   y = 6.75,
        #   geom = "text",
        #   label = "ROPE",,
        #   color = viridis::viridis(100)[1],
        #   size = 2,
        # ) +
        ggdist::stat_slabinterval(
          mapping = ggplot2::aes(
            # fill = ggplot2::after_stat(
            #   abs(x) < abs(bayestestR::rope_range(fit)[1])
            # )
            fill = significance
          ),
          point_size = 0.3,
          color = "black",
          # color = viridis::viridis(100)[1],
          .width = .95,
          # interval_size = 0.1,
          # .width = c(.5, .75, .89, .95),
          interval_size_range = c(0.2, 0.2),
          slab_alpha = 0.3,
          scale = 0.70,
          na.rm = TRUE,
          show.legend = FALSE,
        ) +
        ggplot2::geom_text(
          data = 
            hyp_table |> 
            dplyr::mutate(
              text = ifelse(
                BF_10 < 0.001 | BF_10 > 1000, 
                format(BF_10, digits = 3, scientific = TRUE), 
                as.character(round(BF_10, 2))
              )
            ),
          mapping = ggplot2::aes(
            x = Estimate,
            y = Hypothesis,
            label =
              latex2exp::TeX(
                sprintf(
                  r"($\BF_{10}$: \ %s)",
                  text
                ),
                output = "character"
              )
          ),
          parse = TRUE,
          hjust = ifelse(hyp_table$Estimate < 0, 1, 0),
          nudge_x = ifelse(
            hyp_table$Estimate < 0, 
            -1 * nudge_mult, 
            1 * nudge_mult
          ),
          vjust = 0,
          nudge_y = 0.2,
          size = 2,
          color = ifelse(
            (hyp_table$CI_low > 0 & hyp_table$CI_high > 0) | 
              (hyp_table$CI_low < 0 & hyp_table$CI_high < 0),
            viridis::viridis(100)[30],
            viridis::viridis(100)[1]
          )
        ) +
        ggplot2::scale_fill_manual(
          values = c(
            viridis::viridis(100)[5],
            viridis::viridis(100)[55]
          )
        ) +
        ggplot2::scale_x_continuous(
          # limits = range(c(posteriors$value, 0)) * plot.area.prop,
          breaks = scales::breaks_pretty(15),
          expand = ggplot2::expansion(mult = c(exp_mult, exp_mult))
        ) +
        ggplot2::scale_y_discrete(
          labels = function(x) stringr::str_to_title(x)
        ) +
        theme_pdf(
          base_theme = ggplot2::theme_minimal,
          axis.text.y = ggplot2::element_text(
            size = 6,
            color = "black",
            margin = ggplot2::margin(r = 50)
          )
        ) +
        ggplot2::labs(
          x = xlab,
          y = NULL
          # y = if(non.zero) "Hypothesis" else "Contrasts"
        )
      
      return(gg_distribution)
    }
    
    return(hyp_table)
    
}