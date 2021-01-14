
#' This is the BESTHR source code used to customise functions
#' as of 11/2020




















library(rlang)


#' Convert named columns to factors
#'
#' \code{factorise_cols} returns a copy of the passed data frame in which all
#' explicitly named columns are converted to factors with \code{as.factor}
#' All columns with only NA values are ommitted.
#'
#' @param df data frame
#' @param col_list vector of quoted column names
#' @keywords internal
#'
factorise_cols <- function(df, col_list ){
  not_all_na <- function(x) any(!is.na(x))
  
  for ( i in col_list){
    df <- df %>% dplyr::mutate(!!(i) := as.factor(!!(i)) )
  }
  df %>% dplyr::select_if(not_all_na)
}


obj1 <- function(){
  d1 <- make_data()
  estimate(d1, score, group)
}

obj2 <- function(){
  d2 <- make_data2()
  estimate(d2, score_column_name, sample_column_name, rep_column_name )
}

obj3 <- function(){
  d3 <- make_data3()
  estimate(d3, score, sample, rep, nits = 1000)
}

#' @export
make_data <- function(){
  group_1 <- sample(1:10, 10, replace = TRUE,
                    prob = c(rep(0.1/7, 7), rep(0.3, 3))
  )
  group_2 <- sample(1:10, 10, replace = TRUE)
  group <- rep(c("A", "B"), each = 10)
  tibble::tibble(score = c(group_1, group_2), group = group)
}

#' @export
make_data2 <- function(){
  group_1 <- sample(1:10, 12, replace = TRUE,
                    prob = c(rep(0.1/7, 7), rep(0.3, 3))
  )
  group_2 <- sample(1:10, 12, replace = TRUE)
  tech_rep <- rep( rep(1:3, each = 4), 2)
  group <- rep(c("A", "B"), each = 12)
  
  tibble::tibble(
    score_column_name = c(group_1, group_2),
    sample_column_name = group,
    rep_column_name = tech_rep
  )
}

#' @export
make_data3 <- function(){
  group_1 <- sample(1:10, 12, replace = TRUE,
                    prob = c(rep(0.1/7, 7), rep(0.3, 3))
  )
  group_2 <- sample(1:10, 12, replace = TRUE)
  group_3 <- sample(7:15, 12, replace = TRUE)
  tech_rep <- rep( rep(1:3, each = 4), 3)
  group <- rep(c("A", "B", "C"), each = 12)
  tibble::tibble(score = c(group_1, group_2, group_3),
                 sample = group, rep = tech_rep)
  
  
}


#' calculate ranks of a score column and adds result to a dataframe
#'
#' \code{add_rank} returns a copy of the passed data frame containing a new
#'  column called \code{rank} which has the ranks of the named score column
#'  @param df data frame
#'  @param quo_score_col quoted score column name
#'  @keywords internal
#'
add_rank <- function(df, quo_score_col){
  df %>% dplyr::mutate(rank = rank(!!quo_score_col))
}

#' resamples a rank column in a dataframe based on groups
#'
#' \code{bstrap_sample} performs a single iteration of bootstrapping of a rank
#' column in a data frame. The rows matching \code{control} in the quoted
#' \code{quo_group_col} column are removed. The remaining rows are grouped by
#' \code{quo_group_col} and in each group the rank column is resampled with
#' replacement and the mean rank of the resampling for each group is returned
#'
#' @param iteration integer giving the current bootstrap iteration from the
#' calling function
#' @param quo_score_col quoted column name containing the HR scores
#' @param quo_group_col quoted group name containing the group name of the
#' observation
#' @param df input data frame
#' @param control character naming the control group that will be removed prior
#' to bootstrapping
#' @keywords internal
bstrap_sample <- function(iteration, quo_score_col,
                          quo_group_col, df, control = "A"){
  
  df %>%
    dplyr::filter(!!quo_group_col != control) %>%
    dplyr::group_by(!!quo_group_col) %>%
    dplyr::mutate(resample = sample(rank, length(rank), replace = TRUE)) %>%
    dplyr::summarize(mean = mean(resample), iteration = iteration)
  
  
}

#' runs bootstrapping of ranks
#'
#' \code{boostrap_dist} runs multiple bootstrap resamplings using
#' \code{\link{bstrap_sample}} to generate a data frame of bootstrap rank means
#' of per group scores
#' @param df input data frame
#' @param quo_score_col quoted column name containing the HR scores
#' @param quo_group_col quoted group name containing the group column
#' @param nits number of bootstrap iterations to do
#' @param control character naming the control group that will be removed prior
#' to bootstrapping
#' @keywords internal
bootstrap_dist <- function(df, quo_score_col, quo_group_col,
                           nits=10, control = "A"){
  
  
  lapply(1:nits, bstrap_sample, quo_score_col, quo_group_col, df,
         control = control) %>%
    dplyr::bind_rows()
}

#' gets confidence interval limits for means of bootstrapped ranks
#'
#' \code{conf_intervals} calculates a low and a high quantile for the
#' \code{mean} column of each group of a dataframe
#'
#' @param df input data frame
#' @param quo_group_col quoted group name containing the column to group on
#' @param low the low probability value of the quantile
#' @param high the high probability value of the quantile
#' @keywords internal
conf_intervals <- function(df, quo_group_col, low = 0.05, high=0.95){
  df %>% dplyr::group_by(!!quo_group_col) %>%
    dplyr::summarise(
      low = quantile(mean, prob = low, names = FALSE),
      high = quantile(mean, prob = high, names = FALSE),
      mean = mean(mean)
      
    )
}

#' Get mean rank of groups in a data frame
#'
#' \code{group_means} groups the provided dataframe by a column and returns
#' a summary dataframe with a mean column containing the mean of the group's
#' rank column
#'
#' @param df input data frame
#' @param quo_group_col quoted group name containing the column to group on
#' @keywords internal
group_means <- function(df, quo_group_col){
  
  df %>%
    dplyr::group_by( !!quo_group_col ) %>%
    dplyr::summarize(mean = mean(rank))
}

#' Get number of observations in a group in a data frame
#'
#' \code{group_ns} groups the provided dataframe by a column and returns
#' a summary dataframe with an column \code{n} containg the number of
#' observations in a group
#'
#' @param df input data frame
#' @param quo_group_col quoted group name containing the column to group on
#' @keywords internal
group_ns <- function(df, quo_group_col){
  df %>%
    dplyr::group_by(!!quo_group_col) %>%
    dplyr::summarize(n = dplyr::n() )
}


#' Perform bootstrap estimation of confidence intervals of ranked HR scores
#'
#' \code{estimate} carries out estimation of bootstrap confidence intervals on
#' ranked score data. Returns a \code{hrest} object of the result
#' Proceeeds by calculating score ranks, then bootstrapping ranks in non-control
#' groups retaining the mean for each bootstrap iteration. Calculates
#' low and high quantiles of bootstrap mean distributions for each group.
#' If technical replicates are provided in a second grouping column these will
#' be averaged before proceeding.
#'
#' @param df data frame of score and group data. Contains minimally a score and
#' group column
#' @param ... bare names of columns to use, minimally the score column and the
#' group column in that order. Optionally a third technical replicate column can
#' be provided
#' @param control the value of the grouping column taken to be the control group
#' @param nits the number of bootstap iterations to be done
#' @param low the low probability value of the quantile
#' @param high the high probability value of the quantile
#' @return a list object of class "hrest"
#' @examples
#'  d1 <- make_data()
#'  estimate(d1, score, group)
#'
#'  d2 <- make_data2()
#'  estimate(d2, score_column_name, sample_column_name, rep_column_name )
#'
#'  d3 <- make_data3()
#'  estimate(d3, score, sample, rep, nits = 1000)
#'
#' @export
#'
estimate <- function(df, ..., control = "A", nits = 100,
                     low = 0.025, high=0.975 ){
  
  
  quo_list <- dplyr::enquos(...)
  quo_score_col <- quo_list[[1]]
  quo_group_col <- quo_list[[2]]
  quo_tech_rep_col <- NULL
  if ( length(quo_list) == 3){
    quo_tech_rep_col <- quo_list[[3]]
  }
  
  gdf <- df
  
  if (length(quo_list) == 3){
    gdf <- df %>%
      dplyr::group_by(!!quo_group_col, !!quo_tech_rep_col ) %>%
      dplyr::summarize(tmp_mean = mean(!!quo_score_col)) %>%
      dplyr::select(!!quo_group_col, !!quo_score_col := tmp_mean) %>%
      dplyr::ungroup()
  }
  
  rdf <- add_rank(gdf, quo_score_col)
  bootstraps <- bootstrap_dist(rdf, quo_score_col, quo_group_col,
                               control, nits = nits)
  obj <- list(
    control = control,
    group_means = group_means(rdf, quo_group_col),
    ranked_data = rdf,
    original_data = add_rank(df, quo_score_col),
    bootstraps = bootstraps,
    ci = conf_intervals(bootstraps, quo_group_col, low = low, high = high),
    nits = nits,
    low = low,
    high = high,
    group_n = group_ns(rdf, quo_group_col),
    column_info = quo_list
    
    
  )
  class(obj) <- "hrest"
  return(obj)
}

#' @export
print.hrest <- function(hrest){
  
  cat( stringr::str_glue(
    "
    besthr (HR Rank Score Analysis with Bootstrap Estimation)
    =========================================================

    Control: {hrest$control}
    \n
    ")
  )
  
  group_col <- names(hrest$group_n)[ names(hrest$group_n) != "n" ][[1]]
  
  group_names <- hrest$group_n %>% dplyr::select(-n)
  group_names <- group_names[[1, drop = TRUE]]
  group_names <- group_names[group_names != hrest$control]
  
  #col_name <- rlang::sym(group_col)
  col_name <- rlang::ensym(group_col)
  condition <- dplyr::quo(hrest$control)
  control_n <- hrest$group_n %>%
    dplyr::filter( !!col_name  == !!(condition) )
  control_n <- control_n$n[1]
  
  group_ns <- hrest$group_n %>%
    dplyr::filter( !!col_name != !!(condition))
  
  control_mean <- hrest$group_means %>%
    dplyr::filter( !!col_name == !!(condition) )
  control_mean <- control_mean$mean[1]
  
  group_means <-hrest$group_means %>%
    dplyr::filter( !!col_name != !!(condition) )
  
  
  for (i in 1:length(group_names) ){
    current_group <- group_names[i]
    group_n <- group_ns$n[i]
    
    mean_difference <- control_mean - group_means$mean[i]
    cat(stringr::str_glue(
      "
         Unpaired mean rank difference of {hrest$control} ({control_mean}, n={control_n}) minus {current_group} ({group_means$mean[i]}, n={group_n})
          {mean_difference}
         Confidence Intervals ({hrest$low}, {hrest$high})
          {hrest$ci$low[i]}, {hrest$ci$high[i]}
         \n
         "
    ))
  }
  cat(stringr::str_glue(
    "
     {hrest$nits} bootstrap resamples."
  ))
  
}

#' dot plot of ranked data without technical replicates
#'
#' \code{dot_plot} returns a ggplot object of ranked data with group on the
#' x-axis and rank on the y-axis. Point size indicates the number of
#' observations seen at that point. A per group horizontal line shows the group
#' ranked mean
#'
#' @param hrest the hrest object from \code{estimate}
#' @param group_col quoted group column name
#' @keywords internal
#'
dot_plot <- function(hrest, group_col){
  hrest$ranked_data %>%
    dplyr::group_by(!!group_col, rank) %>%
    dplyr::summarise(count = dplyr::n() ) %>%
    ggplot2::ggplot() +
    ggplot2::aes(!!group_col, rank) +
    ggplot2::geom_point(ggplot2::aes(size = count, colour = !!group_col,
                                     fill = !!group_col)) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = mean, colour = !!group_col),
                        data = hrest$group_means, linetype = 3, size = 1) +
    ggplot2::theme_minimal()
  
}

#' dot plot of score data with technical replicates
#'
#' \code{tech_rep_dot_plot} returns a ggplot object of score data with group on
#' technical replicate on the x-axis, score on the y-axis with point size
#' representing the number of observations at that point. Facets represent
#' individual groups
#' @param hrest the hrest object from \code{estimate}
#' @param score_col quoted score column name
#' @param group_col quoted group column name
#' @param tech_rep_col quoted tech replicate column name
#' @keywords internal
tech_rep_dot_plot <- function(hrest, score_col, group_col, tech_rep_col){
  
  hrest$original_data %>% factorise_cols( list(group_col, tech_rep_col)) %>%
    dplyr::group_by(!!group_col, !!tech_rep_col, !!score_col) %>%
    dplyr::summarise(count = dplyr::n() ) %>%
    ggplot2::ggplot() +
    ggplot2::aes( !!tech_rep_col, !!score_col )  +
    ggplot2::geom_point(
      ggplot2::aes(
        size = count,
        colour = !!group_col,
        fill = !!group_col
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::facet_wrap( ggplot2::vars(!!group_col), strip.position = "bottom", nrow = 1) +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   strip.placement = "outside") +
    scale_size(range = c(1, 10), breaks=c(1,2,5,10))
  
}

#' plots the \code{hrest} object
#'
#' returns a ggplot object representing the hrest object from
#' \code{\link{estimate}}. The content of left panel varies according to the
#' value of the \code{which} parameter. If \code{which = "rank_simulation"} is
#' used a plot of rank score values will be plotted in the left panel. In this
#' case technical replicates will be averaged if provided. If
#' \code{which = "just_data" } a plot of scores only is created and technical
#' replicates are displayed as is. In each case, the right hand panel shows the
#' rank bootstrap distribution and confidence interval boundaries for all non-
#' control groups.
#'
#'  @param hrest the \code{hrest} object from \code{\link{estimate}}
#'  @param which the type of left hand panel to create. Either "rank_simulation"
#'  or "just_data"
#' @export
plot.hrest <- function(hrest, which = "rank_simulation"){
  
  group_col <- names(hrest$group_n)[ names(hrest$group_n) != "n" ][[1]]
  group_col <- rlang::sym(group_col)
  
  a <- NULL
  if (length(hrest$column_info) == 3 && which == "just_data" ){
    quo_score_col <- hrest$column_info[[1]]
    quo_group_col <- hrest$column_info[[2]]
    quo_tech_rep_col <- hrest$column_info[[3]]
    
    a <- tech_rep_dot_plot(hrest, quo_score_col, quo_group_col,
                           quo_tech_rep_col)
    
  }
  else {
    quo_group_col <- hrest$column_info[[2]]
    a <- dot_plot(hrest, quo_group_col)
  }
  
  
  legend <- cowplot::get_legend(a + ggplot2::theme(legend.position = "bottom"))
  
  c <- hrest$bootstraps %>%
    ggplot2::ggplot() + ggplot2::aes(mean, UQ(group_col),
                                     fill = factor(..quantile..)) +
    ggplot2::xlim(min(hrest$ranked_data$rank), max(hrest$ranked_data$rank)) +
    ggridges::stat_density_ridges(geom = "density_ridges_gradient",
                                  calc_ecdf = TRUE,
                                  quantiles = c(hrest$low, hrest$high)) +
    ggplot2::scale_fill_manual(values =
                                 c("#0000FFA0",  "#A0A0A0A0", "#0000FFA0") ) +
    ggplot2::coord_flip() + ggplot2::theme_minimal()
  
  
  d <- cowplot::plot_grid(a + ggplot2::theme(legend.position = "none"),
                          c + ggplot2::theme(legend.position = "none"),
                          align = "vh",
                          nrow = 1, axis = c("b"))
  return(cowplot::plot_grid(d, legend, ncol = 1, rel_heights = c(1, .2)) )
  
  
  
}