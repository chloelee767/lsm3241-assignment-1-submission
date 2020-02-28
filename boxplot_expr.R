# Helper function to plot box plot of gene expression of selected probes
# Sample usage:
# boxplot_probes('AFFX-BkGr-GC03_st',eset)
# boxplot_probes(c('AFFX-BkGr-GC03_st','AFFX-BkGr-GC04_st'),eset)


# returns a dataframe of normalised expression readings for the given probe_ids
# rows: probe_id, columns: 1 column for each sample (ie. GMS)
get_probe_expr <- function(probe_ids, eset) {
  data.frame(exprs(eset))[probe_ids,]
}

# returns treatment type (a factor) for a given GMS
get_treat <- function(sample, eset) {
  pData(eset)[sample,]$treat
}

# input: output of get_probe_expr
# output: a dataframe suitable to box plot
# columns: expr, treat, sample
to_boxplot_df <- function(df, eset) {
  result <- data.frame(matrix(ncol=3,nrow=0))
  colnames(result) <- c('expr','treat','sample')
  for (r in 1:nrow(df)) {
    expr <- unlist(df[r,], use.names = FALSE)
    sample <- colnames(df)
    eset_treat_fn <- function(s){
      get_treat(s, eset)
    }
    treat <- sapply(sample,eset_treat_fn,USE.NAMES = FALSE)
    result <- rbind(result, setNames(list(expr,treat,sample),colnames(result)))
  }
  result  
}

# Inputs:
# probe_ids : a single probe id or list of probe ids
# eset: eset
# output: boxplot of expression against treatment (KO_old,KO_young, WT_old, WT_young)
boxplot_probes <- function(probe_ids, eset) {
  df <- get_probe_expr(probe_ids,eset)
  df <- to_boxplot_df(df,eset)
  show(df)
  boxplot(expr ~ treat, data=df)
}
