# Benchmark behind the timing table of the PGM2 R Journal article.
# Run from this directory:  Rscript benchmark.R
# Writes benchmark-results.csv and benchmark-session.txt.
library(PGM2)

calls <- list(
  list(label = "BIB(7, 2)",        f = function() BIB(7, 2)),
  list(label = "BIB(5, 3)",        f = function() BIB(5, 3)),
  list(label = "Qn(4, 1)",         f = function() Qn(4, 1)),
  list(label = "Qn(5, 2)",         f = function() Qn(5, 2)),
  list(label = "Qn(6, 3)",         f = function() Qn(6, 3)),
  list(label = "Qn(7, 1)",         f = function() Qn(7, 1)),
  list(label = "Qn(7, 2)",         f = function() Qn(7, 2)),
  list(label = "Qn(7, 3)",         f = function() Qn(7, 3)),
  list(label = "Qn(4, 2, p = 3)",  f = function() Qn(4, 2, p = 3)),
  list(label = "Steps(5, 1)",      f = function() Steps(5, 1))
)

# short calls are repeated and the median reported; long calls run once
reps_for <- function(sec) if (sec < 1) 11L else 1L

out <- NULL
for (cl in calls) {
  t0 <- proc.time()[["elapsed"]]; res <- cl$f(); one <- proc.time()[["elapsed"]] - t0
  n <- reps_for(one)
  if (n > 1L) {
    times <- vapply(seq_len(n), function(i) {
      t0 <- proc.time()[["elapsed"]]; cl$f(); proc.time()[["elapsed"]] - t0
    }, numeric(1))
    el <- stats::median(times)
  } else {
    el <- one
  }
  runs <- if (!is.null(res$V)) res$V else res$UDs[[1]]$n
  fac  <- if (!is.null(res$R)) res$R else
          if (!is.null(res$K)) res$K else length(res$UDs)
  out <- rbind(out, data.frame(call = cl$label, runs = runs, factors = fac,
                               reps = n, seconds = round(el, 3),
                               object_MB = round(as.numeric(
                                 utils::object.size(res)) / 1024^2, 2)))
}
write.csv(out, "benchmark-results.csv", row.names = FALSE)
capture.output(sessionInfo(), file = "benchmark-session.txt")
cat(paste(capture.output(print(out, row.names = FALSE)), collapse = "\n"), "\n")
