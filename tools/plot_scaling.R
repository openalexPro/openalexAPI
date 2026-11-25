# Draws man/figures/scaling.svg (README) from tools/benchmark_scaling.csv.
# The table comes from tools/benchmark.R runs on snapshot partitions of
# 10,000 to 9.6 million works (OAAPI_BENCH_CORPUS), on a 14-core Mac with 36 GB.
d <- utils::read.csv("tools/benchmark_scaling.csv")
m <- d$works / 1e6
ticks <- c(0.01, 0.1, 1, 10)
yt <- c(0.01, 0.1, 1, 10, 100)
lab <- function(x) format(x, drop0trailing = TRUE, scientific = FALSE, trim = TRUE)

grDevices::svg("man/figures/scaling.svg", width = 9, height = 3.6, pointsize = 11)
graphics::par(mfrow = c(1, 2), mar = c(4, 4.6, 2.2, 1), mgp = c(3.2, 0.7, 0), las = 1)

# query times
graphics::plot(m, d$query_max_s, log = "xy", type = "n", xaxt = "n", yaxt = "n", ylim = c(0.005, 80),
               xlab = "million works", ylab = "seconds", main = "Query times (75 cases)")
graphics::axis(1, ticks, lab(ticks)); graphics::axis(2, yt, lab(yt))
graphics::rect(36 / max(d$database_gb / m), 0.001, 100, 1000, col = "grey92", border = NA)
graphics::text(9.8, 0.008, "database\n> RAM", adj = c(1, 0), cex = 0.8, col = "grey40")
cols <- c("#1b6ca8", "#e08a00", "#c0392b")
for (i in 1:3) {
  y <- d[[c("query_median_s", "query_p90_s", "query_max_s")[i]]]
  graphics::lines(m, y, type = "b", pch = 19, col = cols[i])
}
graphics::legend("topleft", c("median", "90th percentile", "slowest"), col = cols,
                 pch = 19, lty = 1, bty = "n", cex = 0.85)
graphics::box()

# database size and build time
graphics::plot(m, d$database_gb, log = "xy", type = "b", pch = 19, col = cols[1], xaxt = "n", yaxt = "n",
               ylim = c(0.02, 100), xlab = "million works", ylab = "GB  /  minutes",
               main = "Database and build")
graphics::axis(1, ticks, lab(ticks)); graphics::axis(2, yt, lab(yt))
graphics::lines(m, d$parquet_gb, type = "b", pch = 1, col = cols[1], lty = 2)
graphics::lines(m, d$build_s / 60, type = "b", pch = 19, col = cols[2])
graphics::lines(m, d$build_footprint_gb, type = "b", pch = 19, col = cols[3])
graphics::abline(h = 36, col = "grey60", lty = 3)
graphics::text(0.6, 36, "RAM 36 GB", adj = c(1, -0.4), cex = 0.8, col = "grey40")
graphics::legend("bottomright", c("database (GB)", "Parquet (GB)", "build (minutes)",
                              "peak memory, build (GB)"),
                 col = cols[c(1, 1, 2, 3)], pch = c(19, 1, 19, 19), lty = c(1, 2, 1, 1),
                 bty = "n", cex = 0.85)
grDevices::dev.off()
