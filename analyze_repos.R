#!/usr/bin/env Rscript
#
# Analyze release-asset download counts for a GitHub user's repos,
# with an optional filter on repo creation date.
#
# Usage:
#   Rscript analyze_repos.R [since] [until]
#   since, until: dates in "YYYY-MM-DD" format (either may be omitted)
#   since defaults to 90 days ago; until defaults to no upper bound

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(stringr)
  library(lubridate)
  library(ggplot2)
  library(jsonlite)
})

gh_username <- "davidjayjackson"

run_gh <- function(args) {
  out <- system2("gh", args, stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status") %||% 0L
  list(status = status, output = out)
}

fetch_repos <- function(owner) {
  res <- run_gh(c(
    "repo", "list", owner, "--limit", "1000",
    "--json", "name,createdAt,isFork,isArchived"
  ))
  if (res$status != 0) {
    stop("gh repo list failed:\n", paste(res$output, collapse = "\n"))
  }
  fromJSON(paste(res$output, collapse = "\n")) |>
    as_tibble() |>
    mutate(created_at = as_datetime(createdAt)) |>
    select(name, created_at, is_fork = isFork, is_archived = isArchived)
}

fetch_release_downloads <- function(owner, repo) {
  res <- run_gh(c(
    "api", sprintf("repos/%s/%s/releases", owner, repo),
    "--paginate", "--slurp"
  ))
  if (res$status != 0) return(0)

  pages <- fromJSON(paste(res$output, collapse = "\n"), simplifyVector = FALSE)
  releases <- purrr::flatten(pages)
  if (length(releases) == 0) return(0)

  releases |>
    map_dbl(function(rel) {
      assets <- rel$assets
      if (length(assets) == 0) return(0)
      assets |> map_dbl(~ .x$download_count %||% 0) |> sum()
    }) |>
    sum()
}

filter_by_created <- function(repos, since = NULL, until = NULL) {
  if (!is.null(since)) repos <- filter(repos, created_at >= as_datetime(since))
  if (!is.null(until)) repos <- filter(repos, created_at <= as_datetime(until))
  repos
}

analyze <- function(owner = gh_username, since = NULL, until = NULL) {
  message("Fetching repo list for ", owner, "...")
  repos <- fetch_repos(owner) |> filter_by_created(since, until)

  message("Fetching release download counts for ", nrow(repos), " repo(s)...")
  repos |> mutate(downloads = map_dbl(name, ~ fetch_release_downloads(owner, .x))) |>
    filter(downloads > 0) |>
    arrange(desc(downloads))
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  since <- if (length(args) >= 1 && nzchar(args[[1]])) args[[1]] else as.character(today() - days(90))
  until <- if (length(args) >= 2 && nzchar(args[[2]])) args[[2]] else NULL

  results <- analyze(since = since, until = until)
  print(results, n = Inf)

  if (nrow(results) > 0) {
    plot <- ggplot(results, aes(x = reorder(name, downloads), y = downloads)) +
      geom_col() +
      coord_flip() +
      labs(
        x = "Repository", y = "Release Asset Downloads",
        title = paste("Release Downloads by Repo -", gh_username)
      )
    ggsave("downloads_by_repo.png", plot, width = 8, height = 6)
    message("Chart saved to downloads_by_repo.png")
  }
}
