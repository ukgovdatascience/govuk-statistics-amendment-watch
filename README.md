# GOV.UK Retraction scraper

@matt-dray's idea:

* `./scrape-stats-list.R`

    1. Scrape https://www.gov.uk/government/statistics (or its [RSS
       feed](https://www.gov.uk/government/statistics.atom?publication_filter_option=statistics))
       for stats publications.
    1. Get the [GOV.UK Content
       API](https://content-api.publishing.service.gov.uk/#gov-uk-content-api) [JSON
       representation](https://www.gov.uk/api/content/government/statistics/egg-statistics)
       of the pages.

* `./explore-amendments.R`

    1. Explore changes to each publication, in particular any amendments

    Rendered to `./explore-amendments.html`
