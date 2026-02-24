# Open Case Studies Website

This repository contains the code for the [Open Case Studies](https://www.opencasestudies.org/) project website.

The site is built with **R Markdown** using `rmarkdown::render_site()`. The output is written to the `docs/` directory, which is served via GitHub Pages.

------------------------------------------------------------------------

## Contributing

1.  Edit the `.Rmd` files in the main directory (or in `post/` / `team/` subdirectories)
2.  Knit individual files in RStudio, or run `rmarkdown::render_site()` in the R console to build the entire site
3.  Preview the output in `docs/`
4.  Commit and push your changes — GitHub Pages will serve from `docs/`

------------------------------------------------------------------------

## Site Structure

| File                    | Description                                 |
|-------------------------|---------------------------------------------|
| `index.Rmd`             | Home page                                   |
| `blog.Rmd`              | Blog listing page                           |
| `team.Rmd`              | Team listing page                           |
| `talks.Rmd`             | Talks timeline page                         |
| `contact.Rmd`           | Contact form page                           |
| `search.Rmd`            | Case study search page                      |
| `survey.Rmd`            | User survey page                            |
| `setup.Rmd`             | About page                                  |
| `_site.yml`             | Site-wide config (navbar, theme, CSS)       |
| `styles.css`            | Global CSS styles                           |
| `resources/header.html` | Custom HTML injected into every page header |

------------------------------------------------------------------------

## Adding a Blog Post

1.  Create a new `.Rmd` file in the `post/` directory
2.  Name it with the format `YYYY-MM-DD-your-post-title.Rmd`
3.  Include a YAML front matter block with at least `title`, `author`, and `date`:

``` yaml
---
title: Your Post Title
author: Your Name
date: '2026-02-24'
---
```

4.  Knit the file — it will appear automatically on `blog.html`

------------------------------------------------------------------------

## Adding or Updating a Team Member

-   Each team member has a directory under `team/` named with first initial followed by last name (e.g. `cwright` for Carrie Wright)
-   The directory should contain:
    -   `index.Rmd` — the member's profile page
    -   `avatar.png` — the member's photo
-   The `index.Rmd` YAML controls the name, role, and group (`Faculty`, `Research Assistants`, or `OCS Alumni`) displayed on `team.html`

------------------------------------------------------------------------

## Adding a Talk

Edit `talks.Rmd` and add an entry under the appropriate year section.

------------------------------------------------------------------------

## Open Case Studies GitHub

All case study repositories live at: <https://github.com/opencasestudies>

The case study search table is hosted separately at <http://www.opencasestudies.org/OCS_search/> from the repo: <https://github.com/opencasestudies/OCS_search>
