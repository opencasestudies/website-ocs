library(yaml)
library(stringr)
library(rmarkdown)

# ── Configuration ─────────────────────────────────────────────────────────────
input_dir <- "."   # top-level folder containing your team member subfolders
# ─────────────────────────────────────────────────────────────────────────────

# Null-coalescing helper
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b

# ── FIX 2: detect avatar file in the same folder ─────────────────────────────
find_avatar <- function(folder) {
  hits <- list.files(folder, pattern = "(?i)avatar\\.(png|jpe?g)$",
                     full.names = FALSE)
  if (length(hits)) return(hits[1])
  # fallback: any image file in the folder
  imgs <- list.files(folder, pattern = "(?i)\\.(png|jpe?g)$",
                     full.names = FALSE)
  if (length(imgs)) return(imgs[1])
  "avatar.png"   # default if nothing found
}

# ── Main conversion function ──────────────────────────────────────────────────
convert_profile <- function(md_path, out_dir) {
  
  raw    <- readLines(md_path, warn = FALSE)
  folder <- dirname(md_path)
  
  # ── FIX 2: avatar ────────────────────────────────────────────────────────
  avatar_file <- find_avatar(folder)
  
  # ── 1. Parse front matter & body ─────────────────────────────────────────
  if (raw[1] == "+++") {
    # ── TOML (+++ ... +++) ──────────────────────────────────────────────────
    end_fm   <- which(raw == "+++")[2]
    fm_lines <- raw[2:(end_fm - 1)]
    body     <- paste(raw[(end_fm + 1):length(raw)], collapse = "\n")
    
    get_toml <- function(key) {
      line <- fm_lines[str_detect(fm_lines, paste0("^\\s*", key, "\\s*="))]
      if (!length(line)) return(NA_character_)
      str_trim(str_remove(line[1], paste0("^\\s*", key, "\\s*=\\s*"))) |>
        str_remove_all('^"|"$')
    }
    
    name  <- get_toml("name")
    role  <- get_toml("role")
    email <- get_toml("email")
    bio   <- get_toml("bio")
    
    # FIX 1: read user_groups from TOML, e.g. user_groups = ["Staff", "Faculty"]
    ug_raw     <- get_toml("user_groups")
    user_group <- str_remove_all(ug_raw, '[\\[\\]"]') |>
      str_split(",") |> unlist() |> str_trim() |> (\(x) x[nchar(x) > 0])()
    if (length(user_group) == 0 || all(is.na(user_group))) user_group <- "Faculty"
    user_group <- user_group[1]
    
    # Organizations: first occurrence
    org_line <- fm_lines[str_detect(fm_lines, 'name\\s*=\\s*"[^"]+"')][1]
    org_name <- str_match(org_line, 'name\\s*=\\s*"([^"]+)"')[, 2]
    org_url  <- str_match(org_line, 'url\\s*=\\s*"([^"]+)"')[, 2]
    if (is.na(org_url)) org_url <- ""
    
    # FIX 3: parse [[social]] blocks properly
    # Split the front matter on [[social]] boundaries then extract icon + link
    fm_text     <- paste(fm_lines, collapse = "\n")
    social_blocks <- str_split(fm_text, "\\[\\[social\\]\\]")[[1]][-1]
    
    extract_social_field <- function(block, field) {
      m <- str_match(block, paste0('(?m)^\\s*', field, '\\s*=\\s*"([^"]+)"'))
      if (!is.na(m[1, 2])) m[1, 2] else NA_character_
    }
    
    social <- list()
    for (blk in social_blocks) {
      icon <- extract_social_field(blk, "icon")
      link <- extract_social_field(blk, "link")
      if (is.na(icon) || is.na(link)) next
      key <- switch(icon,
                    "envelope"      = "email",
                    "home"          = "website",
                    "twitter"       = "twitter",
                    "linkedin"      = "linkedin",
                    "google-scholar"= "scholar",
                    "github"        = "github",
                    icon   # fallback: use icon name as key
      )
      social[[key]] <- link
    }
    
    # Education
    edu_blocks <- str_match_all(
      fm_text,
      '\\[\\[education\\.courses\\]\\][^\\[]*course\\s*=\\s*"([^"]+)"[^\\[]*institution\\s*=\\s*"([^"]+)"[^\\[]*year\\s*=\\s*(\\d+)'
    )[[1]]
    
    education <- if (nrow(edu_blocks) > 0) {
      lapply(seq_len(nrow(edu_blocks)), function(i)
        list(degree      = edu_blocks[i, 2],
             institution = edu_blocks[i, 3],
             year        = as.integer(edu_blocks[i, 4])))
    } else list()
    
  } else if (raw[1] == "---") {
    # ── YAML (--- ... ---) ──────────────────────────────────────────────────
    end_fm  <- which(raw == "---")[2]
    fm_text <- paste(raw[2:(end_fm - 1)], collapse = "\n")
    body    <- paste(raw[(end_fm + 1):length(raw)], collapse = "\n")
    fm      <- yaml::yaml.load(fm_text)
    
    name       <- fm$name %||% fm$title
    role       <- fm$role
    email      <- fm$email
    bio        <- fm$bio
    org_name   <- (fm$organizations[[1]]$name) %||% fm$organization
    org_url    <- (fm$organizations[[1]]$url)  %||% fm$organization_url %||% ""
    
    # FIX 1: read user_groups / user_group from YAML
    ug_raw     <- fm$user_groups %||% fm$user_group %||% list("Faculty")
    user_group <- if (is.list(ug_raw)) ug_raw[[1]] else ug_raw[1]
    
    # FIX 3: YAML social — support both named list and icon-based list
    raw_social <- fm$social %||% list()
    if (is.list(raw_social) && !is.null(names(raw_social))) {
      social <- raw_social   # already a named list like list(github = "...")
    } else {
      # list of blocks with icon/link keys
      social <- list()
      for (blk in raw_social) {
        icon <- blk$icon
        link <- blk$link
        if (is.null(icon) || is.null(link)) next
        key <- switch(icon,
                      "envelope"      = "email",
                      "home"          = "website",
                      "twitter"       = "twitter",
                      "linkedin"      = "linkedin",
                      "google-scholar"= "scholar",
                      "github"        = "github",
                      icon)
        social[[key]] <- link
      }
    }
    
    education <- fm$education$courses %||% fm$education %||% list()
    
  } else {
    message("Skipping ", md_path, ": no recognised front matter")
    return(invisible(NULL))
  }
  
  # ── 2. Build new YAML header ───────────────────────────────────────────────
  new_yaml <- list(
    title            = name,
    name             = name,
    role             = role,
    organization     = org_name,
    organization_url = org_url,
    email            = email,
    bio              = bio,
    user_group       = user_group,
    avatar           = avatar_file,
    education        = lapply(education, function(e)
      list(degree      = e$degree %||% e$course,
           institution = e$institution,
           year        = e$year)),
    social           = social
  )
  
  yaml_block <- paste0("---\n", yaml::as.yaml(new_yaml), "---")
  
  # ── 3. Social icon HTML ────────────────────────────────────────────────────
  make_icon <- function(href, title, icon_html) {
    if (is.null(href) || is.na(href) || href == "") return("")
    # FIX 3: ensure mailto: prefix for email links
    if (title == "Email" && !str_starts(href, "mailto:"))
      href <- paste0("mailto:", href)
    sprintf('<a href="%s" title="%s">%s</a>', href, title, icon_html)
  }
  
  # Inline SVGs — no external fonts or CDN needed
  svg_email   <- '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="48" height="48" fill="currentColor"><path d="M20 4H4c-1.1 0-2 .9-2 2v12c0 1.1.9 2 2 2h16c1.1 0 2-.9 2-2V6c0-1.1-.9-2-2-2zm0 4-8 5-8-5V6l8 5 8-5v2z"/></svg>'
  svg_website <- '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="48" height="48" fill="currentColor"><path d="M10 20v-6h4v6h5v-8h3L12 3 2 12h3v8z"/></svg>'
  svg_twitter <- '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="48" height="48" fill="currentColor"><path d="M18.244 2.25h3.308l-7.227 8.26 8.502 11.24H16.17l-5.214-6.817L4.99 21.75H1.68l7.73-8.835L1.254 2.25H8.08l4.713 6.231zm-1.161 17.52h1.833L7.084 4.126H5.117z"/></svg>'
  svg_linkedin <- '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="48" height="48" fill="currentColor"><path d="M19 3a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h14m-.5 15.5v-5.3a3.26 3.26 0 0 0-3.26-3.26c-.85 0-1.84.52-2.32 1.3v-1.11h-2.79v8.37h2.79v-4.93c0-.77.62-1.4 1.39-1.4a1.4 1.4 0 0 1 1.4 1.4v4.93h2.79M6.88 8.56a1.68 1.68 0 0 0 1.68-1.68c0-.93-.75-1.69-1.68-1.69a1.69 1.69 0 0 0-1.69 1.69c0 .93.76 1.68 1.69 1.68m1.39 9.94v-8.37H5.5v8.37h2.77z"/></svg>'
  svg_scholar <- '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="48" height="48" fill="currentColor"><path d="M12 3 1 9l4 2.18v6L12 21l7-3.82v-6l2-1.09V17h2V9L12 3zm6.82 6L12 12.72 5.18 9 12 5.28 18.82 9zM17 15.99l-5 2.73-5-2.73v-3.72L12 15l5-2.73v3.72z"/></svg>'
  svg_github  <- '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="48" height="48" fill="currentColor"><path d="M12 2A10 10 0 0 0 2 12c0 4.42 2.87 8.17 6.84 9.5.5.08.66-.23.66-.5v-1.69c-2.77.6-3.36-1.34-3.36-1.34-.46-1.16-1.11-1.47-1.11-1.47-.91-.62.07-.6.07-.6 1 .07 1.53 1.03 1.53 1.03.87 1.52 2.34 1.07 2.91.83.09-.65.35-1.09.63-1.34-2.22-.25-4.55-1.11-4.55-4.92 0-1.11.38-2 1.03-2.71-.1-.25-.45-1.29.1-2.64 0 0 .84-.27 2.75 1.02.79-.22 1.65-.33 2.5-.33.85 0 1.71.11 2.5.33 1.91-1.29 2.75-1.02 2.75-1.02.55 1.35.2 2.39.1 2.64.65.71 1.03 1.6 1.03 2.71 0 3.82-2.34 4.66-4.57 4.91.36.31.69.92.69 1.85V21c0 .27.16.59.67.5C19.14 20.16 22 16.42 22 12A10 10 0 0 0 12 2z"/></svg>'
  
  social_icons <- paste(Filter(nchar, c(
    make_icon(social[["email"]],    "Email",          svg_email),
    make_icon(social[["website"]],  "Website",        svg_website),
    make_icon(social[["twitter"]],  "Twitter / X",    svg_twitter),
    make_icon(social[["linkedin"]], "LinkedIn",        svg_linkedin),
    make_icon(social[["scholar"]],  "Google Scholar",  svg_scholar),
    make_icon(social[["github"]],   "GitHub",          svg_github)
  )), collapse = "\n")
  
  # ── 4. Education HTML ──────────────────────────────────────────────────────
  edu_html <- if (length(new_yaml$education) == 0) "" else
    paste(sapply(new_yaml$education, function(e) sprintf(
      '<div class="education-item">
<div class="education-icon">&#127891;</div>
<div class="education-content">
<div class="education-degree">%s, %s</div>
<div class="education-institution">%s</div>
</div>
</div>', e$degree, e$year, e$institution)), collapse = "\n\n")
  
  # Build name / role / org block
  # - With role:    name (plain) → role (grey) → org (plain or linked)
  # - Without role: name (plain) → org (blue hyperlink, like screenshot)
  has_role   <- !is.null(role) && !is.na(role) && nchar(trimws(role)) > 0
  has_org_url <- !is.null(org_url) && !is.na(org_url) && nchar(trimws(org_url)) > 0
  
  name_and_org_html <- if (has_role) {
    # Active member: role in grey, org as plain link
    org_part <- if (has_org_url)
      sprintf('<div class="profile-org"><a href="%s">%s</a></div>', org_url, org_name)
    else
      sprintf('<div class="profile-org">%s</div>', org_name %||% "")
    sprintf('<h1 class="profile-name">%s</h1>\n<div class="profile-role">%s</div>\n%s',
            name, role, org_part)
  } else {
    # Alumni: no role, org is the prominent blue hyperlink
    org_part <- if (has_org_url)
      sprintf('<div class="profile-org-alumni"><a href="%s">%s</a></div>', org_url, org_name)
    else
      sprintf('<div class="profile-org-alumni">%s</div>', org_name %||% "")
    sprintf('<h1 class="profile-name">%s</h1>\n%s', name, org_part)
  }
  
  # ── 5. Assemble .Rmd ───────────────────────────────────────────────────────
  body <- str_trim(body)
  
  rmd <- sprintf(
    '%s
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

<style>
.profile-container {
  max-width: 1200px;
  margin: 60px auto;
  padding: 0 40px;
}
.profile-layout {
  display: grid;
  grid-template-columns: 400px 1fr;
  gap: 80px;
  align-items: start;
}
.profile-left { text-align: center; }
.profile-avatar {
  width: 280px;
  height: 280px;
  border-radius: 50%%;
  object-fit: cover;
  margin: 0 auto 30px auto;
  display: block;
}
.profile-name {
  font-size: 2.5rem;
  font-weight: 400;
  color: #333;
  margin: 0 0 12px 0;
  line-height: 1.2;
}
.profile-role {
  font-size: 1.1rem;
  color: #888;
  font-weight: 300;
  margin: 0 0 10px 0;
}
.profile-org {
  font-size: 1.5rem;
  margin: 0 0 30px 0;
}
.profile-org a { color: #1e90ff; text-decoration: none; }
.profile-org a:hover { text-decoration: underline; }
.profile-org-alumni {
  font-size: 1.3rem;
  font-weight: 400;
  margin: 8px 0 20px 0;
  line-height: 1.4;
}
.profile-org-alumni a { color: #1e90ff; text-decoration: none; }
.profile-org-alumni a:hover { text-decoration: underline; }
.social-links-large {
  display: flex;
  justify-content: center;
  gap: 16px;
  margin-top: 24px;
  flex-wrap: wrap;
}
.social-links-large a {
  color: #1e90ff;
  transition: transform 0.2s, color 0.2s;
  text-decoration: none;
  display: inline-flex;
  align-items: center;
}
.social-links-large a svg {
  fill: #1e90ff;
}
.social-links-large a:hover {
  transform: scale(1.15);
  color: #0066cc;
}
.social-links-large a:hover svg {
  fill: #0066cc;
}
.profile-right { padding-top: 40px; }
.bio-section p {
  font-size: 2rem;
  line-height: 1.8;
  color: #333;
  margin-bottom: 16px;
}
.education-section { margin-top: 50px; }
.education-section h2 {
  font-size: 2rem;
  font-weight: 400;
  color: #333;
  margin: 0 0 30px 0;
}
.education-item {
  margin: 0 0 30px 0;
  display: flex;
  gap: 20px;
  align-items: flex-start;
}
.education-icon { font-size: 2rem; color: #333; margin-top: 5px; }
.education-content { flex: 1; }
.education-degree {
  font-weight: 600;
  color: #333;
  font-size: 1.7rem;
  margin-bottom: 4px;
}
.education-institution { color: #666; margin-bottom: 2px; }
@media (max-width: 900px) {
  .profile-layout { grid-template-columns: 1fr; gap: 40px; }
  .profile-right { padding-top: 0; }
}
</style>

<div class="profile-container">
<div class="profile-layout">

<div class="profile-left">
<img src="%s" alt="%s" class="profile-avatar">
%s
<div class="social-links-large">
%s
</div>
</div>

<div class="profile-right">
<div class="bio-section">
<p>%s</p>
</div>

<div class="education-section">
<h2>Education</h2>
%s
</div>
</div>

</div>
</div>
',
yaml_block,
avatar_file, name, name_and_org_html,
social_icons,
bio,
edu_html
  )
  
  # ── 6. Write .Rmd ──────────────────────────────────────────────────────────
  base     <- tools::file_path_sans_ext(basename(md_path))
  rmd_path <- file.path(out_dir, paste0(base, ".Rmd"))
  writeLines(rmd, rmd_path, useBytes = TRUE)
  message("Converted : ", basename(md_path), " -> ", basename(rmd_path))
  
  # FIX 4: render to .html in the same folder
  tryCatch({
    rmarkdown::render(rmd_path, output_format = "html_document",
                      output_dir = out_dir, quiet = TRUE)
    message("Rendered  : ", basename(rmd_path), " -> ",
            paste0(base, ".html"))
  }, error = function(e) {
    message("Render failed for ", basename(rmd_path), ": ", conditionMessage(e))
  })
  
  invisible(rmd_path)
}

# ── Run on all _index.md files found recursively ──────────────────────────────
md_files <- list.files(input_dir, pattern = "\\.md$",
                       full.names = TRUE, recursive = TRUE, all.files = TRUE)

if (length(md_files) == 0) {
  message("No .md files found under: ", normalizePath(input_dir))
} else {
  message("Found ", length(md_files), " .md file(s) to convert...\n")
  lapply(md_files, function(md_path) {
    convert_profile(md_path, out_dir = dirname(md_path))
  })
  message("\nAll done!")
}
