library(tidyverse)
library(RDCOMClient)
library(knitr)
library(base64enc)
library(magick)
library(kableExtra)
library(here)

# Source the assessment pipeline (salmon_code.R + text_updates.R)
project <- here()
source(here(project, 'source_code/text_updates.R'), echo = FALSE)

#########################################################
# BUILD WEEKLY SUMMARY TABLE FROM LOSS SUMMARY TABLE
#########################################################

# loss_summary_table rows: 1 = 7-day loss, 2 = avg daily, 3 = cumulative, 4 = % threshold
weekly_email_table <- loss_summary_table %>%
  select(
    `Data Item`          = data_item,
    `Natural WR`         = dna_winter_run_chinook,
    `Hatchery WR`        = lsnfh_hatchery_cwt_winter_run_chinook,
    `Natural Steelhead`  = natural_steelhead,
    `Hatchery Steelhead` = hatchery_steelhead,
    `Spring-run`         = any_of(c("spring_run_chinook", "spring_run"))
  )

n_cols <- ncol(weekly_email_table)

weekly_html <- knitr::kable(
  weekly_email_table,
  format     = "html",
  align      = rep("l", n_cols),
  table.attr = "style='border-collapse: collapse; margin: 0; padding: 0;
                line-height: 1.2; width: 700px; font-family: Arial; font-size: 10pt;'"
) |>
  kable_styling(
    full_width   = FALSE,
    position     = "left",
    font_size    = 12,
    stripe_color = "#f9f9f9"
  ) |>
  row_spec(0, bold = TRUE,
           extra_css = "border-top: 1px solid black; border-bottom: 1px solid black;
                        white-space: normal;") |>
  column_spec(
    1:n_cols,
    width     = "120px",
    extra_css = "border-left: none; border-right: none; white-space: normal;
                 word-wrap: break-word;"
  )

#########################################################
# DOWNLOAD AND EMBED LSP IMAGES FROM CBR
#########################################################

base_url <- paste0('https://www.cbr.washington.edu/sacramento/workgroups/include_gen/WY', wy, '/')

embed_cbr_image <- function(url) {
  tmp <- tempfile(fileext = ".png")
  tryCatch({
    img <- magick::image_read(url) %>% magick::image_scale("900x")
    magick::image_write(img, path = tmp, format = "png")
    base64enc::dataURI(file = tmp, mime = "image/png")
  }, error = function(e) {
    warning(paste("Could not download image:", url))
    NULL
  })
}

img_wr_lsp <- embed_cbr_image(paste0(base_url, 'samt_lsp_winter.png'))
img_sh_lsp <- embed_cbr_image(paste0(base_url, 'samt_lsp_stlhd.png'))

img_tag <- function(b64, alt, caption = "") {
  if (is.null(b64)) return(paste0("<p><em>", alt, " \u2013 image unavailable</em></p>"))
  paste0(
    "<figure style='margin:0 0 12px 0;'>",
    "<img src='", b64, "' alt='", alt, "' style='width:100%; max-width:800px;'/>",
    if (nchar(caption) > 0)
      paste0("<figcaption style='font-size:9pt; font-style:italic;'>", caption, "</figcaption>")
    else "",
    "</figure>"
  )
}

#########################################################
# PRE-COMPUTE THRESHOLD AND ITL VALUES FOR EMAIL
# Natural WR: Action 5 threshold (1% JPE) != ITL (0.56% / 0.36%) -> report both
# Hatchery WR: Action 5 threshold = ITL = 1% JPE                 -> report once
# Natural SH:  no Action 5 threshold; ITL only (5,294 / 2,319)   -> report ITL only
# Hatchery SH: Action 5 threshold = 1% JPE                       -> report once
# Spring-run:  Action 5 = 1% JPE per life stage (yearling, YOY); ITL = 0.5% per release group
#########################################################

wr_nat_threshold_val  <- prettyNum(round(jpe * wr_loss_threshold,     0), big.mark = ",")
wr_nat_itl_single_val <- prettyNum(round(jpe * itl_wr_natural_single, 0), big.mark = ",")
wr_nat_itl_3yr_val    <- prettyNum(round(jpe * itl_wr_natural_3yr,    0), big.mark = ",")

wr_hatch_threshold_val <- prettyNum(
  round(livingston_jpe * wr_hatch_loss_threshold, 0), big.mark = ","
)

#########################################################
# COMPOSE EMAIL BODY
#########################################################

email_body <- paste0(
  
  "<p style='font-family:Arial; font-size:10pt;'>Hi all,</p>",
  "<p style='font-family:Arial; font-size:10pt;'>",
  "Please see the DAT pre-FAWOG summary and assessment as of ",
  "<strong>", format(Sys.Date() - 1, "%B %d, %Y"), "</strong>. ",
  "Data are preliminary and subject to change. Full assessment available on ",
  "<a href='https://www.cbr.washington.edu/sacramento/workgroups/salmon_monitoring.html'>SacPAS</a>.</p>",
  
  # ---- RISK EVALUATION ----
  "<h3 style='font-family:Arial; font-size:11pt; margin-bottom:4px; margin-top:16px;'>",
  "Risk Evaluation</h3>",
  "<ol style='font-family:Arial; font-size:10pt;'>",
  "<li><strong>Natural and hatchery winter-run Chinook:</strong> ", risk_q1, "</li>",
  "<li><strong>Spring-run Chinook surrogates:</strong> ", risk_q2, "</li>",
  "<li><strong>Natural and hatchery steelhead:</strong> ", risk_q3, "</li>",
  "</ol>",
  
  # ---- STATUS OVERVIEW ----
  #"<h3 style='font-family:Arial; font-size:11pt; margin-bottom:4px;'>Status Overview</h3>",
  #"<ul style='font-family:Arial; font-size:10pt;'>",
  #"<li>", entrainment_status, "</li>",
  #"<li>", salvage_status, "</li>",
  #"<li>", itl_status, "</li>",
  #"<li>", sr_threshold_status, "</li>",
  #"<li>", sr_yearling_itl_summary, "</li>",
  #"<li>", wr_presence_status, "</li>",
  #"<li>", sh_presence_status, "</li>",
  #"</ul>",
  
  # ---- HATCHERY RELEASE NOTE (Livingston Stone only) ----
  #if(exists("wr_hatch_lsnfh") && nrow(wr_hatch_lsnfh) > 0) {
  #paste0(
  #"<p style='font-family:Arial; font-size:10pt;'>",
  #"<strong>Hatchery Winter-run Release (Livingston Stone NFH):</strong> ",
  #prettyNum(sum(wr_hatch_lsnfh$cwt_number_released, na.rm = TRUE), big.mark = ","),
  #" fish released (100% CWT-marked production fish). ",
  #"See <a href='https://www.cbr.washington.edu/sacramento/workgroups/include_gen/WY2026/hatch_winter.html'>SacPAS release table</a> for details.",
  #"</p>"
  #)
  #} else { "" },
  
  # ---- LOSS SUMMARY TABLE ----
  "<h3 style='font-family:Arial; font-size:11pt; margin-bottom:4px;'>Loss Summary</h3>",
  "<p style='font-family:Arial; font-size:9pt; margin-top:0;'>",
  "<strong>Action 5 Thresholds (1% of JPE):</strong> ",
  "Natural WR = ", prettyNum(round(jpe * wr_loss_threshold, 0), big.mark = ","), " fish; ",
  "Hatchery WR (Sac River) = ", wr_hatch_threshold_val, " fish; ",
  "Hatchery SH = ", prettyNum(sh_clipped_threshold, big.mark = ","), " fish; ",
  "SR surrogate yearlings = ", sr_yearling_threshold_fmt, " fish; ",
  "SR surrogate YOY = ", sr_yoy_threshold_fmt, " fish.<br>",
  "<strong>Incidental Take Limits (BiOp Table 184):</strong> ",
  "Natural WR = ", prettyNum(round(jpe * itl_wr_natural_single, 0), big.mark = ","), " fish (0.56% JPE single-yr); ",
  "Natural SH = ", prettyNum(itl_sh_natural_single, big.mark = ","), " fish (single-yr); ",
  "SR surrogates = 0.5% per release group.",
  "</p>",
  weekly_html,
  
  
  
  # ---- LSP FIGURES ----
  #"<h3 style='font-family:Arial; font-size:11pt; margin-bottom:4px;'>Loss Predictor Figures</h3>",
  #img_tag(img_wr_lsp, "Winter-run Loss Predictor",
  #"Figure 1. Estimates of winter-run Chinook loss generated by the Loss and Salvage Predictor tool."),
  #img_tag(img_sh_lsp, "Steelhead Loss Predictor",
  #"Figure 2. Estimates of steelhead loss generated by the Loss and Salvage Predictor tool."),
  
  "<p style='font-family:Arial; font-size:10pt;'>Best regards,<br>SaMT Team</p>"
)

#########################################################
# CREATE AND SAVE OUTLOOK DRAFT VIA RDCOMClient
#########################################################

outlook_app <- COMCreate("Outlook.Application")
email       <- outlook_app$CreateItem(0)  # 0 = olMailItem

email[["To"]] <- paste(
  "geasterbrook@usbr.gov",
  "jvogel@usbr.gov",
  "JAIsrael@usbr.gov",
  "RField@usbr.gov",
  "lejohnson@usbr.gov",
  "ashamilton@usbr.gov",
  "tyang@usbr.gov",
  "lmccormick@usbr.gov",
  "jfenolio@usbr.gov",
  sep = "; "
)

email[["Subject"]]  <- paste0("DAT pre-FAWOG summary and assessment \u2013 ", format(Sys.Date(), "%B %d, %Y"))
email[["HTMLBody"]] <- email_body
email$Save()

message("Draft saved to Outlook Drafts folder.")
