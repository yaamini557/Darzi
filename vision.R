# vision.R : read the STYLE of a garment from a reference photo (not its measurements).
# A photo has no scale, so exact sizes always come from the body measurements in drafting.R.
# Needs: install.packages(c("httr2", "jsonlite", "base64enc")) and an API key:
#   Sys.setenv(ANTHROPIC_API_KEY = "your-key")   (or put it in ~/.Renviron)
# Privacy: the photo is sent to the Anthropic API. Do not upload customer photos without consent.

vision_model <- "claude-sonnet-5-5"   # change if you prefer another model

detect_style <- function(path, api_key = Sys.getenv("ANTHROPIC_API_KEY")) {
  if (!nzchar(api_key)) stop("No ANTHROPIC_API_KEY set. Choose the style manually instead.")

  ext <- tolower(tools::file_ext(path))
  media_type <- switch(ext, png = "image/png", webp = "image/webp", "image/jpeg")

  prompt <- paste(
    "You are assisting a tailor. Look at the main garment in the photo and reply with ONE JSON object",
    "and nothing else, using only these values:",
    '{"garment":"Kurti|Anarkali|Dress|Lehenga|Skirt|Blouse|Shirt|Custom piece list",',
    '"fit":"Fitted|Regular|Loose", "flare":"Straight|A-line|Flared",',
    '"neck":"Round|V|Boat", "sleeve":"Sleeveless|Short|Elbow|ThreeQuarter|Full",',
    '"length":"Hip|Knee|Calf|Ankle", "skirt_type":"Gathered|Circular",',
    '"shaping":"None|Darts|Princess seams",',
    '"bottom":"None|Straight Pant|Cigarette Pant|Bootcut|Flared Pant|Wide-leg Trouser|Culottes|Shorts|Jogger|Leggings|Palazzo|Salwar|Churidar|Patiala|Dhoti Pant|Sharara",',
    '"has_dupatta":true|false}.',
    'Use "Custom piece list" if the garment is none of the listed types.',
    "If unsure, pick the most likely option."
  )

  resp <- httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers(`x-api-key` = api_key, `anthropic-version` = "2023-06-01") |>
    httr2::req_body_json(list(
      model = vision_model, max_tokens = 300,
      messages = list(list(role = "user", content = list(
        list(type = "image", source = list(type = "base64", media_type = media_type,
                                           data = base64enc::base64encode(path))),
        list(type = "text", text = prompt)
      )))
    )) |>
    httr2::req_perform()

  txt <- httr2::resp_body_json(resp)$content[[1]]$text
  jsonlite::fromJSON(gsub("```json|```", "", txt))
}
