## pkgdown/favicon/ from man/figures/logo.png, made here rather than by
## pkgdown::build_favicons(), which uploads the logo to realfavicongenerator.net.
## The same files illume's site has, from the same kind of source: square
## icons with the hex fitted to their height on a transparent background,
## a multi-size favicon.ico, an SVG holding a small copy of the logo, and a
## web manifest whose icon paths are relative, so they resolve on a project
## site served under /illumex/.
##
## Keep the results committed: pkgdown::init_site() runs build_favicons()
## by itself whenever there is a logo and no pkgdown/favicon/favicon.ico,
## and that would call the service on every CI build.
##
## Run from the package root, after dev/logo/make_logo.R:
##   Rscript dev/logo/make_favicons.R
## Needs magick and jsonlite; neither is a dependency of the package.

logo <- magick::image_read("man/figures/logo.png")
dir <- "pkgdown/favicon"
dir.create(dir, showWarnings = FALSE, recursive = TRUE)

## the hex fitted inside an n x n square, centred, the rest transparent
square <- function(n) {
  fit <- magick::image_resize(logo, sprintf("%dx%d", n, n), filter = "Lanczos")
  magick::image_extent(magick::image_background(fit, "none"), sprintf("%dx%d", n, n),
                       gravity = "center", color = "none")
}
png_out <- function(img, file) magick::image_write(img, file.path(dir, file), format = "png")

png_out(square(96), "favicon-96x96.png")
png_out(square(180), "apple-touch-icon.png")
png_out(square(192), "web-app-manifest-192x192.png")
png_out(square(512), "web-app-manifest-512x512.png")
magick::image_write(c(square(16), square(32), square(48)),
                    file.path(dir, "favicon.ico"), format = "ico")

## The SVG is the logo's shape holding a 96 px wide copy of it: an icon is
## drawn at 16 to 32 px, so embedding the full 480 px logo would only make
## every page load heavier.
info <- magick::image_info(logo)
small <- tempfile(fileext = ".png")
magick::image_write(magick::image_resize(logo, "96x", filter = "Lanczos"), small,
                    format = "png")
b64 <- jsonlite::base64_enc(readBin(small, "raw", file.size(small)))
writeLines(sprintf(paste0(
  '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" ',
  'version="1.1" width="%d" height="%d" viewBox="0 0 %d %d">',
  '<image width="%d" height="%d" xlink:href="data:image/png;base64,%s"></image></svg>'),
  info$width, info$height, info$width, info$height, info$width, info$height,
  gsub("\n", "", b64, fixed = TRUE)), file.path(dir, "favicon.svg"), sep = "")

writeLines(jsonlite::toJSON(list(
  name = "illumex", short_name = "illumex",
  icons = list(
    list(src = "web-app-manifest-192x192.png", sizes = "192x192",
         type = "image/png", purpose = "maskable"),
    list(src = "web-app-manifest-512x512.png", sizes = "512x512",
         type = "image/png", purpose = "maskable")),
  theme_color = "#ffffff", background_color = "#ffffff", display = "standalone"),
  auto_unbox = TRUE, pretty = TRUE), file.path(dir, "site.webmanifest"))

for (f in list.files(dir))
  cat(sprintf("%-30s %6.1f KB\n", f, file.size(file.path(dir, f)) / 1024))
