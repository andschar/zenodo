#' Create API path
#' 
#' @noRd
#' 
apipath = function(endpoint = NULL) {
  baseurl = 'https://zenodo.org/api'
  checkmate::assert_choice(
    endpoint,
    choices = c('deposit')
  )
  if (endpoint == 'deposit') {
    endpoint_real = 'deposit/depositions'
  }
  
  file.path(baseurl, endpoint_real)
}

#' Create a Zenodo Draft Upload.
#' 
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' 
#' @description Creates a new draft upload to Zenodo. Files can be added with
#' the help of the zen_bucket() function. If you want to publish the
#' upload, please do this manually on Zenodo. 
#' 
#' @returns An API response as a list.
#' 
#' @param title Title of upload.
#' @param upload_type Type of upload. Can be one of: 'publication', 'poster',
#' 'presentation', 'dataset', 'image', 'video', 'software', 'lesson', 'physicalobject',
#' 'other'.
#' @param publication_type Not yet documented.
#' @param publication_date Date of publication. Submit a object of class Date, IDate
#' or a date string fromated as 'YYYY-MM-DD'.
#' @param author A list of lists containing name, affiliation and ORCID of the author.
#' Multiple entries are possible.
#' @param description Description of the data as a string.
#' @param version A version string. Semantically-versioned tag is recommended:
#' \url{https://semver.org/}.
#' @param language A lower-cased 3-letter ISO 639-3 string (e.g. 'ger').
#' @param keywords Keywords as a vector or string.
#' @param notes Additional notes as a string.
#' @param access_right Has to be one of 'open', 'embargoed', 'restricted', 'closed'.
#' @param access_right_addition Only needs to be specified if 'embargoed' or
#' 'restricted' is selected in access_right. For 'embargoed' provide a date string
#' ('YYYY-MM-DD'). For 'restricted' provide a text string explaining the conditions.
#' @param access_token An individual access_token.
#'
#' @export
#' 
zen_upload = function(title = "My Title",
                      upload_type = "dataset",
                      publication_type = NULL,
                      publication_date = NULL,
                      author = NULL,
                      description = NULL,
                      version = NULL,
                      language = NULL,
                      keywords = NULL,
                      notes = NULL,
                      access_right = 'open',
                      access_right_addition = NULL,
                      access_token = getOption('zenodo_token')) {
  # checks
  acces_token = checkmate::assert_character(access_token)
  upload_type = checkmate::assert_choice(
    upload_type,
    choices = c('publication', 'poster', 'presentation',
                'dataset', 'image', 'video', 'software',
                'lesson', 'physicalobject', 'other'),
    null.ok = TRUE
  )
  checkmate::assert_true(length(upload_type) == 1)
  access_right = checkmate::assert_choice(
    access_right,
    choices = c('open', 'embargoed', 'restricted', 'closed'),
    null.ok = FALSE
  )
  # preparation
  creators = lapply(author, as.list)
  # list
  metadata = list(
    title = title,
    upload_type = upload_type,
    publication_date = publication_date,
    creators = creators,
    description = description,
    version = version,
    language = language,
    keywords = keywords,
    notes = notes
  )
  metadata = metadata[ !sapply(metadata, is.null) ] # NOTE this is necessary b/c the API does not accept null or {}.
  l = list(
    metadata = metadata
  )
  # JSON
  body = jsonlite::toJSON(l, auto_unbox = TRUE)
  # API request
  qurl = paste0(apipath('deposit'), '?access_token=', acces_token)
  req = httr::POST(qurl,
                   body = body,
                   encode = 'raw',
                   httr::content_type(type = 'application/json'))
  httr::warn_for_status(req)
  cont = httr::content(req)

  cont
}

#' Function to put a file into a Zenodo Upload.
#' 
#' @description Use this function to put a file into a Zenodo upload.
#' 
#' @returns An API response as a list.
#' 
#' @param id An id of a Zenodo Upload.
#' @param file_name Name of the file which it should have on Zenodo.
#' @param file_disk A file on disk tp upload.
#' @param access_token An individual access_token.
#' 
#' @export
#'
zen_put = function(id,
                   file_name,
                   file_disk,
                   access_token = getOption('zenodo_token')) {
  # checks
  checkmate::assert_numeric(id)
  checkmate::assert_character(file_name)
  checkmate::assert_file(file_disk)
  # API GET request
  qurl1 = paste0(file.path(apipath('deposit'), id),  '?access_token=', access_token)
  req1 = httr::GET(qurl1)
  httr::warn_for_status(req1)
  cont1 = httr::content(req1)
  # API PUT request
  bucket = cont1$links$bucket
  qurl2 = paste0(file.path(bucket, file_name), '?access_token=', access_token)
  # API request
  req2 = httr::PUT(qurl2,
                   body = httr::upload_file(file_disk),
                   httr::verbose())
  httr::warn_for_status(req2)
  cont2 = httr::content(req2)
  
  cont2
}
