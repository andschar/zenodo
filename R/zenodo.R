#' Create API path.
#' 
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
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

#' Create a Zenodo Draft deposit.
#' 
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' 
#' @description Creates a new draft deposit to Zenodo. Files can be added with
#' the help of the zen_bucket() function. If you want to publish the
#' deposit, please do this manually on Zenodo. 
#' 
#' @returns An API response as a list.
#' 
#' @param title Title of deposit.
#' @param upload_type Type of deposit. Can be one of: 'publication', 'poster',
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
                      access_token = Sys.getenv('ZENODO_TOKEN')) {
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
  checkmate::assert_character(access_token)
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

#' Function to put a file into a Zenodo deposit.
#' 
#' @author Andreas Scharmueller \email{andschar@@protonmail.com}
#' 
#' @description Use this function to put a file into a Zenodo deposit.
#' 
#' @returns An API response as a list.
#' 
#' @param id An id of a Zenodo deposit.
#' @param file_name Name of the file which it should have on Zenodo.
#' @param file_disk A file on disk to deposit.
#' @param access_token An individual access_token.
#' 
#' @export
#'
zen_put = function(id,
                   file_name,
                   file_disk,
                   access_token = Sys.getenv('ZENODO_TOKEN')) {
  # checks
  checkmate::assert_numeric(id)
  checkmate::assert_character(file_name)
  checkmate::assert_file(file_disk)
  checkmate::assert_character(access_token)
  # GET info about deposit
  qurl1 = paste0(file.path(apipath('deposit'), id),  '?access_token=', access_token)
  req1 = httr::GET(qurl1)
  httr::warn_for_status(req1)
  cont1 = httr::content(req1)
  # PUT files to deposit
  bucket = cont1$links$bucket
  qurl2 = paste0(file.path(bucket, file_name), '?access_token=', access_token)
  req2 = httr::PUT(qurl2,
                   body = httr::upload_file(file_disk))
  httr::warn_for_status(req2)
  cont2 = httr::content(req2)
  
  cont2
}
