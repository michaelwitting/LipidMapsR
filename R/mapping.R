#' @title SwissLipids API Mapping
#'
#' @description
#'
#' `lipidMapsMapping` uses the SwissLipids API to perform mapping between
#'     different database identifier
#'
#' @param context `character`
#' @param input_item `character`
#' @param input_value `character`
#' @param output_item `character`
#'
#' @return `data.frame`
#'
#' @author Michael Witting
#'
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#'
#' @export
#'
#' @examples
#'
#' lipidMapsMapping(context = "compound",
#' input_item = "inchi_key",
#' input_value = "WTJKGGKOPKCXLL-VYOBOKEXSA-N",
#' output_item = "all")
lipidMapsMapping <- function(context = c("compound", "gene", "protein"),
                             input_item,
                             input_value,
                             output_item) {

  # check input
  match.arg(context)

  # dependent on the context, input_item can have different values
  if(context == "compound") {

    match.arg(input_item, c("lm_id", "formula", "inchi_key", "pubchem_cid",
                            "hmdb_id", "kegg_id", "chebi_id", "smiles",
                            "abbrev", "abbrev_chain"))

  } else if(context == "gene") {

    match.arg(input_item, c())

  } else if(context == "protein") {

    match.arg(input_item, c())

  }

  # dependent on the context, output_item can have different values
  if(context == "compound") {

    match.arg(output_item, c("all", "classification", "lm_id", "name",
                             "sys_name", "synonyms", "core", "main_class",
                             "sub_class", "class_level4", "exactmass",
                             "formula", "inchi", "inchi_key", "kegg_id",
                             "hmdb_id", "chebi_id", "lipidbank_id",
                             "pubchem_cid", "smiles", "molfile", "structure",
                             "physchem"))

  } else if(context == "gene") {

    match.arg(output_item, c())

  } else if(context == "protein") {

    match.arg(output_item, c())

  }

  lapply(input_value, .query,
         context = context,
         input_item = input_item,
         output_item = output_item)


}

.query <- function(context, input_item, input_value, output_item) {

  # create query url
  query_url <- paste0(BASE_URL,
                      "/", context,
                      "/", input_item,
                      "/", input_value,
                      "/", output_item)

  # perform request
  r <- curl::curl_fetch_memory(URLencode(query_url))

  # dependent on status code return results
  if(r$status_code == as.integer(200)) {

    return(jsonlite::fromJSON(rawToChar(r$content), flatten = TRUE))

  } else {

    return(NA_character_)

  }

}
