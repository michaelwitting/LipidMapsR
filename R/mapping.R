#' @title SwissLipids API Mapping
#'
#' @description
#'
#' `lipidMapsMapping` uses the SwissLipids API to perform mapping between
#'     different database identifier
#'
#' @param context `character`
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
lipidMapsMapping <- function(context = c("compound", "gene", "protein"),
                             input_item,
                             input_value,
                             output_item) {

  # check input
  match.arg(context)

  # dependen on the context, input_item can have different values
  if(context == "compound") {

    match.arg(input_item, c("lm_id", "formula", "inchi_key", "pubchem_cid",
                            "hmdb_id", "kegg_id", "chebi_id", "smiles",
                            "abbrev", "abbrev_chain"))

  } else if(context == "gene") {

    match.arg(input_item, c())

  } else if(context == "protein") {

    match.arg(input_item, c())

  }


  query_url <- paste0(BASE_URL, "mapping?from=", from, "&to=", to, "&ids=", paste(ids, collapse = ","))

  jsonlite::fromJSON(utils::URLencode(query_url))

}
