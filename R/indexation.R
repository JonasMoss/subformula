#' Takes to subformlas objects "subs1" and "subs2", and finds the indices
#' "indices" such that subs1[indices] = subs2. If such an indices vector
#' does not exist, it throws an error.
#'
#' @export
#' @param subs1 An object of class "subformula". Should contain subs2.
#' @param subs2 An object of class "subformula".
#'
#' @return An integer vector of indices satisfying subs1[indices] = subs2.

subformula_indices = function(subs1, subs2) {
  subs1 %>%
    as.character ->
    string1

  subs2 %>%
    as.character ->
    string2

 match(string2, string1)
}
