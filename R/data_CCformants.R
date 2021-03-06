#' Corncrake, \emph{Crex crex} - formants
#'
#'\itemize{
#'  \item \strong{Species:} Corncrake, \emph{Crex crex}
#'  \item \strong{Number of individuals:} 33
#'  \item \strong{Number of calls per individual:} 10
#'  \item \strong{Number of acoustic variables:} 4
#'  \item \strong{Individual identity:} HS=10.51
#'  \item \strong{Reference:} Budka, M., & Osiejuk, T. S. (2013). Formant
#'  Frequencies are Acoustic Cues to Caller Discrimination and are a Weak
#'  Indicator of the Body Size of Corncrake Males. Ethology, 119, 960-969.
#'  doi:10.1111/eth.12141
#'}
#' Corncrake calls were recorded at three sites in Poland and one in the Czech
#' Republic Recordings were made during the corncrake breeding season, from 8 to
#' 30 July, in 2011 and in 2012. Males were recorded when calling spontaneously,
#' in favourable conditions, at night (from 22.00 to 03.30, local time) from a
#' distance of ca. 5-10 m. The original dataset comprised 104 males with 10
#' calls measured from each male.\cr\cr
#' Formants (second to fifth) were measured within the first syllable of the
#' call. Formants were extracted by PRAAT.
#'
#' @format A data frame with 330 rows and 5 variables:
#'
#' \describe{
#'   \item{id}{factor, identity code of an individual emitting the call}
#'   \item{s1f2, s1f3, s1f4, s1f5}{formants 2 to 5, respectively, measured within the first syllable of the call, in Hertz}
#' }
#'
#' @source \href{https://onlinelibrary.wiley.com/doi/abs/10.1111/eth.12141}{Budka, M., & Osiejuk, T. S. (2013). Formant Frequencies are Acoustic Cues to Caller Discrimination and are a Weak Indicator of the Body Size of Corncrake Males. Ethology, 119, 960-969. doi:10.1111/eth.12141}
"CCformants"
