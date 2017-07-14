# foo <- function(x, ...) {
#   UseMethod("foo")
# }
#
# foo.character <- function(x, ..., truncate = 10) {
#   substr(x, 1, truncate)
# }
#
# foo.numeric <- function(x, ..., add = 2) {
#   x + add
# }
#
# # Silently returns wrong result because misspelled argument na
# # swallowed by ...
# foo("This is a long string", truncate = 15)
# #> [1] "This is a "
#
# foo("2.0", truncate = 2)
