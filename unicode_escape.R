### Esta función la tomé verbatim del usuario de stackoverflow Xin Yin
### http://stackoverflow.com/questions/25308913/automatically-escape-unicode-characters

unicode_escape <- function(x, endian="big") {
    if (Encoding(x) != 'UTF-8') {
        x <- enc2utf8(enc2native(x))
    }
    to.enc <- ifelse(endian == 'big', 'UTF-32BE', 'UTF-32LE')

    bytes <- strtoi(unlist(iconv(x, "UTF-8", "UTF-32BE", toRaw=T)), base=16)
    # there may be some better way to do thibs.
    runes <- matrix(bytes, nrow=4)
    escaped <- apply(runes, 2, function(rb) {
        nonzero.bytes <- rb[rb > 0]
        ifelse(length(nonzero.bytes) > 1,
               # convert back to hex
               paste("\\u", paste(as.hexmode(nonzero.bytes), collapse=""), sep=""),
               rawToChar(as.raw(nonzero.bytes))
        )
    })
    paste(escaped, collapse="")
}