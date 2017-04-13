#' Encode Factor-like Levels and Labels as a Simple String
#' 
#' For compact storage, \code{encode} combines a set of levels and labels
#' (codes and decodes) into a simple string.  The default method converts its
#' argument to character.  The list method operates element-wise, expecting and
#' equal number of label elements, each of which have the same length as the
#' corresponding element of x.
#' 
#' An 'encoding' must be at least 5 characters long, beginning and ending with
#' two instances of \code{sep}. Specified levels are likewise separated by
#' double separators. If a label (decode) is available for a level, it follows
#' the corresponding level: the two are separated by a single instance of
#' \code{sep}.  Separators may be mixed within an encoded vector, but not
#' within an element.  \code{--0-male--1-female--} indicates that the value 0
#' represents the concept "male" and the value 1 represents "female".
#' 
#' If you don't supply the separator, \code{encode} uses the first of these that is not otherwise present in the result: /|:\\~!@#$%^&*?. (including dot).
#' 
#' @param x object
#' @param ... passed arguments
#' @seealso \code{\link{encode.character}} \code{\link{encode.default}} \code{\link{encode.list}} \code{\link{codes}} \code{\link{decodes}} \code{\link{decode}} \code{\link{encoded}}
#' @export
#' @examples
#' 
#' a <- encode(
#'   x = list(
#'     c('M','F'),
#'     c(1:4)
#'   ),
#'   labels = list(
#'     c('male','female'),
#'     c('caucasian','asian','african',NA)
#'   )
#' )
#' b <- encode(c(1:2),c('pediatric','adult'))
#' a
#' b
#' c <- c('a',NA,'##b##')
#' encoded(a)
#' encoded(b)
#' encoded(c)
#' encoded(' //4// ')
#' codes(a)
#' codes(b)
#' codes(b,simplify=FALSE)
#' codes(c)
#' codes('..1..')
#' decodes(a)
#' decodes(b)
#' decodes(c)
#' decode(1:4,'//1/a//2/b//3/c//')
encode <- function(x,...)UseMethod('encode')

#' Encode a List
#' 
#' Encodes a list.
#' @inheritParams encode
#' @param labels same length as x if supplied
#' @return list
#' @export
encode.list <- function(x,labels=NULL,...){
  if(!is.null(labels) & length(labels) != length(x))stop('lengths of x and labels must match')
  sapply(seq_along(x),function(i)encode(as.character(x[[i]]),labels=labels[[i]],...))
}
is.defined <- function(x)!is.na(x)

#' Encode Character.
#' 
#' Encodes character.
#' @inheritParams encode
#' @param labels same length as x if supplied
#' @param sep a single character not present in x or labels
#' @return character
#' @export
encode.character <- function(x,labels=NULL,sep=NULL,...){
  if(!is.null(labels) & length(labels) != length(x))stop('lengths of x and labels must match')
  if(is.null(sep))sep <- defaultSep(c(x,labels))
  if(!is.null(labels))if(any(is.defined(labels)))x[is.defined(labels)] <- paste(x,labels,sep=sep)[is.defined(labels)]
  doublesep <- paste0(sep,sep)
  x <- paste(x,collapse=doublesep)
  x <- paste0(doublesep,x,doublesep)
  x
} 

#' Encode Default.
#' 
#' Encodes using default method:  coerces to character and and encodes the result.
#' @inheritParams encode
#' @param labels same length as x if supplied
#' @return character
#' @export
encode.default <- function(x,labels=NULL,...)encode(as.character(x),labels=labels,...)
.encoded <- function(x,...){
  stopifnot(length(x) == 1)
  x <- as.character(x)
  x <- sub('^\\s','',x)
  x <- sub('\\s$','',x)
  if(is.na(x))return(FALSE)
  if (nchar(x) < 5 )return(FALSE)
  first <- substr(x,1,1)
  second <- substr(x,2,2)
  end <- nchar(x)
  ultimate <- substr(x,end,end)
  penult <- substr(x, end - 1,end - 1)
  if(
    second != first ||
      ultimate != first ||
      penult != first
  )return(FALSE)
  return(TRUE)
}

#' Check If Object is Encoded
#' 
#' Checks if object is encoded. 
#' 
#' @param x object
#' @param ... passed arguments
#' @export
#' @seealso \code{\link{encoded.default}}
encoded <- function(x, ...)UseMethod('encoded')

#' Check If Default Object is Encoded
#' 
#' Checks if object is encoded, using default methodology. Always returns TRUE or FALSE, telling whether the corresponding element represents an encoding of levels and labels.
#' @inheritParams encoded
#' @export
encoded.default <- function(x, ...)sapply(x,.encoded,USE.NAMES=FALSE)
{
# extract <- function(x, pattern, group = 0, invert=FALSE,...){
#   y <- regexec(pattern,x)
#   scale <- sapply(y,length)
#   group <- rep(group, length.out= length(y))
#   group <- group + 1
#   start <- sapply(seq_along(y), function(i){
#     y <- y[[i]]
#     group <- group[[i]]
#     if(group > length(y)) return(0)
#     y[[group]]
#   })
#   start[is.na(start)] <- 0
#   start[start < 0] <- 0
#   len <- sapply(seq_along(y), function(i){
#     y <- y[[i]]
#     group <- group[[i]]
#     if(group > length(y)) return(0)
#     attr(y,'match.length')[[group]]
#   })
#   len[is.na(len)] <- 0
#   len[len < 0] <- 0
#   stop <- start + len - 1
#   stop[stop < 0] <- 0
#   indices <- lapply(seq_along(y),function(i)start[[i]]:stop[[i]])
#   indices[len == 0] <- 0
#   splits <- strsplit(x,NULL)
#   welds <- sapply(seq_along(splits), function(i){
#     z <- splits[[i]]
#     index <- indices[[i]]
#     if(invert){
#       if(len[i] > 0) z <- z[-index]
#     } else z <- z[index]
#     z <- paste(z,collapse='')
#     z
#    })
#   welds[is.na(x)] <- NA
#   welds
# }
}
.extract <- function(x,node,...){
  stopifnot(length(x) == 1)
  x <- as.character(x)
  if(!encoded(x)) return(as.character(NA))
  sep <- substr(x,1,1)
  doublesep <- paste0(sep,sep)
  y <- strsplit(x,split=doublesep,fixed=TRUE)[[1]]
  y <- y[y!='']
  lift <- function(x,node){
    z <- strsplit(x,split=sep,fixed=TRUE)[[1]]
    if(length(z) >= node) (z[[node]]) 
    else as.character(NA)
  }
  y <- sapply(y,lift,node=node)
  y <- as.character(y)
  y
} 
.codes <- function(x,...).extract(x,node=1,...)
.decodes <- function(x,...).extract(x,node=2,...)

#' Extract Codes from an Object
#' 
#' Extracts Codes from an object. Default method is supplied.
#' @param x object
#' @param ... passed arguments
#' @seealso \code{\link{codes.default}}
#' @export
codes <- function(x,...)UseMethod('codes')

#' Extract Codes by Default from an Object
#' 
#' Extracts codes from an object using the default method.
#'
#' @inheritParams codes
#' @param simplify whether to convert length one list to vector
#' @return list, or vector if simplify = TRUE
#' @export
codes.default <- function(x, simplify = TRUE, ...){
  y <- lapply(x,.codes)
  if(length(y) == 1 & simplify) y <- y[[1]]
  y
}
#' Extract Decodes from an Object
#' 
#' Extracts decodes from an object. Default method is supplied.
#' @param x object
#' @param ... passed arguments
#' @seealso \code{\link{decodes.default}}
#' @export
decodes <- function(x,...)UseMethod('decodes')
#' Extract Decodes by Default from an Object
#' 
#' Extracts decodes from an object using the default method.
#'
#' @inheritParams decodes
#' @param simplify whether to convert length one list to vector
#' @return list, or vector if simplify = TRUE
#' @export
decodes.default <- function(x, simplify = TRUE, ...){
  y <- lapply(x,.decodes)
  if(length(y) == 1 & simplify) y <- y[[1]]
  y
}


map <- function (x, from, to, strict = TRUE, ...) 
{
  stopifnot(length(to) == length(from))
  res <- to[match(x, table = from)]
  if (!strict) 
    res[!(x %in% from)] <- x[!(x %in% from)]
  res
}

#' Decode an Object
#' 
#' Decodes an object.  Default method supplied.
#' @param x object
#' @param ... passed arguments
#' @seealso \code{\link{decode.default}}
#' @export
decode <- function(x,...)UseMethod('decode')

#' Decode an Object by Default
#' 
#' Decodes an object using the default method. Typically \code{x} is a character vector containing codes that can be extracted from \code{encoding}. Corresponding decodes are returned as a factor. 
#' 
#' @inheritParams decode
#' @param encoding length one character that is itself encoded
#' @return factor
#' @export
decode.default <- function(x,encoding='',...){
  if(!encoded(encoding))encoding = encode(unique(x),unique(x),sep=defaultSep(unique(x)))
  stopifnot(encoded(encoding))
  stopifnot(length(encoding) == 1)
  codes <- codes(encoding)
  decodes <- decodes(encoding)
  y <- map(x,from=codes,to=decodes)
  y <- factor(y,levels=decodes)
  y
}

defaultSep <- function(x,...){
  x <- as.character(x)
  sep <- character(0)
  candidates <- "/|:\\~!@#$%^&*?."
  candidates <- strsplit(candidates,'')[[1]]
  for(i in candidates){
    sep <- i
    if(!any(grepl(sep,x,fixed=TRUE)))break
  }
  if(!length(sep)) stop('x contains all the default candidates for sep')
  sep
}
