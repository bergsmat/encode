#' Encode Factor-like Levels and Labels as a Simple String
#' 
#' For compact storage, \code{encode} combines a set of levels and labels
#' (codes and decodes) into a simple string.  The default method converts its
#' argument to character.  The list method operates element-wise, expecting an
#' equal number of label elements, each of which have the same length as the
#' corresponding element of x.
#' 
#' An empty 'encoding' consists of four identical characters, e.g. \code{////}.
#' 
#' A non-empty encoding must be at least 5 characters long, beginning and 
#' ending with two instances of \code{sep} e.g. \code{//1//}. Levels are 
#' likewise separated from each other by double separators, e.g. \code{//1//2//}. 
#' 
#' If a label (decode) is available for a level, it follows the corresponding level: 
#' the two are separated by a single instance of \code{sep}, e.g. \code{//1/a//2/b//}.
#' 
#' Encodings may be combined as elements of a character vector, i.e. and encoded vector.
#' Choice of separator may vary among elements, but must be consistent within elements.
#' 
#' Labels (decodes) may be zero-length, but not levels (codes), e.g. \code{//1///} 
#' is valid but \code{///a//} is not. A zero-length decode is extracted as an empty string.
#' 
#' Duplicate levels (codes) result in a warning for encode(), and are otherwise silently ignored.  Duplicate labels (decodes) result in case-collapsing.
#' 
#' 
#' @param x object
#' @param ... passed arguments
#' @seealso \code{\link{encode.character}} \code{\link{encode.default}} \code{\link{encode.list}} \code{\link{codes}} \code{\link{decodes}} \code{\link{decode}} \code{\link{encoded}}
#' @export
#' @family encode
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
#' decode(1:4,'//1/a//1/b//3/c//') # duplicate code: ignored
#' decode(1:4,'//1/a//2/a//3/c//') # duplicate decode: collapsed
#' # encode(c(1,1,2,3),c('a','b','c','d')) Warning: duplicate codes
#' 
#' \dontshow{
#' stopifnot(encoded('////'))
#' stopifnot(encoded('//a///'))
#' stopifnot(!encoded('///a//')) 
#' stopifnot(!encoded('//a/a//b/b///')) 
#' stopifnot(identical(decode(1:4),factor(1:4)))
#' }
encode <- function(x,...)UseMethod('encode')

#' Encode a List
#' 
#' Encodes a list.
#' @inheritParams encode
#' @param labels same length as x if supplied
#' @return list
#' @family encode
#' @export
encode.list <- function(x,labels=NULL,...){
  if(!is.null(labels) & length(labels) != length(x))stop('lengths of x and labels must match')
  sapply(seq_along(x),function(i)encode(as.character(x[[i]]),labels=labels[[i]],...))
}
is.defined <- function(x)!is.na(x)

#' Encode Character.
#' 
#' Encodes character.  If \code{sep} is NULL, it is replaced with the first of these that is not otherwise present in the result: /|:\\~!@#$%^&*?. (including dot).

#' @inheritParams encode
#' @param labels same length as x if supplied
#' @param sep a single character not present in x or labels
#' @return character
#' @family encode
#' @export
encode.character <- function(x, labels = NULL, sep = NULL, ...){
  if(!is.null(labels) & length(labels) != length(x))stop('lengths of x and labels must match')
  if(any(is.na(x)))stop('elements of x cannot be NA')
  if(any(x == '')) stop('elements of x cannot be empty strings')
  dups <- x[duplicated(x)]
  if(length(dups)){
    warning('duplicate codes, e.g., ', dups[[1]])
  }
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
#' @family encode
#' @export
encode.default <- function(x,labels=NULL,...)encode(as.character(x),labels=labels,...)

.encoded <- function(x,...){
  if(length(x) == 0) return(FALSE)
  stopifnot(length(x) == 1)
  if(grepl('EMPTYSTRING',x)) stop('EMPTYSTRING is reserved')
  x <- as.character(x)
  x <- sub('^\\s','',x)
  x <- sub('\\s$','',x)
  if(is.na(x))return(FALSE)
  if (nchar(x) < 4 )return(FALSE)
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
  sep <- first
  doublesep <- paste0(sep,sep)
  triplesep <- paste0(sep,sep,sep)
  triplesub <- paste0(sep,'EMPTYSTRING',doublesep)
  y <- sub(doublesep,'',x, fixed = TRUE) # strip leader
  if(y == doublesep) return(TRUE)
  y <- gsub(triplesep,triplesub,y, fixed = TRUE) # trap empty label
  y <- strsplit(y, doublesep, fixed = TRUE)[[1]] # vector of levels
  y <- strsplit(y, sep, fixed = TRUE) # list of label / level
  z <- sapply(y,length)
  if(any(z > 2))return(FALSE)
  y <- sapply(y,`[[`,1) # label only
  if(any(y == '')) return(FALSE) # label must have length
  return(TRUE)
}

#' Check If Object is Encoded
#' 
#' Checks if object is encoded. 
#' 
#' @param x object
#' @param ... passed arguments
#' @export
#' @family encoded
#' @seealso \code{\link{encoded.default}}
encoded <- function(x, ...)UseMethod('encoded')

#' Check If Default Object is Encoded
#' 
#' Checks if object is encoded, using default methodology. Always returns logical, telling whether the corresponding element represents an encoding of levels and labels. Objects with zero length give \code{FALSE}.
#' @inheritParams encoded
#' @export
#' @family encoded
#' @return logical
encoded.default <- function(x, ...){
  if(length(x) == 0) return(FALSE)
  sapply(x,.encoded,USE.NAMES=FALSE)
}
.extract <- function(x,node,...){
  stopifnot(length(x) == 1)
  if(grepl('EMPTYSTRING',x))stop('EMPTYSTRING is reserved')
  x <- as.character(x)
  if(!encoded(x)) return(as.character(NA))
  sep <- substr(x,1,1)
  doublesep <- paste0(sep,sep)
  triplesep <- paste0(sep,sep,sep)
  triplesub <- paste0(sep,'EMPTYSTRING',sep,sep)
  y <- sub(doublesep,'',x, fixed = TRUE) # strip leader
  #if(y == doublesep) return(TRUE)
  y <- gsub(triplesep,triplesub,y,fixed = TRUE)
  y <- strsplit(y,split=doublesep,fixed=TRUE)[[1]]
  y <- y[y!='']
  lift <- function(x,node){
    z <- strsplit(x,split=sep,fixed=TRUE)[[1]]
    if(length(z) >= node) (z[[node]]) 
    else as.character(NA)
  }
  y <- sapply(y,lift,node=node)
  y <- as.character(y)
  y[y == 'EMPTYSTRING'] <- ''
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
#' @family codes
#' @export
codes <- function(x,...)UseMethod('codes')

#' Extract Codes by Default from an Object
#' 
#' Extracts codes from an object using the default method.
#'
#' @inheritParams codes
#' @param simplify whether to convert length one list to vector
#' @return list, or vector if simplify = TRUE
#' @family codes
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
#' @family decodes
#' @export
decodes <- function(x,...)UseMethod('decodes')
#' Extract Decodes by Default from an Object
#' 
#' Extracts decodes from an object using the default method.
#'
#' @inheritParams decodes
#' @param simplify whether to convert length one list to vector
#' @return list, or vector if simplify = TRUE
#' @family decodes
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
#' @family decode
decode <- function(x,...)UseMethod('decode')

#' Decode an Object by Default
#' 
#' Decodes an object using the default method. Typically \code{x} is a character vector containing codes that can be extracted from \code{encoding}. Corresponding decodes are returned as a factor with levels of unique decodes. If \code{encoding} is NULL, it is replaced with an encoding such that levels and labels are both \code{unique(x)}. Duplicate codes are ignored.  Duplicate decodes are collapsed (combined to a single level).
#' 
#' @inheritParams decode
#' @param encoding length one character that is itself encoded
#' @return factor
#' @export
#' @family decode
decode.default <- function(x,encoding = NULL, ...){
  if(is.null(encoding)) encoding <- encode(unique(x),unique(x),sep=defaultSep(unique(x)))
  stopifnot(length(encoding) == 1)
  stopifnot(encoded(encoding))
  codes <- codes(encoding)
  decodes <- decodes(encoding)
  y <- map(x,from=codes,to=decodes)
  y <- factor(y,levels=unique(decodes))
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

#' Coerce to Factor using Encoding if Present
#'
#' Coerces to factor, blending levels with encoding, if present as a 'guide' attribute. Vectors without encodings (or with empty encodings) acquire levels equal to \code{unique(x)} (notice that storage order controls presentation order). Vectors with non-empty encodings are decoded after harmonizing the encoding and the actual data. Factors with encodings defer to order and display value of the encoding as much as possible.  Missing levels are supplied.  Unused levels are removed. Other attributes beside 'class' and 'levels' are preserved.
#'
#' @export
#' @param x vector or factor
#' @return factor
#' @family decode
#' @examples
#' library(magrittr)
#' foo <- c(1, 2, NA, 4, 5)
#' as_factor(foo)
#' as_factor(factor(foo))
#' as_factor(as.factor(foo))
#' as_factor(structure(foo, guide = '....'))
#' as_factor(structure(foo, guide = '//5//'))
#' as_factor(structure(foo, guide = '//5/bar//'))
#' as_factor(structure(foo, guide = '//5/bar//6/baz//'))
#' as_factor(structure(factor(foo), guide = '//5/bar//'))
#' as_factor(structure(factor(foo), guide = '//5/bar//')) %>% sort
#' as_factor(structure(factor(foo), guide = '....'))
#' as_factor(structure(factor(foo), guide = '//1/bar//5/bar//'))
#'
#'
as_factor <- function(x){
  at <- attributes(x)
  at[['guide']] <- NULL
  at[['class']] <- NULL
  at[['levels']] <- NULL
  guide <- attr(x,'guide') # may be NULL (not encoded)
  vals <- if(is.factor(x)) levels(x) else unique(x)
  vals <- vals[!is.na(vals)]
  if(is.null(guide)) guide <- encode(vals) # guide present
  if(!encoded(guide)) guide <- encode(vals) # guide encoded
  if(!length(decodes(guide))) guide <- encode(vals) # guide non-empty
  codes <- codes(guide)
  decodes <- decodes(guide)
  decodes[is.na(decodes)] <- codes[is.na(decodes)] # decodes fully defined
  extra <- setdiff(vals, codes) # values not captured by encoding
  codes <- c(codes, extra) # all possible values now recognized ...
  decodes <- c(decodes, extra) # ... and displayed as themselves
  encoding <- encode(codes, labels = decodes)
  x <- as.character(x)
  x <- decode(x, encoding = encoding)
  #x <- factor(x)
  for(a in names(at))attr(x,a) <- at[[a]]
  x
}

#' Decode Data Frame.
#'
#' Decodes a data.frame. Calls as_factor() for each column with an encoded guide attribute.
#'
#' @export
#' @param x inherits data.frame
#' @param ... ignored
#' @return same class as x
#' @family decode

decode.data.frame <- function(x, ...){
  dec <- function(x){
    guide <- attr(x,'guide')
    if(is.null(guide)) return(x)
    if(is.na(guide)) return(x)
    if(!encoded(guide)) return(x)
    as_factor(x)
  }
  x[] <- lapply(x,dec)
  x
}
