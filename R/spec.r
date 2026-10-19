# Module/package specification expression grammar as a PEG, approximating the R
# parse tree:
#
# declaration → “lazy” “(” spec “)” /
#               spec
# spec        → pkg_name (“[” attach_spec “]”)? !“/” /
#               mod
# pkg_name    → name
# mod         → mod_prefix “/” mod_name (“[” attach_spec “]”)?
# mod_prefix  → name !“/” /
#               mod_prefix “/” name
# mod_name    → name
# attach_spec → “...” /
#               attach_list
# attach_list → name (“,” name)+
# name        → ‹valid R name›
#
# Note: This is missing the alias declaration, since that is not captured in the
# AST provided by R. It is instead handled separately.
# A “lazy(spec)” call marks a lazy import. It is only valid as the outermost
# call of a declaration, and “...” attach lists are rejected for lazy imports.

#' Parse a mod or pkg spec expression passed to \code{use}
#'
#' @details
#' A declaration wrapped in \code{lazy()} (for example \code{lazy(dplyr)} or
#' \code{lazy(./mod[a, b])}) marks a lazy import. The wrapper is removed before
#' the declaration is parsed, so lazy declarations accept the same forms as
#' ordinary ones, and it is recorded in the \code{lazy} field. An alias goes
#' outside the wrapper, as in \code{alias = lazy(pkg)}. \code{lazy()} must be
#' the outermost call of a declaration, takes exactly one unnamed argument, and
#' cannot be nested. Wildcard attach lists (\code{lazy(pkg[...])}) are rejected,
#' since the exported names are only known after loading.
#'
#' @param expr the mod or pkg spec expression to parse
#' @param alias the mod or pkg spec alias as a character, or \code{NULL}
#' @return \code{parse_spec} returns a named list that contains information
#' about the parsed mod specification. Currently it contains:
#' \describe{
#'  \item{\code{name}}{the module or package name}
#'  \item{\code{prefix}}{the prefix, if the spec is a module}
#'  \item{\code{attach}}{a named vector of symbols to attach, or
#'      \code{TRUE} to attach all symbols, or \code{NULL} to attach nothing}
#'  \item{\code{alias}}{the module or package alias}
#'  \item{\code{explicit}}{a logical value indicating whether the caller
#'      provided an explicit alias}
#'  \item{\code{lazy}}{a logical value indicating whether the caller lazily
#'      imports the module}
#' }
#'
#' @keywords internal
#' @name spec
parse_spec = function (expr, alias) {
    is_lazy = is_lazy_import(expr)
    spec = parse_spec_impl(strip_lazy_import(expr))
    if (is_lazy && anyNA(spec$attach)) {
        throw('cannot lazily import all names in {expr;"}; list the names or remove lazy()')
    }

    is_pkg = 'pkg' %in% names(spec)
    spec_type = if (is_pkg) pkg_spec else mod_spec
    name = spec[[if (is_pkg) 'pkg' else 'mod']]$name
    spec_type(spec, alias = alias %||% name, explicit = nzchar(alias), lazy = is_lazy)
}

lazy = function (mod) {
    box_error()
}

#' @return \code{is_lazy_import} returns \code{TRUE} if \code{expr} is a call to
#' \code{lazy}, and \code{FALSE} otherwise. It does not validate the call.
#' @rdname spec
is_lazy_import = function (expr) {
    is.call(expr) && expr[[1L]] %==% quote(lazy)
}

#' @return \code{strip_lazy_import} returns the declaration without its
#' \code{lazy()} wrapper, or unchanged if it has none. It rejects a
#' \code{lazy()} call that does not have exactly one unnamed argument, and any
#' \code{lazy()} that is not the outermost call. It must not store \code{expr}
#' in a local variable, since \code{expr} can be the empty argument of a
#' declaration such as \code{alias = }, which is reported later by the parser.
#' @rdname spec
strip_lazy_import = function (expr) {
    if (is_lazy_import(expr)) {
        if (length(expr) != 2L) {
            throw('expected exactly one declaration in {expr;"}, got {length(expr) - 1L}')
        }
        if (nzchar(names(expr)[2L] %||% '')) {
            throw('cannot give an alias inside {expr;"}; write the alias outside, as in alias = lazy(...)')
        }
        if (contains_lazy_import(expr[[2L]])) {
            throw('lazy() must be the outermost call of a declaration, found in {expr;"}')
        }
        expr[[2L]]
    } else if (contains_lazy_import(expr)) {
        throw('lazy() must be the outermost call of a declaration, found in {expr;"}')
    } else {
        expr
    }
}

#' @return \code{contains_lazy_import} returns \code{TRUE} if \code{lazy()}
#' appears anywhere in the declaration expression \code{expr}. It only descends
#' into the positions that can hold a spec (the operands of \code{/} and the
#' first operand of \code{[}), so empty arguments in attach lists are never
#' touched.
#' @rdname spec
contains_lazy_import = function (expr) {
    if (! is.call(expr)) {
        FALSE
    } else if (is_lazy_import(expr)) {
        TRUE
    } else if (expr[[1L]] %==% quote(`[`)) {
        contains_lazy_import(expr[[2L]])
    } else if (expr[[1L]] %==% quote(`/`)) {
        contains_lazy_import(expr[[2L]]) || contains_lazy_import(expr[[3L]])
    } else {
        FALSE
    }
}

#' @param spec named list of information the parser constructed from a given
#' spec expression
#' @param \dots further information about a spec, not represented by the spec
#' expression parse tree
#' @keywords internal
#' @name spec
mod_spec = function (spec, ...) {
    extra_spec = spec[setdiff(names(spec), 'mod')]
    structure(c(spec$mod, extra_spec, ...), class = c('box$mod_spec', 'box$spec'))
}

#' @keywords internal
#' @name spec
pkg_spec = function (spec, ...) {
    extra_spec = spec[setdiff(names(spec), 'pkg')]
    structure(c(spec$pkg, extra_spec, ...), class = c('box$pkg_spec', 'box$spec'))
}

#' @keywords internal
#' @name spec
spec_name = function (spec) {
    UseMethod('spec_name')
}

#' @export
`spec_name.box$mod_spec` = function (spec) {
    paste(paste(spec$prefix, collapse = '/'), spec$name, sep = '/')
}

#' @export
`spec_name.box$pkg_spec` = function (spec) {
    spec$name
}

#' @export
`print.box$spec` = function (x, ...) {
    cat(as.character(x, ...), '\n', sep = '')
    invisible(x)
}

#' @export
`as.character.box$spec` = function (x, ...) {
    r_name = function (names) {
        map_chr(
            function (n) {
                if (is.na(n)) '\u2026' else fmt('\x1b[33m{as.name(n)}\x1b[0m')
            },
            names
        )
    }

    format_attach = function (a) {
        with_alias = function (names, aliases) {
            mapply(
                function (n, a) if (n == '\u2026' || n %==% a) n else paste(a, '=', n),
                names, aliases
            )
        }

        if (is.null(a)) {
            ''
        } else {
            aliased_names = with_alias(r_name(a), r_name(names(a)))
            paste(', attach =', paste0('[', toString(aliased_names), ']'))
        }
    }

    mod_or_pkg = function (spec) {
        if (inherits(spec, 'box$mod_spec')) {
            prefix = paste(r_name(spec$prefix), collapse = '/')
            fmt('mod({prefix}/\x1b[4;33m{r_name(spec$name)}\x1b[0m)')
        } else {
            fmt('pkg(\x1b[4;33m{r_name(spec$name)}\x1b[0m)')
        }
    }

    fmt(
        'mod_spec({alias}{mod_or_pkg(x)}{format_attach(x$attach)})',
        alias = if (x$explicit) paste(r_name(x$alias), '= ') else ''
    )
}

parse_spec_impl = function (expr) {
    if (is.name(expr)) {
        if (expr %==% quote(.)) {
            list(prefix = '.', name = '__init__')
        } else if (expr %==% quote(..)) {
            list(prefix = '..', name = '.')
        } else {
            c(parse_pkg_name(expr), list(attach = NULL))
        }
    } else if (is.call(expr)) {
        if (expr[[1L]] %==% quote(`[`)) {
            if (is.name((name = expr[[2L]]))) {
                if (name %==% quote(.)) {
                    c(list(prefix = '.', name = '__init__'), parse_attach_spec(expr))
                } else if (name %==% quote(..)) {
                    # parse_mod(bquote(../.(`[[<-`(expr, 2L, quote(.)))))
                    c(list(prefix = '..', name = '.'), parse_attach_spec(expr))
                } else {
                    c(parse_pkg_name(name), parse_attach_spec(expr))
                }
            } else {
                throw('expected a name in {expr;"}, got {describe_token(name)}')
            }
        } else if (expr[[1L]] %==% quote(`/`)) {
            parse_mod(expr)
        } else {
            throw('expected {"/";"} in {expr;"}, got {expr[[1L]];"}')
        }
    } else {
        throw('expected a module or package name, got {describe_token(expr)}')
    }
}

parse_pkg_name = function (expr) {
    name = parse_name(expr)
    if (is.na(name)) {
        throw('alias without a name provided in use declaration')
    }
    list(pkg = list(name = name))
}

parse_mod = function (expr) {
    prefix = parse_mod_prefix(expr[[2L]])
    mod = expr[[3L]]

    # Runs of `..` from the start are valid; `..` in the middle is not:

    if (any(diff(which(c(TRUE, prefix$prefix == '..'))) > 1L)) {
        # At least one gap in the sequence of `..` from the start
        throw('{"..";"} can only be used as a prefix')
    }

    if ('.' %in% prefix$prefix[-1L]) {
        throw('{".";"} can only be used as a prefix')
    }

    if (is.call(mod)) {
        if (mod[[1L]] %==% quote(`[`)) {
            c(
                list(mod = c(parse_mod_name(mod[[2L]], prefix), prefix)),
                parse_attach_spec(mod)
            )
        } else {
            throw('expected a name in {expr;"}, got {describe_token(mod)}')
        }
    } else if (is.name(mod)) {
        list(mod = c(parse_mod_name(mod, prefix), prefix), attach = NULL)
    } else {
        throw('expected a name in {expr;"}, got {describe_token(mod)}')
    }
}

parse_mod_prefix = function (expr) {
    if (is.name(expr)) {
        list(prefix = deparse1(expr))
    } else if (is.call(expr) && expr[[1L]] %==% quote(`/`)) {
        if (! is.name(expr[[3L]])) {
            throw('expected a name in module prefix, got {describe_token(expr[[3L]])}')
        } else {
            suffix = deparse1(expr[[3L]])
            list(prefix = c(parse_mod_prefix(expr[[2L]])$prefix, suffix))
        }
    } else {
        throw('expected a module prefix, got {describe_token(expr)}')
    }
}

parse_mod_name = function (expr, prefix) {
    name = parse_name(expr)

    if (length((prefix = prefix$prefix)) > 0L) {
        dot_after_prefix = name == '.'
        back_inside_path = ! all(prefix == '..') && name == '..'

        if (dot_after_prefix || back_inside_path) {
            throw('{expr;"} can only be used as a prefix')
        }
    }

    list(name = name)
}

parse_attach_spec = function (expr) {
    syms = parse_attach_list(expr[-1L][-1L])
    list(attach = assign_missing_names(syms))
}

assign_missing_names = function (syms) {
    x = stats::setNames(syms, names(syms) %|% unlist(syms))
    x[x == '...'] = NA_character_

    if (any((dup = duplicated(names(x))))) {
        throw('cannot attach duplicate names, found duplicated {unique(names(x)[dup]);"}')
    }
    if (any(names(syms)[is.na(x)] != '')) {
        throw('wildcard imports cannot be aliased')
    }
    x
}

parse_attach_list = function (expr) {
    if (length(expr) == 1L && expr[[1L]] %==% quote(expr =)) {
        throw('expected at least one name in attach list')
    } else {
        names = stats::setNames(map_chr(parse_name, expr), names(expr))

        missing_names = is.na(names)
        if (any(missing_names)) {
            # Allow missing name for last, unnamed argument to allow “trailing
            # comma” syntax:
            #   box::use(./a[x, y, ])
            index = which(missing_names)
            if (
                length(index) != 1L
                || index != length(names)
                || nzchar(names(names)[index] %||% '')
            ) {
                throw('alias without a name provided in attach list')
            }

            names[-index]
        } else {
            names
        }
    }
}

parse_name = function (expr) {
    if (length(expr) != 1L || ! is.name(expr)) {
        throw('expected a name, got {describe_token(expr)}')
    } else {
        deparse1(expr) %||% NA_character_
    }
}

describe_token = function (expr) {
    fmt('{class(expr)} literal {expr;"}')
}
