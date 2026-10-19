context('spec parser')

test_use = function (...) {
    call = match.call()
    parse_spec(call[[2L]], names(call)[[2L]] %||% '')
}

is_mod_spec = function (x) {
    inherits(x, 'box$mod_spec')
}

is_pkg_spec = function (x) {
    inherits(x, 'box$pkg_spec')
}

test_that('modules without attaching can be parsed', {
    m = test_use(foo/bar)
    expect_true(is_mod_spec(m))
    expect_equal(m$name, 'bar')
    expect_equal(m$prefix, 'foo')
    expect_equal(m$alias, 'bar')
    expect_false(m$explicit)
    expect_null(m$attach)
})

test_that('parse errors are correctly handled', {
    expect_box_error(
        test_use(foo/bar[]),
        '^expected at least one name in attach list'
    )
})

test_that('fully qualified names can be nested', {
    m = test_use(foo/bar/baz)
    expect_true(is_mod_spec(m))
    expect_equal(m$name, 'baz')
    expect_equal(m$prefix, c('foo', 'bar'))
    expect_equal(m$alias, 'baz')
    expect_false(m$explicit)
    expect_null(m$attach)
})

test_that('imports can be relative', {
    m = test_use(./foo)
    expect_true(is_mod_spec(m))
    expect_equal(m$name, 'foo')
    expect_equal(m$prefix, '.')
    expect_null(m$attach)

    m = test_use(./foo[bar])
    expect_true(is_mod_spec(m))
    expect_equal(m$name, 'foo')
    expect_equal(m$prefix, '.')
    expect_equal(m$attach, c(bar = 'bar'))

    m = test_use(..)
    expect_true(is_mod_spec(m))
    expect_equal(m$name, '.')
    expect_equal(m$prefix, '..')
    expect_null(m$attach)

    m = test_use(..[foo])
    expect_true(is_mod_spec(m))
    expect_equal(m$name, '.')
    expect_equal(m$prefix, '..')
    expect_equal(m$attach, c(foo = 'foo'))

    m = test_use(../foo)
    expect_true(is_mod_spec(m))
    expect_equal(m$name, 'foo')
    expect_equal(m$prefix, '..')
    expect_null(m$attach)

    m = test_use(../foo[bar])
    expect_true(is_mod_spec(m))
    expect_equal(m$name, 'foo')
    expect_equal(m$prefix, '..')
    expect_equal(m$attach, c(bar = 'bar'))
})

test_that('`./` can only be used as a prefix', {
    expect_box_error(test_use(a/./b), 'can only be used as a prefix')
    expect_box_error(test_use(././b), 'can only be used as a prefix')
    expect_box_error(test_use(a/b/.), 'can only be used as a prefix')
    expect_box_error(test_use(a/./b[foo]), 'can only be used as a prefix')
    expect_box_error(test_use(././b[foo]), 'can only be used as a prefix')
    expect_box_error(test_use(a/b/.[foo]), 'can only be used as a prefix')

    expect_error(test_use(../../b), NA)
    expect_box_error(test_use(.././b), 'can only be used as a prefix')
    expect_box_error(test_use(../.), 'can only be used as a prefix')
    expect_error(test_use(../../b[foo]), NA)
    expect_box_error(test_use(.././b[foo]), 'can only be used as a prefix')
    expect_box_error(test_use(../.[foo]), 'can only be used as a prefix')

    expect_box_error(test_use(a/../b), 'can only be used as a prefix')
    expect_box_error(test_use(a/b/..), 'can only be used as a prefix')
    expect_box_error(test_use(a/../b[foo]), 'can only be used as a prefix')
    expect_box_error(test_use(a/b/..[foo]), 'can only be used as a prefix')

    expect_box_error(test_use(./../b), 'can only be used as a prefix')
    expect_box_error(test_use(./..), 'can only be used as a prefix')
    expect_box_error(test_use(./../b[foo]), 'can only be used as a prefix')
    expect_box_error(test_use(./..[foo]), 'can only be used as a prefix')
})

test_that('modules can have explicit aliases', {
    m = test_use(qux = foo/bar)
    expect_true(is_mod_spec(m))
    expect_equal(m$name, 'bar')
    expect_equal(m$prefix, 'foo')
    expect_equal(m$alias, 'qux')
    expect_true(m$explicit)
    expect_null(m$attach)
})

test_that('modules can have specific exports', {
    m = test_use(foo/bar[sym1, sym2])
    expect_true(is_mod_spec(m))
    expect_equal(m$name, 'bar')
    expect_equal(m$prefix, 'foo')
    expect_equal(m$alias, 'bar')
    expect_false(m$explicit)
    expect_equal(m$attach, c(sym1 = 'sym1', sym2 = 'sym2'))
})

test_that('modules with alias can have specific exports', {
    m = test_use(baz = foo/bar[sym1, sym2])
    expect_true(is_mod_spec(m))
    expect_equal(m$name, 'bar')
    expect_equal(m$prefix, 'foo')
    expect_equal(m$alias, 'baz')
    expect_true(m$explicit)
    expect_equal(m$attach, c(sym1 = 'sym1', sym2 = 'sym2'))
})

test_that('modules can export everything', {
    m = test_use(foo/bar[...])
    expect_true(is_mod_spec(m))
    expect_equal(m$name, 'bar')
    expect_equal(m$prefix, 'foo')
    expect_equal(m$alias, 'bar')
    expect_false(m$explicit)
    expect_true(is.na(m$attach))
})

test_that('attached names can have aliases', {
    m = test_use(foo/bar[alias1 = sym1, sym2])
    expect_true(is_mod_spec(m))
    expect_equal(m$name, 'bar')
    expect_equal(m$prefix, 'foo')
    expect_equal(m$alias, 'bar')
    expect_false(m$explicit)
    expect_equal(m$attach, c(alias1 = 'sym1', sym2 = 'sym2'))
})

test_that('exports and aliases can’t be duplicated', {
    expect_box_error(test_use(foo/bar[a, b, a]), 'duplicate names')
    expect_box_error(test_use(foo/bar[x = a, y = b, x = c]), 'duplicate names')
    expect_box_error(test_use(foo/bar[..., ...]), 'duplicate names')

    m = test_use(foo/bar[x = a, a = b, b])
    expect_true(is_mod_spec(m))
    expect_equal(m$attach, c(x = 'a', a = 'b', b = 'b'))
})

test_that('wildcards can be mixed with aliases', {
    m = test_use(foo/bar[x = a, y = b, ...])
    expect_true(is_mod_spec(m))
    expect_equal(m$name, 'bar')
    expect_equal(m$prefix, 'foo')
    expect_equal(m$alias, 'bar')
    expect_false(m$explicit)
    expect_equal(m$attach, c(x = 'a', y = 'b', ... = NA_character_))
})

test_that('wildcard can’t have alias', {
    expect_box_error(test_use(foo/bar[x = ...]), 'cannot be aliased')
    expect_box_error(test_use(foo/bar[x = a, y = ...]), 'cannot be aliased')
})

# … the same for packages.

test_that('packages without attaching can be parsed', {
    m = test_use(foo)
    expect_true(is_pkg_spec(m))
    expect_equal(m$name, 'foo')
    expect_equal(m$alias, 'foo')
    expect_false(m$explicit)
    expect_null(m$attach)
})

test_that('packages can have explicit alias', {
    m = test_use(bar = foo)
    expect_true(is_pkg_spec(m))
    expect_equal(m$name, 'foo')
    expect_equal(m$alias, 'bar')
    expect_true(m$explicit)
    expect_null(m$attach)
})

test_that('packages can have specific exports', {
    m = test_use(foo[sym1, sym2])
    expect_true(is_pkg_spec(m))
    expect_equal(m$name, 'foo')
    expect_equal(m$alias, 'foo')
    expect_false(m$explicit)
    expect_equal(m$attach, c(sym1 = 'sym1', sym2 = 'sym2'))
})

test_that('packages with alias can have specific exports', {
    m = test_use(bar = foo[sym1, sym2])
    expect_true(is_pkg_spec(m))
    expect_equal(m$name, 'foo')
    expect_equal(m$alias, 'bar')
    expect_true(m$explicit)
    expect_equal(m$attach, c(sym1 = 'sym1', sym2 = 'sym2'))
})

test_that('packages can export everything', {
    m = test_use(foo[...])
    expect_true(is_pkg_spec(m))
    expect_equal(m$name, 'foo')
    expect_equal(m$alias, 'foo')
    expect_false(m$explicit)
    expect_true(is.na(m$attach))
})

test_that('attached names in packages can have aliases', {
    m = test_use(foo[alias1 = sym1, sym2])
    expect_true(is_pkg_spec(m))
    expect_equal(m$name, 'foo')
    expect_equal(m$alias, 'foo')
    expect_false(m$explicit)
    expect_equal(m$attach, c(alias1 = 'sym1', sym2 = 'sym2'))
})

test_that('exports and aliases can’t be duplicated', {
    expect_box_error(test_use(foo[a, b, a]), 'duplicate names')
    expect_box_error(test_use(foo[x = a, y = b, x = c]), 'duplicate names')
    expect_box_error(test_use(foo[..., ...]), 'duplicate names')

    m = test_use(foo[x = a, a = b, b])
    expect_true(is_pkg_spec(m))
    expect_equal(m$attach, c(x = 'a', a = 'b', b = 'b'))
})

test_that('wildcards can be mixed with aliases', {
    m = test_use(foo[x = a, y = b, ...])
    expect_true(is_pkg_spec(m))
    expect_equal(m$name, 'foo')
    expect_equal(m$alias, 'foo')
    expect_false(m$explicit)
    expect_equal(m$attach, c(x = 'a', y = 'b', ... = NA_character_))
})

test_that('wildcard can’t have alias', {
    expect_box_error(test_use(foo[x = ...]), 'cannot be aliased')
    expect_box_error(test_use(foo[x = a, y = ...]), 'cannot be aliased')
})

test_that('trailing comma is accepted', {
    expect_error(test_use(mod/a, ), NA)
    expect_error(test_use(mod/a, mod/b, ), NA)
    expect_error(test_use(mod/a[modname, double, ]), NA)
})

test_that('aliases need a name', {
    expect_box_error(test_use(alias =), 'alias without a name provided in use declaration')
    expect_box_error(box::use(mod/a, alias =), 'alias without a name provided in use declaration')
    expect_box_error(test_use(foo/bar[alias =, y]), 'alias without a name provided in attach list')
    expect_box_error(test_use(foo/bar[x, alias =]), 'alias without a name provided in attach list')
})

test_that('lazy imports are marked with lazy()', {
    m = test_use(lazy(dplyr))
    expect_true(is_pkg_spec(m))
    expect_equal(m$name, 'dplyr')
    expect_equal(m$alias, 'dplyr')
    expect_false(m$explicit)
    expect_null(m$attach)
    expect_true(m$lazy)
})

test_that('imports are not lazy by default', {
    expect_false(test_use(dplyr)$lazy)
    expect_false(test_use(foo/bar)$lazy)
    expect_false(test_use(dplyr[...])$lazy)
})

test_that('lazy imports can have explicit aliases', {
    m = test_use(dl = lazy(dplyr))
    expect_true(is_pkg_spec(m))
    expect_equal(m$name, 'dplyr')
    expect_equal(m$alias, 'dl')
    expect_true(m$explicit)
    expect_true(m$lazy)
})

test_that('lazy imports can have specific exports', {
    m = test_use(lazy(dplyr[filter, keep = select]))
    expect_true(is_pkg_spec(m))
    expect_equal(m$attach, c(filter = 'filter', keep = 'select'))
    expect_true(m$lazy)
})

test_that('lazy imports work for modules', {
    m = test_use(lazy(foo/bar))
    expect_true(is_mod_spec(m))
    expect_equal(m$name, 'bar')
    expect_equal(m$prefix, 'foo')
    expect_true(m$lazy)
})

test_that('lazy imports work for relative modules with exports', {
    m = test_use(lazy(./foo/bar[sym]))
    expect_true(is_mod_spec(m))
    expect_equal(m$name, 'bar')
    expect_equal(m$prefix, c('.', 'foo'))
    expect_equal(m$attach, c(sym = 'sym'))
    expect_true(m$lazy)
})

test_that('lazy imports cannot attach all names', {
    expect_box_error(test_use(lazy(dplyr[...])), 'cannot lazily import all names')
    expect_box_error(test_use(lazy(dplyr[filter, ...])), 'cannot lazily import all names')
    expect_box_error(test_use(lazy(foo/bar[...])), 'cannot lazily import all names')
    expect_box_error(test_use(dl = lazy(dplyr[...])), 'cannot lazily import all names')
})

test_that('lazy imports work for aliased relative modules', {
    m = test_use(dl = lazy(./foo/bar))
    expect_true(is_mod_spec(m))
    expect_equal(m$name, 'bar')
    expect_equal(m$prefix, c('.', 'foo'))
    expect_equal(m$alias, 'dl')
    expect_true(m$explicit)
    expect_true(m$lazy)
})

test_that('the ~ marker is not accepted', {
    expect_box_error(test_use(~dplyr), '^expected')
    expect_box_error(test_use(dl = ~dplyr), '^expected')
    expect_box_error(test_use(a ~ b), '^expected')
})

test_that('lazy() takes exactly one declaration', {
    expect_box_error(test_use(lazy()), 'expected exactly one declaration')
    expect_box_error(test_use(lazy(dplyr, tidyr)), 'expected exactly one declaration')
})

test_that('lazy() does not accept an alias inside', {
    expect_box_error(test_use(lazy(dl = dplyr)), 'cannot give an alias')
})

test_that('lazy() must be the outermost call', {
    expect_box_error(test_use(lazy(lazy(dplyr))), 'outermost')
    expect_box_error(test_use(foo/lazy(bar)), 'outermost')
    expect_box_error(test_use(lazy(foo)/bar), 'outermost')
    expect_box_error(test_use(lazy(dplyr)[filter]), 'outermost')
})

test_that('is_lazy_import detects only calls to lazy()', {
    expect_true(is_lazy_import(quote(lazy(a))))
    expect_true(is_lazy_import(quote(lazy(a/b[c]))))
    expect_true(is_lazy_import(quote(lazy(a, b))))
    expect_false(is_lazy_import(quote(a)))
    expect_false(is_lazy_import(quote(~a)))
    expect_false(is_lazy_import(quote(a/b)))
    expect_false(is_lazy_import(quote(f(x)(y))))
})
