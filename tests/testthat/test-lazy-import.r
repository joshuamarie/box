context('lazy imports')

test_that('lazy package imports are not loaded until used', {
    unloadNamespace('splines')
    expect_false(isNamespaceLoaded('splines'))
    box::use(lazy(splines))
    expect_in('splines', ls())
    expect_false(isNamespaceLoaded('splines'))
    expect_in('bs', ls(splines))
    expect_true(isNamespaceLoaded('splines'))
})

test_that('lazy package imports can be aliased', {
    unloadNamespace('splines')
    expect_not_in('spl', ls())
    box::use(spl = lazy(splines))
    expect_in('spl', ls())
    expect_not_in('splines', ls())
    expect_false(isNamespaceLoaded('splines'))
    expect_identical(spl$bs, splines::bs)
    expect_true(isNamespaceLoaded('splines'))
})

test_that('lazy package imports are evaluated only once', {
    box::use(lazy(splines))
    first = splines
    second = splines
    expect_identical(first, second)
})

test_that('lazy imports with attach lists are still attached', {
    expect_not_in('bs', ls(parent.env(environment())))
    box::use(lazy(splines[bs]))
    expect_in('bs', ls(parent.env(environment())))
})

test_that('lazy imports with an alias and an attach list bind both', {
    unloadNamespace('splines')
    box::use(spl = lazy(splines[bs]))
    expect_in('spl', ls())
    expect_in('bs', ls(parent.env(environment())))
    expect_false(isNamespaceLoaded('splines'))
    expect_identical(spl$ns, splines::ns)
    expect_true(isNamespaceLoaded('splines'))
})

test_that('lazy module imports still work', {
    box::use(lazy(mod/a))
    expect_in('a', ls())
    expect_equal(a$double(2), 4)
})

test_that('lazy module imports are not loaded until used', {
    clear_mods()
    spec = box:::parse_spec(quote(mod/a), '')
    info = eval(
        quote(find_mod(spec, caller)),
        list(spec = spec, caller = environment()),
        asNamespace('box')
    )
    expect_false(box:::is_mod_loaded(info))
    box::use(lazy(mod/a))
    expect_in('a', ls())
    expect_false(box:::is_mod_loaded(info))
    expect_equal(a$double(2), 4)
    expect_true(box:::is_mod_loaded(info))
})

test_that('lazy attached names are not loaded until used', {
    unloadNamespace('splines')
    box::use(lazy(splines[bs, ns]))
    expect_in('bs', ls(parent.env(environment())))
    expect_in('ns', ls(parent.env(environment())))
    expect_false(isNamespaceLoaded('splines'))
    expect_identical(bs, splines::bs)
    expect_true(isNamespaceLoaded('splines'))
})

test_that('renaming lazy attached names is allowed', {
    unloadNamespace('splines')
    box::use(lazy(splines[b_spline = bs]))
    expect_identical(b_spline, splines::bs)
})

test_that('lazy imports of missing packages fail at the use call', {
    expect_error(box::use(lazy(this_package_does_not_exist)))
    expect_not_in('this_package_does_not_exist', ls())
})

test_that('lazy imports of missing modules fail at the use call', {
    expect_error(box::use(lazy(mod/this_module_does_not_exist)))
    expect_not_in('this_module_does_not_exist', ls())
})
