
context('get_table_cbs_odata4 retrieve global info\n')


test_that("retrieve global info", {
  get_beta =
    purrr::partial(HOQCutil::get_table_cbs_odata4,odata_cat='')
  contents1 = get_beta()
  cats1  = get_beta(subtable="Catalogs")
  dsets1 = get_beta(subtable="Datasets")
  xml1   = get_beta(subtable="$MetaData")
  contents1a = get_beta(restype='')
  contents1b = jsonlite::fromJSON(get_beta(restype='json'))
  contents1c = get_beta(restype='resp')
  expect_identical(class(contents1),"data.frame")
  expect_identical(dim(contents1),c(2L,3L))
  expect_identical(class(cats1),"data.frame")
  expect_identical(dim(cats1),c(2L,9L))
  expect_identical(class(dsets1),"data.frame")
  expect_true(dim(dsets1)[1]>0)
  expect_equal(dim(dsets1)[2],13)
  expect_identical(class(xml1),c("xml_document", "xml_node"))
  expect_identical(contents1,contents1a)
  expect_equal(names(contents1b),c("@odata.context", "value"))
  expect_identical(class(contents1c),"response")
})

context('get_table_cbs_odata4 retrieve cat info\n')

test_that("retrieve cat info", {
  get_beta =
    purrr::partial(HOQCutil::get_table_cbs_odata4,odata_cat='CBS-asd')
  contents2 = get_beta()
  cats2 = get_beta(subtable="Catalogs")
  dsets2 = get_beta(subtable="Datasets")
  expect_identical(class(contents2),"data.frame")
  expect_identical(dim(contents2),c(2L,3L))
  expect_identical(class(cats2),"data.frame")
  expect_identical(dim(cats2),c(1L,9L))
  expect_identical(class(dsets2),"data.frame")
  expect_true(dim(dsets2)[1]>0)
  expect_equal(dim(dsets2)[2],13)
})

context('get_table_cbs_odata4 retrieve table info\n')

test_that("retrieve table info", {
  table_id  = '900001NED'
  odata_cat = 'CBS-asd'
  table_id  = '81589NED'
  odata_cat = 'CBS'
  get_beta  =
    purrr::partial(HOQCutil::get_table_cbs_odata4,
                   table_id  = table_id,
                   odata_cat = odata_cat)

  contents3 = get_beta()
  expect_identical(class(contents3),"data.frame")
  expect_true(dim(contents3)[1]>=4)
  expect_equal(dim(contents3)[2],3L)

  subtable0 = get_beta(subtable="$Metadata")
  expect_identical(class( subtable0),c("xml_document", "xml_node"))

  subtable1 = get_beta(subtable='Properties')
  expect_identical(class(subtable1),"list")
  expect_equal(length(subtable1),28L)

  subtable2 = get_beta(subtable='MeasureCodes')
  expect_identical(class(subtable2),"data.frame")
  expect_equal(dim(subtable2)[2],10L)

  if ('MeasureGroups' %in% contents3$name) {
    subtable3 = get_beta(subtable='MeasureGroups')
    expect_identical(class(subtable3),"data.frame")
    expect_equal(dim(subtable3)[2],5L)
  }

  subtable4 = get_beta(subtable='Dimensions')
  expect_identical(class(subtable4),"data.frame")
  expect_equal(dim(subtable4)[2],6L)

  # Handling of codes and groups in Dimensions
  purrr::walk(subtable4$Identifier,
           function(dimtab) {
             purrr::walk(c('Codes','Groups'),
                  function(cg)  {
                     subtab = paste0(dimtab,cg)
                     # print(subtab) # debug
                     x = get_beta(subtable=subtab)
                     if (!class(x)=="character") {
                       # print(subtab) # debug
                       expect_identical(class(x),"data.frame")
                       expect_true(dim(x)[2]>=5L)
                     }
                  } )
             } )


})


context('get_table_cbs_odata4 table queries count, skip, top\n')


test_that("table queries count, skip, top", {
  table_id  = '900002NED'
  odata_cat = 'CBS-asd'
  get_beta  =
    purrr::partial(HOQCutil::get_table_cbs_odata4,
                   table_id  = table_id,
                   odata_cat = odata_cat)
  get_beta_nt  =
    purrr::partial(HOQCutil::get_table_cbs_odata4,
                   odata_cat = odata_cat)

  # $count
  q4 = get_beta(subtable='MeasureCodes')
  q5 = get_beta(subtable='MeasureCodes',query="$Count")
  q5 = as.numeric(stringr::str_sub(q5)) # convert Unicode
  q6 = as.numeric(get_beta_nt(
     subtable='Datasets',
     query=
       glue::glue("$select=ObservationCount&$filter=Identifier eq '{table_id}'")
    ))
  q7 = get_beta(subtable='Observations',query="$count")
  q7 = as.numeric(stringr::str_sub(q7)) # convert Unicode
  expect_equal(dim(q4)[1],q5)
  expect_equal(q6,q7)

  # $skip and $top
  q0 = get_beta(subtable='MeasureCodes')$Identifier
  q1 = get_beta(subtable='MeasureCodes',query='$skip=1')$Identifier
  q2 = get_beta(subtable='MeasureCodes',query='$top=2')$Identifier
  q3 = get_beta(subtable='MeasureCodes',query='$skip=1&$top=1')$Identifier

  if (q5 > 1) {
    expect_equal(q1, q0[-1])
    expect_equal(q2, q0[1:2])
    expect_equal(q3, q0[2])
  } else {
    expect_true(is.null(q1))
    expect_equal(length(q2), 1L)
    expect_true(is.null(q3))
  }
})


context('get_table_cbs_odata4 table queries startswith, endswith, substring, contains\n')


test_that("table queries startswith, endswith, substr, contains", {
  table_id  = '900002NED'
  odata_cat = 'CBS-asd'
  get_beta  =
    purrr::partial(HOQCutil::get_table_cbs_odata4,
                   table_id  = table_id,
                   odata_cat = odata_cat)

  q0 = get_beta(subtable='Observations')

  q1 = q0[stringr::str_detect(q0$OData3Identifier,'^Uitvoer'),]
  rownames(q1)=NULL
  q2 = get_beta(subtable='Observations',query="$filter=startswith(OData3Identifier,'Uitvoer')")
  expect_identical(q1,q2)
  q3 = q0[stringr::str_detect(q0$OData3Identifier,'_20$'),]
  rownames(q3)=NULL
  q4 = get_beta(subtable='Observations',query="$filter=endswith(OData3Identifier,'_20')")
  expect_identical(q3,q4)

  q5 = q0[stringr::str_detect(q0$OData3Identifier,stringr::regex('Uitvoer',ignore_case=F)),]
  rownames(q5)=NULL
  q6 = get_beta(subtable='Observations',query="$filter=contains(OData3Identifier,'Uitvoer')")
  expect_identical(q5,q6)

  q7 = q0[stringr::str_sub(q0$OData3Identifier,12,18) == 'Uitvoer',]
  rownames(q7)=NULL
  q8 = get_beta(subtable='Observations',query="$filter=substring(OData3Identifier,11,7) eq 'Uitvoer'")
  expect_identical(q7,q8)

})

context('get_table_cbs_odata4 table queries id\n')


test_that("table queries table queries id", {
  table_id  = '900002NED'
  odata_cat = 'CBS-asd'
  get_beta  =
    purrr::partial(HOQCutil::get_table_cbs_odata4,
                   table_id  = table_id,
                   odata_cat = odata_cat)

  obs_id = 5
  q1 = get_beta(subtable = 'Observations',
                query = glue::glue("({obs_id})"))
  q2 = get_beta(subtable = 'Observations',
                query = glue::glue("$filter=Id eq {obs_id}"))
  expect_identical(q1, q2)

})

