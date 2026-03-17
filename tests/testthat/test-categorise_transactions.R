
context('set_categorise_transactions\n')
df1 <- data.frame(Omschrijving = c("Rente betaling", "Huur", "Interest", "Onbekend"))
res1 <- df1 |> categorise_transactions(
  Omschrijving,
  .default="weet niet",
  .rescolumn="cat",
  # categorisation rules follow:
  rentes = c("rente", "interest"),
  huren = c("huur")
  )
res2 <- df1 |> categorise_transactions(
  "Omschrijving",
  # categorisation rules follow:
  rentes = c("rente", "interest"),
  huren = c("huur")
)

test_that("categorise_transaction is okay", {
  expect_identical(res1$Omschrijving,df1$Omschrijving)
  expect_identical(names(res1),c("Omschrijving", "cat"))
  expect_identical(res1$cat,c("rentes","huren","rentes","weet niet"))
  expect_identical(res2$Omschrijving,df1$Omschrijving)
  expect_identical(names(res2),c("Omschrijving", "soort"))
  expect_identical(res2$soort,c("rentes","huren","rentes","onbekend"))
})

