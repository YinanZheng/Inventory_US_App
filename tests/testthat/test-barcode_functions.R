test_that("export_barcode_pdf generates a PDF file", {
  sku <- "TEST123"
  quantity <- 5
  pdf_path <- export_barcode_pdf(sku, quantity)
  
  expect_true(file.exists(pdf_path), info = "PDF file should exist after generation")
  expect_true(grepl("\\.pdf$", pdf_path), info = "Generated file should have a .pdf extension")
})