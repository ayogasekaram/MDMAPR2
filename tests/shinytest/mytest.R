app <- ShinyDriver$new("../../")
app$snapshotInit("mytest")

log <- app$getDebugLog("all")
print(log)


# app$takeScreenshot()

app$snapshot()

# app$setInputs(sidebarCollapsed = FALSE)
app$setInputs(tab_being_displayed = "dataImport", allowInputNoBinding_ = TRUE)
# app$setInputs(`tab_being_displayed-dataImport` = "click")
# app$setInputs(`tabs_being_displayed` = "click", allowInputNoBinding_ = TRUE)
# app$setInputs(`examplemodule1-button` = "click")
app$snapshot()

# Upload the files here
app$uploadFile(qpcr_file = "../files/AAFC_rdml_file.rdml")

app$uploadFile(SCI_fluorescence_file = "../files/std.curve.lori.sample.csv")
app$uploadFile(metadata_file = "../files/Phillips_OKMetadata_Template_abi_filled.xlsx")

app$setInputs(platform = "MIC")

app$setInputs(upload_data_name = "SnapShotTest1")

app$snapshot()

app$setInputs(`submit` = 'click')

app$snapshot()
