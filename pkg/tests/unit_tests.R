library(RUnit)

testsuite <- defineTestSuite("Event Studies Test Suite",
                             dirs = ".",
                             testFileRegexp = "^test_.*\\.R$",
                             testFuncRegexp = "^.*$"
                             )

testresult <- runTestSuite(testsuite)
