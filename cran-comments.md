## Resubmission
This is a resubmission. In this version I have:

* reformatted the DESCRIPTION File
* modified the Example Documentation
* used tempdir() by default for all functions
* removed options(warn = -1) from setup.R
* added selective warning suppression helper function
* revised functions to not modify .GlobalEnv
* added environment parameters for user control
* replaced installed.packages() with requireNamespace()
* removed automatic installation from all functions
* only provided installation suggestions via messages
