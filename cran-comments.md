## Test environments
* Ubuntu Linux 16.04 LTS

## R CMD check results
There were no ERRORs or WARNINGs. 

1 note
checking for future file timestamps ... NOTE unable to verify current time

## R submission reviewer comments

### v0.0.1

Thanks, please write package names, software names and API names in single quotes (e.g. 'Cytoscape') in Title and Description.

Please add an URL for 'Cytoscape' in your Description text in the form
<http:...> or <https:...>
with angle brackets for auto-linking and no space after 'http:' and 'https:'.


Why is your function call in the example

file.path <- system.file("extdata", "example.R", package = "ScriptMapR")
'# scriptmapr(file.path)

commented out?
Please ensure that examples are executable.


Please also ensure that you do not modify the user's global environment (e.g. by using <<-) in your functions.


Please fix and resubmit. 

