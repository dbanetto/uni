# Pywget

Third Party Libraries used: `lxml`

I used `lxml` in this project for its extension
to xml.etree.ElementTree in the standard library that allows for HTML parsing.

## What was implemented

Core
----

* The specified file is downloaded
* The names of files are preserved from the url file names
* Name collisions are detected with `resolve_name` and increment in the
  correct format

Completion
----------

* If the target has an `html` extension the file is parsed for its linked
  files such as links to other pages via `<a>` tags and images via `<img>`
  tags

* If the linked file is of a absolute path, and of the same domain as the
  initial, they are made relative to the root file



Challenge
---------

* Creates the directory structure of `<domain>/<path ..>/file.ext`
  and sorts the linked files from Completion into the paths they live
  in the website with corrected links
