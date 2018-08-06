# Pywget

Student id: 300313764
Student name: David Barnett

Assertions made: That a url cannot contain whitespace. This is used
for the regex for capturing urls from href's and src's. The regex also
captures link's so style sheets will be downloaded as well if present.

Unsure of if third party libraries were really allowed, this is an alternative
version and functionally equivalent which works only with using `re` instead
of the third party library `lxml` for HTML etree parsing which works
a lot more cleanly.

## What was implemented

core.py
-------

* The specified file is downloaded
* The names of files are preserved from the url file names
* Name collisions are detected with `resolve_name` and increment in the
  correct format

completion.py
-------------

All of above plus:

* If the target has an `html` extension the file is parsed for its linked
  files such as links to other pages via `<a>` tags and images via `<img>`
  tags

* If the linked file is of a absolute path, and of the same domain as the
  initial, they are made relative to the root file



challenge.py
------------

All of above plus:

* Creates the directory structure of `<domain>/<path ..>/file.ext`
  and sorts the linked files from Completion into the paths they live
  in the website with corrected links

* The links are always pointing to the version they were downloaded with
  So index.1.html links to felids.1.html and so on.
