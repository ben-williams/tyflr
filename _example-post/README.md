---
title: README.md
author: Iago Mosqueira, EC JRC G03
date: 03 February 2015
rights:  Creative Commons Share Alike 4.0
---

# Using the flr-project.org tutorial template

Tutorials documents should be written in [github's version of markdown](https://help.github.com/articles/github-flavored-markdown/), to be [processed by the [knitr package](http://yihui.name/knitr/). Please refer to those two pages for more information.

## Previewing the tutorial

In RStudio, press the **Knit html** button at the top of the document window and an html preview will open in another window. Plase check that the page is valid after making changes.

## Categories and sections

The R markdown document includes some YAML front-matter, in between lines 1 and 12. The following fields need to be completed for the document to be processed correctly by github pages:

- *title*: Provide a title to the post, if not given.
- *description*: A one line description of the content of the document
- *post_author*: Your Name <you@email.com>
- *tags*: One or more keywords, between brackets, including names of FLR packages and classes used, for example:
  tags: ["stock recruitment" FLCore FLSR FLModel]
- *categories*: Two options, *learning* for tutorials, and *gallery* for example applications.
- *section*: Section within the document category, leave empty if in doubt.
- *rights*: we are adopting by default the [Creative Commons Share Alike 4.0](https://creativecommons.org/licenses/by-sa/4.0/) license, but if you would like to adopt a different one, please consult.

## Package versions

A section at the end of the document will record the versions of the packages used when processing the document. Just copy and paste the example FLCore* line as neccesary to include any other FLR package needed.