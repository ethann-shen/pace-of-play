# README  
There are multiple methods for creating a bookdown.  This template enables a streamlined approach where the bookdown template has been overlayed into an RStudio Project which is also a Git repository.

## Bookdown  
[Bookdown](https://bookdown.org/home/about/) is an open source R package that makes it easy to write books.  It works really well within the  RStudio project environment and is the preferred method within the Duke Stats department for producing and submitting honors theses. 

## Procedures  
1. Update/install: R/RStudio.  This workshop assumes you have [Git](https://git-scm.com/) installed.  

    - Windows: install [Rtools](https://cran.r-project.org/bin/windows/Rtools/)  
    
2. Update/install tidyverse, bookdown, tinytex.  Then install TinyTex distribution

```
install.packages(c("tidyverse", "bookdown", "tinytex"))
tinytex::install_tinytex()   
```

3. Clone your customized GitHub repository (https://github.com/DukeStatSci)
3. Create a test Bookdown Book:  `Build` tab > `Build Book` button.  Your test book should build without errors
3. Develop your analysis; compose and orchestrate your HTML book.  Ignore PDF for now 

## Tips  
1. In bookdown, each chapter is an Rmd file found in the project root.  (index.Rmd [required], 01, 02, ... 999999)
1. Files and directory names that begin with underscore _ will not be processed by `bookdown::render_book()`
1. Cannot have identical code-chunk names
1. Code chunks that produce visualizations should have a code chunk _name_
1. Synchronize the file_name for each chapter with HEADER 1: #, line 1
1. Cannot have identical identifiers for chapters and sections.  Can have custom identifiers {#foo-identifier}
1. [**Merge and Knit** or _Knit and Merge_](https://bookdown.org/yihui/bookdown/new-session.html)
1. Your compiled book is in the _book directory.  You may put _book and other directories into .gitignore 

## In Workshop

1. Open `index.Rmd` and **Knit** a single page  

    - Note YAML metadata (_title, author, bibliography, etc._)
    - See LaTeX in the end of line 16
    - _output.yml controls the TOC header/footer.  Lines 1 & 10 identify the gitbook and PDF outputs  

1. `book.bib`  

    - Copy/Paste one of the entries. Edit the identifier (i.e. the first line of the copied bib entry), the author, and the title
    - Tools like Zotero can generate a .bib file
    
1. `45-tables_are_fun.Rmd`  

    - line 54:  Reference the new citation you created in step 2
    - line 61:  LaTeX `$$` or `$`
    - line 5 & 37:  uncomment `library(gt)` and change `eval=TRUE`
    
1. Build > Build Book will generate the whole gitbook and the PDF derivative  
1. `_bookdown.yml`:  Optionally, you can edit the `book_filename`.  "_main" is the default  
1. As time allows make a new chapter (e.g. Visualization) and display a plot without code

```


```


## license: “CC BY-NC”  
- [*Choose license(s)*](https://docs.google.com/presentation/d/1CcKWMUsH7ADCpLQZ57tfhiUIZYgKahmd_z45pVucVlw/edit#slide=id.g72011cc5c1_1_90)
- A selective list of [uesthis enabled licenses](https://usethis.r-lib.org/reference/licenses.html) can be generated automatically

Creative Commons: Attribution, Non-Commerical  
<https://creativecommons.org/licenses/by-nc/4.0/>

## References  
- [bookdown: Authoring Books and Technical Documents with R Markdown](https://bookdown.org/yihui/bookdown/)
- [R Markdown Cookbook](https://bookdown.org/yihui/rmarkdown-cookbook/)
- [R Markdown: The Definitive Guide](https://bookdown.org/yihui/rmarkdown/)
- [Tier Protocol - suggested organizational structure](https://www.projecttier.org/tier-protocol/specifications/#overview-of-the-documentation)
- [UseThis](https://usethis.r-lib.org/)
- [Happy Git and GitHub for the useR](https://happygitwithr.com/)



