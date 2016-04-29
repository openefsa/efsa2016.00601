
# combines knitr & pandoc calls
manuscript: analysis/mapProposals.Rmd
	Rscript -e 'library("rmarkdown");library("methods"); render("./analysis/mapProposals.Rmd",word_document(toc=T,fig_caption=T))'

manuscript_html: analysis/mapProposals.Rmd
	Rscript -e 'library("rmarkdown");library("methods"); render("./analysis/mapProposals.Rmd","html_document")'


clean:
	rm -f $(MANUSCRIPT).md
	rm -rf analysis/mapProposals_cache
