talk:

	quarto render index.qmd
	Rscript -e 'knitr::purl("index.qmd")'
	git add index_files

short:

	quarto render index.qmd
	Rscript -e 'knitr::purl("index.qmd")'
	git add index_files

chrome:

	@open -a Google\ Chrome.app index-speaker.html

present:

	@open -a RStudio.app
	@open -a Google\ Chrome.app index-speaker.html
	@clear
	@echo "Presentation Tips"
	@echo " "
	@echo "- Cmd-F1 to mirror/unmirror external display (first plug in external display)"
	@echo "- Ctrl-Cmd-F for fullscreen of apps (“Fullscreen”)"
	@echo "- Ctrl-Cmd-G for moving an app to external display (“Go”)"
	@echo " "
	@echo "In revealjs presentation"
	@echo " "
	@echo "Z - Zoom"
	@echo "E - Export to PDF"
	@echo "F - Fullscreen"
	@echo "B - Blackboard"
	@echo "C - Crayon"
	@echo " "
	@echo "Start speaker view, push slides to external display (safest venue option) "

clean: 

	rm -rf index_files index_cache *~ *.bak index.html index-speaker.html
