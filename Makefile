.PHONY: build buildsite check clean cleanvars coverage docs getwd initialize install installcranpkg installgithubpkg installedpkgs load removepkg render setwd start test usegit
.DEFAULT_GOAL := help

# The directory where R files are stored
R_DIR = ./R

define BROWSER_PYSCRIPT
import os, webbrowser, sys
from urllib.request import pathname2url

# The input is expected to be the full HTML filename
filename = sys.argv[1]
filepath = os.path.abspath(os.path.join("./vignettes/", filename))
webbrowser.open("file://" + pathname2url(filepath))
endef
export BROWSER_PYSCRIPT

define PRINT_HELP_PYSCRIPT
import re, sys

for line in sys.stdin:
	match = re.match(r'^([a-zA-Z_-]+):.*?## (.*)$$', line)
	if match:
		target, help = match.groups()
		print("%-20s %s" % (target, help))
endef
export PRINT_HELP_PYSCRIPT

BROWSER := python3 -c "$$BROWSER_PYSCRIPT"

build: setwd ## build package
	Rscript -e "devtools::build('.')"

buildsite: setwd ## create a website for the package
	Rscript -e "pkgdown::build_site('.')"
	cp -rf docs/* ~/Documents/Pro_Website/Techtonique.github.io/tisthemachinelearner_r/

check: clean setwd ## check package 
	@read -p "Enter options (e.g: --no-tests --no-examples) or leave empty: " pckgcheckoptions; \
	if [ -z "$$pckgcheckoptions" ]; then \
		Rscript -e "try(devtools::check('.'), silent=TRUE)" && exit 0; \
	fi; \
	Rscript -e "try(devtools::check('.', args=base::strsplit('$$pckgcheckoptions', ' ')[[1]]), silent=TRUE)";

clean: ## remove all build, and artifacts
	rm -f .Rhistory
	rm -f *.RData
	rm -f *.Rproj
	rm -rf .Rproj.user
	rm -f src/*.o
	rm -f src/*.so
	rm -f vignettes/*.html
	rm -rf /Library/Frameworks/R.framework/Versions/4.3-x86_64/Resources/library/tisthemachinelearner

cleanvars: setwd ## remove all local variables
	@read -p "Do you want to remove all local variables in R? (1-yes, 2-no): " choice; \
	if [ $$choice -eq 1 ]; then \
		echo "Removing all local variables..."; \
		Rscript -e "rm(list=ls())"; \
	else \
		echo "Keeping the variables."; \
	fi

coverage: ## get test coverage
	Rscript -e "devtools::test_coverage('.')"

create: setwd ## create a new package in current directory
	Rscript -e "usethis::create_package(path = getwd(), rstudio = FALSE)"
	rm -f .here

docs: clean setwd ## generate docs		
	Rscript -e "devtools::document('.')"

getwd: ## get current directory
	Rscript -e "getwd()"

install: clean setwd docs ## install current package
	Rscript -e "try(devtools::install('.'), silent = FALSE)"

installcranpkg: setwd ## install a package
	@read -p "Enter the name of package to be installed: " pckg; \
	if [ -z "$$pckg" ]; then \
		echo "Package name cannot be empty."; \
		exit 1; \
	fi; \
	Rscript -e "utils::install.packages('$$pckg', repos='https://cloud.r-project.org')";

installgithubpkg: setwd ## install a package from GitHub ('repository/pkgname')
	@read -p "Enter the name of package to be installed ('repository/pkgname'): " pckg; \
	if [ -z "$$pckg" ]; then \
		echo "Package name cannot be empty."; \
		exit 1; \
	fi; \
	Rscript -e "devtools::install_github('$$pckg')";

installedpkgs: ## list of installed packages
	Rscript -e "utils::installed.packages()[,c(10, 16)]"

initialize: setwd ## initialize: install packages devtools, usethis, pkgdown and rmarkdown
	Rscript -e "utils::install.packages(c('devtools', 'remotes', 'roxygen2', 'usethis', 'pkgdown', 'rmarkdown'), repos='https://cloud.r-project.org')"

help: ## print menu with all options
	@python3 -c "$$PRINT_HELP_PYSCRIPT" < $(MAKEFILE_LIST)

load: clean setwd docs ## load all and restart (when developing the package)
	Rscript -e "devtools::load_all('.')"
	@read -p "Start R session? (y/n): " choice; \
	if [ "$$choice" = "y" ]; then \
		$(MAKE) start; \
	fi

removepkg: ## remove package
	@read -p "Enter the name of package to be removed: " pckg; \
	if [ -z "$$pckg" ]; then \
		echo "Package name cannot be empty."; \
		exit 1; \
	fi; \
	Rscript -e "utils::remove.packages('$$pckg')"; \
	Rscript -e "base::unlink(paste0(.libPaths()[1], '/$$pckg'), recursive = TRUE, force = TRUE)"

render: ## run R markdown file in /vignettes, open rendered HTML
	@files=$$(ls -1 ./vignettes/*.Rmd | sort); \
	i=0; \
	echo "Available Rmd files:"; \
	for file in $$files; do \
		echo "$$i: $$(basename $$file .Rmd)"; \
		i=$$((i+1)); \
	done; \
	read -p "Enter the number of the Rmd file to render: " filenum; \
	filename=$$(echo $$files | cut -d' ' -f$$((filenum+1))); \
	filename=$$(basename $$filename .Rmd); \
	Rscript -e "rmarkdown::render(paste0('./vignettes/', '$$filename', '.Rmd'))"; \
	python3 -c "$$BROWSER_PYSCRIPT" "$$filename.html"

setwd: ## set working directory to current directory
	Rscript -e "setwd('.')"

start: ## start or restart R session
	Rscript -e "system('R')"

test: ## runs package tests
	Rscript -e "devtools::test('.')"	

usegit: ## initialize Git repo and initial commit
	@read -p "Enter the first commit message: " message; \
	if [ -z "$$message" ]; then \
		echo "Commit message cannot be empty."; \
		exit 1; \
	fi; \
	Rscript -e "usethis::use_git('$$message')"; \
	git add .; \
	git commit -m "$$message"