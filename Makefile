project_name = ocaml-toon

.PHONY: help
help: ## Print this help message
	@echo "";
	@echo "List of available make commands";
	@echo "";
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}';
	@echo "";

.PHONY: build
build: ## Build the project
	@dune build --profile=dev

.PHONY: build-prod
build-prod: ## Build for production (--profile=prod)
	@dune build --profile=prod

.PHONY: dev watch
dev watch: ## Build in watch mode (--profile=dev)
	@dune build -w --profile=dev

.PHONY: clean
clean: ## Clean artifacts
	@dune clean

.PHONY: test
test: ## Run the unit tests
	@dune runtest

.PHONY: test-watch
test-watch: ## Run the unit tests in watch mode
	@dune runtest -w

.PHONY: format
format: ## Format the codebase with ocamlformat
	@dune build @fmt --auto-promote

.PHONY: format-check
format-check: ## Checks if format is correct
	@dune build @fmt

.PHONY: setup-githooks
setup-githooks: ## Setup githooks
	@git config core.hooksPath .githooks

.PHONY: lock
lock: ## Lock dependencies
	@dune pkg lock

.PHONY: install
install: lock ## Install dependencies (lock and build)
	@dune build @pkg-install

.PHONY: init
init: setup-githooks lock install ## Create a local dev environment

.PHONY: utop
utop: ## Run a REPL with the project loaded
	@dune utop lib

.PHONY: bench
bench: build ## Run benchmarks
	@dune exec benchmarks/main.exe

.PHONY: subst
subst: ## Run dune substitute (for version strings)
	@dune subst
