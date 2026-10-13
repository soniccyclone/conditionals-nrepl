.PHONY: test eastwood cljfmt kondo kondo-clr cloverage install clean lint javac javac-test docs verify-docs verify-cljdoc
.DEFAULT_GOAL := install

# Set bash instead of sh for the @if [[ conditions,
# and use the usual safety flags:
SHELL = /bin/bash -Ee

CLOJURE_VERSION ?= 1.12
# The version local installs get.
PROJECT_VERSION ?= 99.99

javac:
	clojure -T:build javac

javac-test:
	clojure -T:build javac :with-tests true

test: javac-test
	clojure -M:$(CLOJURE_VERSION):dev:test$(EXTRA_ALIASES)

eastwood: javac-test
	clojure -M:eastwood:+junixsocket

cljfmt:
	clojure -M:cljfmt check

kondo:
	clojure -M:kondo

KONDO_CLR_DIR := $(or $(TMPDIR),/tmp)/nrepl-kondo-clr
kondo-clr:
	rm -rf $(KONDO_CLR_DIR) && mkdir -p $(KONDO_CLR_DIR)
	cp -r src $(KONDO_CLR_DIR)/src
	find $(KONDO_CLR_DIR)/src -name '*.cljc' -exec sed -i 's/:cljr\b/:cljs/g' {} +
	clojure -M:kondo-clr --lint $(KONDO_CLR_DIR)/src --config-dir .clj-kondo --config .clj-kondo/clr.edn

cloverage:
	clojure -M:cloverage

docs:
	clojure -X:docs :file '"doc/modules/ROOT/pages/ops.adoc"'

# Regenerate ops.adoc and fail if it differs from the committed version.
# This catches cases where middleware descriptors changed but the generated
# docs weren't regenerated (or ops.adoc was edited by hand).
verify-docs: docs
	@if ! git diff --quiet -- doc/modules/ROOT/pages/ops.adoc; then \
		echo "[Error] ops.adoc is out of sync with the middleware descriptors."; \
		echo "Run 'make docs' and commit the result."; \
		git --no-pager diff -- doc/modules/ROOT/pages/ops.adoc; \
		exit 1; \
	fi

lint: kondo cljfmt eastwood

# Usage: make install (or PROJECT_VERSION=1.2.3 make install)
install:
	clojure -T:build install :version '"$(PROJECT_VERSION)"'

clean:
	clojure -T:build clean

verify-cljdoc:
	clojure -T:build verify-cljdoc
