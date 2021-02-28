FROM alpine:3.12

# Install native deps needed for Tezos (etc?)
# Adapted from https://github.com/asbjornenge/tezos-docker
RUN apk update && apk upgrade && apk --no-cache add \
  build-base snappy-dev alpine-sdk \
  bash ncurses-dev xz m4 git pkgconfig \
  gmp-dev libev-dev libressl-dev linux-headers pcre-dev perl zlib-dev hidapi-dev \
  libffi-dev \
  opam cargo

RUN opam init --disable-sandboxing --bare
RUN opam update

# make bls12-381 build ???
ENV RUSTFLAGS='--codegen target-feature=-crt-static'

# Install opam switch & deps
WORKDIR /ligo
COPY scripts/setup_switch.sh /ligo/scripts/setup_switch.sh
RUN sh scripts/setup_switch.sh
COPY scripts/install_opam_deps.sh /ligo/scripts/install_opam_deps.sh
COPY ligo.opam /ligo
COPY ligo.opam.locked /ligo
# copy all vendor .opams... this lets us install all transitive deps,
# but devs can change vendored code without invalidating the cache
COPY vendors/ParserLib/ParserLib.opam /ligo/vendors/ParserLib/ParserLib.opam
COPY vendors/Red-Black_Trees/RedBlackTrees.opam /ligo/vendors/Red-Black_Trees/RedBlackTrees.opam
COPY vendors/UnionFind/UnionFind.opam /ligo/vendors/UnionFind/UnionFind.opam
COPY vendors/Preprocessor/Preprocessor.opam /ligo/vendors/Preprocessor/Preprocessor.opam
COPY vendors/Michelson/Michelson.opam /ligo/vendors/Michelson/Michelson.opam
COPY vendors/LexerLib/LexerLib.opam /ligo/vendors/LexerLib/LexerLib.opam
COPY vendors/ligo-utils/proto-alpha-utils/proto-alpha-utils.opam /ligo/vendors/ligo-utils/proto-alpha-utils/proto-alpha-utils.opam
COPY vendors/ligo-utils/tezos-utils/tezos-utils.opam /ligo/vendors/ligo-utils/tezos-utils/tezos-utils.opam
COPY vendors/ligo-utils/memory-proto-alpha/tezos-memory-proto-alpha.opam /ligo/vendors/ligo-utils/memory-proto-alpha/tezos-memory-proto-alpha.opam
COPY vendors/ligo-utils/simple-utils/simple-utils.opam /ligo/vendors/ligo-utils/simple-utils/simple-utils.opam
RUN sh scripts/install_opam_deps.sh

# Now install vendor libs
COPY vendors /ligo/vendors
COPY scripts/install_vendors_deps.sh /ligo/scripts/install_vendors_deps.sh
COPY ligo.opam /ligo
COPY ligo.opam.locked /ligo
WORKDIR /ligo
RUN sh scripts/install_vendors_deps.sh

# Install LIGO
COPY src /ligo/src
COPY dune-project /ligo/dune-project
COPY scripts/version.sh /ligo/scripts/version.sh
WORKDIR /ligo
# Version info and changelog
ARG ci_commit_tag
ARG ci_commit_sha
ARG ci_commit_timestamp
ENV CI_COMMIT_TAG=$ci_commit_tag
ENV CI_COMMIT_SHA=$ci_commit_sha
ENV CI_COMMIT_TIMESTAMP=$ci_commit_timestamp
COPY changelog.txt /ligo/changelog.txt
ENV CHANGELOG_PATH=/ligo/changelog.txt
RUN eval $(opam env) && LIGO_VERSION=$(/ligo/scripts/version.sh) dune build -p ligo --profile static

# Run tests
COPY gitlab-pages /ligo/gitlab-pages
RUN BISECT_ENABLE=yes opam exec -- dune runtest --profile static --no-buffer
RUN LIGO_FORCE_NEW_TYPER=true opam exec -- dune runtest --force --profile static --no-buffer

# Coverage (only the overall)
RUN opam exec -- bisect-ppx-report html -o coverage --title="LIGO test coverage"
RUN opam exec -- bisect-ppx-report summary --per-file > coverage/coverage-summary
# echo "Test coverage:"
# BISECT_ENABLE=yes dune runtest src/test --force
# bisect-ppx-report html -o $out/share/coverage/ligo --title="LIGO test coverage"
# echo "Doc coverage:"
# BISECT_ENABLE=yes dune build @doc-test --force
# bisect-ppx-report html -o $out/share/coverage/docs --title="LIGO doc coverage"
# echo "CLI test coverage:"
# BISECT_ENABLE=yes dune runtest src/bin/expect_tests
# bisect-ppx-report html -o $out/share/coverage/cli --title="CLI test coverage"

# Run doc
RUN opam exec -- dune build @doc

# TODO see also ligo-docker-large in nix build
FROM alpine:3.12
WORKDIR /root/
COPY --from=0 /ligo/_build/install/default/bin/ligo /root/ligo
COPY --from=0 /ligo/_build/default/_doc/_html /root/doc
COPY --from=0 /ligo/coverage /root/coverage
ENTRYPOINT ["/root/ligo"]