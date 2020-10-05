#!/bin/sh

# this bash script is translated from lisp function
# 'lsp-java--ensure-server in lsp-java.el

#JDT_DOWNLOAD_URL=https://download.eclipse.org/jdtls/milestones/0.62.0/jdt-language-server-0.62.0-202009291815.tar.gz
JDT_DOWNLOAD_URL=http://127.0.0.1:8300/jdtls/milestones/0.62.0/jdt-language-server-0.62.0-202009291815.tar.gz

LSP_JAVA_SERVER_INSTALL_DIR=~/.emacs.d/.cache/lsp/eclipse.jdt.ls

if ! mvn -version; then
    exit 1
fi

sed -e '/github.com/ { s/https/http/; s/github.com/127.0.0.1:8301/; }' pom.xml.orig > pom.xml

mvn -Djdt.js.server.root="$LSP_JAVA_SERVER_INSTALL_DIR" \
    -Djunit.runner.root="$LSP_JAVA_SERVER_INSTALL_DIR/test-runner/" \
    -Djava.debug.root="$LSP_JAVA_SERVER_INSTALL_DIR/bundles" \
    -Djdt.download.url="$JDT_DOWNLOAD_URL" \
    -Djunit.runner.fileName="junit-platform-console-standalone.jar" \
    clean package
