#!/usr/bin/env python

# use socks5 proxy:
#
# $ env https_proxy=socks5://127.0.0.1:1089 python download.py

import sys,os
import subprocess

urls_to_download = [
    ("https", "raw.githubusercontent.com", "emacs-lsp/lsp-java/master/install/pom.xml"),
    ("https", "download.eclipse.org"     , "jdtls/milestones/0.62.0/jdt-language-server-0.62.0-202009291815.tar.gz"),
    ("https", "github.com"               , "emacs-lsp/lsp-java/releases/download/2.2/Pivotal.vscode-spring-boot-1.6.0.vsix"),
    ("https", "github.com"               , "emacs-lsp/lsp-java/releases/download/2.2/dgileadi.java-decompiler-0.0.2.vsix"),
    ("https", "github.com"               , "emacs-lsp/lsp-java/releases/download/2.2/vscjava.vscode-java-dependency-0.5.1.vsix"),
    ("https", "github.com"               , "emacs-lsp/lsp-java/releases/download/3.1/vscjava.vscode-java-test-0.24.1.vsix"),
]

for url in urls_to_download:
    dest_file = os.path.join('downloads', url[1], url[2])

    dest_file_parent_dir = os.path.dirname(dest_file)
    if not os.path.exists(dest_file_parent_dir):
       os.makedirs(dest_file_parent_dir)

    print "downloading", url
    subprocess.check_call(['curl', '-L', '%s://%s/%s' % url, '-o', dest_file])
