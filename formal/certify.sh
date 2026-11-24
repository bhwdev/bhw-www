#!/bin/sh
# SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
# SPDX-License-Identifier: AGPL-3.0-or-later

# Certify the ACL2 books: the definitions SBCL runs (src/acl2/), then the
# proofs about them (formal/).  Each in an ACL2 process of its own, as a
# failed CERTIFY-BOOK leaves a session in a state that breaks the next one,
# and with its output in BOOK.out.  ACL2 exits 0 even after a failure, so a
# book is certified when its .cert was written anew and its output names no
# failure.  ACL2 is $ACL2, else acl2 on the PATH.
#
#   formal/certify.sh            every book
#   formal/certify.sh BOOK...    those, as src/acl2/pitch or formal/pitch

set -eu
acl2=${ACL2:-acl2}
cd "$(dirname "$0")/.."

if [ $# -eq 0 ]; then
    set -- src/acl2/slides src/acl2/deck src/acl2/pitch src/acl2/windows \
           src/acl2/reminder \
           formal/slides formal/deck formal/pitch formal/windows formal/reminder
fi

for book in "$@"; do
    rm -f "$book.cert"
    printf '(ld "src/acl2/package.lsp")\n(certify-book "%s" ?)\n' "$book" \
        | "$acl2" > "$book.out" 2>&1
    if [ ! -f "$book.cert" ] || grep -q 'FAILED\|ACL2 Error' "$book.out"; then
        echo "$book: not certified; see $book.out" >&2
        exit 1
    fi
    echo "$book: certified"
done
