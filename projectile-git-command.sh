#!/usr/bin/env bash
set -eu
set -o pipefail

# Files should be \0 separated

list-project-files() {
    git ls-files -zco --exclude-standard
}

list-interesting-erlang-deps() {
    local interesting="deps/rabbit_common deps/amqp_client deps/rabbit deps/rabbitmq_test"
    for dir in $interesting; do
        if [[ -d $dir && -e $dir/.git ]]; then
            git -C "$dir" ls-files -zco --exclude-standard | DIR=$dir perl -E 'local $/ = undef; $data = <>; @files = split /\0/, $data; print join(qq{\0}, map { $ENV{DIR} . "/" . $_} @files)'
        fi
    done
}

list-project-files
list-interesting-erlang-deps
