#!/usr/bin/env bash
pod=$1;shift;kubectl exec -c app -i $pod -- "$@"
