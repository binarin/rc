#!/bin/bash
set -e
ROOT=$(dirname $(readlink -f $0))
cd $ROOT
export FACTER_machine_role=binarin::role::demandred
puppet apply --parser=future --fileserverconfig=$ROOT/fileserver.conf --verbose --modulepath=$ROOT/modules --hiera_config=$ROOT/hiera.yaml $ROOT/site.pp
