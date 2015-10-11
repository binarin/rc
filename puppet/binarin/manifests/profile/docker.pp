class binarin::profile::docker {
  ensure_packages(["docker.io"], {ensure => latest})
  Package['docker.io']
  -> file { "/etc/default/docker":
    content => 'DOCKER_OPTS="--exec-opt native.cgroupdriver=cgroupfs"
'
  }
  ~> service { docker:
    ensure => running
  }
}
