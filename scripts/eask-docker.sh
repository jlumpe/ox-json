#!/usr/bin/env bash
# Run eask inside silex/emacs Docker with the project bind-mounted.
# Eask in the image lives under /root (not traversable as non-root), so we run
# as root and chown the project tree to the caller's UID/GID afterward.

set -euo pipefail

version=${1:?usage: eask-docker.sh VERSION eask-args...}
shift

uid=$(id -u)
gid=$(id -g)
project_dir=$(pwd)
container_dir="/$(basename "$project_dir")"

if [[ $version =~ ^[0-9]+(\.[0-9]+)*(-[a-zA-Z0-9.]+)?(\+[a-zA-Z0-9.]+)?$ ]]; then
  image="silex/emacs:${version}-eask"
else
  image=$version
fi

exec docker run --rm \
  -v "${project_dir}:${container_dir}" \
  -w "${container_dir}" \
  "$image" \
  sh -c 'eask "$@"; ec=$?; chown -R '"${uid}:${gid}"' . 2>/dev/null || true; exit $ec' \
  _ "$@"
