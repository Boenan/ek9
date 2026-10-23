#!/bin/sh
# Run the ek9 integration tests against a local kind cluster.
#
# The cluster is "ek9-itest".  Its kubeconfig is test/integration/kubeconfig,
# so the script does not change ~/.kube/config.
#
# Usage: test/integration/run.sh [--delete]
#   --delete  Delete the kind cluster after the tests.

set -eu

dir=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$dir/../.." && pwd)
cluster=ek9-itest
export KUBECONFIG="$dir/kubeconfig"
context="kind-$cluster"

if ! kind get clusters 2>/dev/null | grep -qx "$cluster"; then
    kind create cluster --name "$cluster" --kubeconfig "$KUBECONFIG" --wait 120s
fi
# The kubeconfig file can be gone, for example after "git clean".
kind export kubeconfig --name "$cluster" --kubeconfig "$KUBECONFIG"

k() { kubectl --context "$context" "$@"; }

k apply -f "$dir/manifests.yaml"
# The objects of a CRD can only be applied after the CRD is established.
k apply -f "$dir/crds.yaml"
k wait --for=condition=established --timeout=60s \
  crd/widgets.ek9.example.com crd/gizmos.ek9.example.com crd/sprockets.ek9.example.com
k apply -f "$dir/custom-resources.yaml"
k -n ek9-itest get job tick-manual >/dev/null 2>&1 ||
    k -n ek9-itest create job tick-manual --from=cronjob/tick

k -n ek9-itest rollout status deployment/web --timeout=180s
k -n ek9-itest rollout status statefulset/db --timeout=180s
k -n ek9-itest rollout status daemonset/agent --timeout=180s
k -n ek9-itest wait --for=condition=complete job/tick-manual --timeout=180s

# Wait until the crasher pod restarted at least one time.
i=0
restarts() {
    k -n ek9-itest get pod crasher \
      -o jsonpath='{.status.containerStatuses[0].restartCount}'
}
until [ "$(restarts)" -ge 1 ] 2>/dev/null; do
    i=$((i + 1))
    [ "$i" -lt 60 ] || { echo "crasher did not restart" >&2; exit 1; }
    sleep 2
done

# metrics-server gives the CPU and memory columns.  The kubelet of
# kind has a self-signed certificate, so metrics-server needs
# --kubelet-insecure-tls.  This is only for the local test cluster.
metrics_url=https://github.com/kubernetes-sigs/metrics-server/releases/download/v0.7.2/components.yaml
curl -fsSL "$metrics_url" |
    sed 's/^\( *\)- --metric-resolution=15s$/&\n\1- --kubelet-insecure-tls/' |
    k apply -f - >/dev/null
k -n kube-system rollout status deployment/metrics-server --timeout=180s
i=0
until k top nodes >/dev/null 2>&1 && k -n ek9-itest top pods >/dev/null 2>&1; do
    i=$((i + 1))
    [ "$i" -lt 60 ] || { echo "metrics-server has no metrics" >&2; exit 1; }
    sleep 2
done

status=0
# A package-vc rebuild can leave old .elc files.  Remove them, as
# CLAUDE.md asks.
(cd "$root" && eask clean elc >/dev/null)
(cd "$root" && eask test ert test/integration/ek9-itest.el) || status=$?
(cd "$root" && eask clean elc >/dev/null)

if [ "${1:-}" = "--delete" ]; then
    kind delete cluster --name "$cluster" --kubeconfig "$KUBECONFIG"
fi

exit "$status"
