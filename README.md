# ek9

[![CI](https://github.com/Boenan/ek9/actions/workflows/ci.yml/badge.svg)](https://github.com/Boenan/ek9/actions/workflows/ci.yml)

ek9 lets you work with Kubernetes clusters from Emacs, as k9s does in a terminal.
ek9 runs `kubectl` in the background, so Emacs does not stop while it waits.
Lists read through one `kubectl proxy` for each context, on a Unix socket that only you can open.
Thus, a list refresh is fast, and the lists show the same columns as `kubectl get`.

## Requirements

- Emacs 30.2 or later.
- `kubectl` in your `PATH`. The types after the common ones (see below) need a `kubectl` that supports `kubectl api-resources -o json`.

## Installation

Use latest:
```elisp
(use-package ek9
  :vc (:url "https://github.com/Boenan/ek9" :rev :newest))
```

Or with pin version:
```elisp
(use-package ek9
  :vc (:url "https://github.com/Boenan/ek9" :rev "<version>"))
```


## Usage

Run `M-x ek9`. ek9 opens the pod list of your current kubectl context.
ek9 does not change the current context in your kubeconfig.

Push `?` in a list to see all keys.

A list updates itself in the background. It draws the rows again only when the text or the colors change. While the mark is active (for example after `C-SPC`), the list does not update, so you can select and copy text. The list shows the updates when the mark goes off.

| Key | Action |
|---|---|
| `:` | Select a resource type, for example `deploy`, `pods argocd`, or `pods -A`. A saved view name also works. `C-u :` loads the types of the cluster again. |
| `RET` / `^` | Go down a level / go back up |
| `n` / `c` | Select a namespace / a context |
| `v` | Open a saved view |
| `/` | Filter the rows. `-l REGEXP` filters by labels, `-a REGEXP` by annotations (see below). |
| `w` | Show or hide the wide columns of `kubectl get -o wide`. They show by default (option `ek9-list-wide`). |
| `d` / `y` | Describe / show the YAML |
| `E` | Show the events of the item |
| `l` / `s` | Show the logs / open a shell |
| `e` | Edit the YAML. Save the buffer to apply it. |
| `f` / `F` | Start a port-forward / show all port-forwards |
| `S` / `R` / `D` | Scale / restart / delete |
| `x` | Decode a secret |
| `Q` | Kill all ek9 buffers |

## Logs

Push `l` on a pod, a container, a deployment, a statefulset, a daemonset, or a job.
For a workload, ek9 shows the logs of all pods that match its label selector.
ek9 sorts the lines of all pods by their timestamps. New lines show after a short delay (option `ek9-logs-sort-delay`, 0.1 seconds).
If there is more than one container, ek9 asks for a container.
The choice `<all containers>` shows the logs of all containers.

| Key | Action |
|---|---|
| `s` | Stop the stream, or start it again |
| `g` | Remove the lines and start again |
| `t` | Show or hide the timestamps |
| `p` | Show or hide the pod names (workload logs only, off by default) |
| `P` | Show the logs of the previous container instance |
| `c` | Select another container |
| `q` | Quit |

Each pod name has its own color. The color comes from the pod name, so a pod keeps its color when you toggle `t` or `p`, or open the logs again.
ek9 makes 50 colors that fit the background (option `ek9-logs-name-color-count`).
The option `ek9-logs-name-colors` sets your own list of colors.
`p` changes all lines immediately. The stream does not start again.
With `<all containers>`, each line always starts with the container name in color.
The option `ek9-logs-max-requests` sets the maximum number of streams (one for each container of each pod).

## All resource types

`:` offers all types of the current context that you can list.
These are the common types, for example pods and deployments, the other built-in types, for example `ingresses` or `pvc`, and the CRD types, for example `certificates.cert-manager.io`.
Each list shows the same columns as `kubectl get -o wide`.
Push `w` to hide the wide columns.

The common types also have drill-down (`RET`), status colors, and actions such as logs or scale.
The other types have describe, YAML, events, and, if the API permits, edit and delete.

A built-in type uses its plural as its name, for example `ingresses`.
If that name is not free, use the full name, for example `events.events.k8s.io`.
A CRD type uses its full name, or a short name of the CRD.
If two types have the same short name, only the full name works.
ek9 loads the types one time for each context.
After you install a new CRD, push `C-u :` to load the types again.

## Filter

Push `/` to show only some rows of a list.
The match ignores case.
An empty filter shows all rows.

| Filter | Shows the rows where |
|---|---|
| `web` | a column contains the text `web` |
| `-l REGEXP` | a label `KEY=VALUE` matches REGEXP, for example `-l app=web` or `-l ^team=frontend$` |
| `-a REGEXP` | an annotation `KEY=VALUE` matches REGEXP, for example `-a ^owner=` |

A saved view keeps the filter.

## Live updates

A visible list reads the full list one time.
Then a watch shows each change at once: a new, changed, or deleted object.
ek9 calculates the AGE column itself, so the age does not stop.
Every 30 seconds, ek9 reads the full list again (option `ek9-watch-reload-interval`).
This updates other times that the server calculates, for example the last restart in RESTARTS.

A hidden list stops its watch.
If the watch fails three times, the list reads the full list every 3 seconds (option `ek9-refresh-interval`), and the tab line shows "Watch failed".
Push `g` to try the watch again.
Set `ek9-watch` to nil to always read the full list every 3 seconds.

## CPU and memory

The pod and node lists show the CPU and memory use, as `kubectl top` does.
The cluster must have the metrics API, for example from metrics-server.
Without it, the lists show no CPU and memory columns.
ek9 reads the metrics every 15 seconds (option `ek9-metrics-interval`).
Set `ek9-metrics` to nil to remove the columns.

| Column | Value |
|---|---|
| `CPU` | CPU use in millicores, for example `250m` |
| `MEM` | Memory use in MiB, for example `64Mi` |
| `CPU/R`, `MEM/R` | Pods: the use as a percentage of the requests. A wide column. |
| `CPU/L`, `MEM/L` | Pods: the use as a percentage of the limits. A wide column. |
| `CPU/A`, `MEM/A` | Nodes: the use as a percentage of the allocatable resources |

A pod without requests or limits shows `n/a` in these columns.

## Saved views

A saved view opens a list that you use often in its own buffer.
`M-x ek9-saved-view-save` saves the current list as a view.
The view also keeps the wide columns setting (`w`) of the list.
Use `M-x customize-option RET ek9-saved-views` to change the views.
Do not use `setq` for this option, because ek9 saves the views with Customize.

## Safety

- ek9 asks before it deletes, scales, restarts, or applies a change.
- Add a context to `ek9-read-only-contexts` to block all changes in that context.

## Options

Use `M-x customize-group RET ek9`.

## License

ek9 is free software under the GNU General Public License, version 3 or later.
See [LICENSE](LICENSE).
