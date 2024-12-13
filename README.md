# Git Save All

Report, and optionally push, branches that are behind a remote

## Motivation

I have a `~/code/{org}/{repo}` directory tree that I **don't** back up, since
everything there is git-tracked. When I migrate to a new work machine, I want to
be sure that I don't accidentally leave local changes behind.

This is tedious to do by hand, so I wrote this tool:

```console
% git-save-all */* --quiet
WARNING: /home/patrick/code/freckle/aeson-compat/ could not fetch origin
✗ freckle/hs-opentelemetry@main needs to be pushed
WARNING: /home/patrick/code/iterative/iterative.ai/ could not fetch origin
✗ jfstephe/aws-s3-lock@pb/first needs to be pushed
✗ jfstephe/aws-s3-lock@pb/git-save-all-wip needs to be pushed
✗ josephsumabat/static-ls@pb/git-save-all-wip needs to be pushed
✗ kazu-yamamoto/logger@pb/repro needs to be pushed
! Maskuh/status@main needs to be force-pushed
WARNING: /home/patrick/code/pbrisbin/amazonka-s3-sync/ could not fetch origin
WARNING: /home/patrick/code/pbrisbin/expenses/ could not fetch origin
WARNING: /home/patrick/code/pbrisbin/git-save-all/ could not fetch origin
WARNING: /home/patrick/code/pbrisbin/jmap/ could not fetch origin
WARNING: /home/patrick/code/pbrisbin/one-dictionary/ could not fetch origin
✗ pbrisbin/opt-env-conf@master needs to be pushed
WARNING: /home/patrick/code/pbrisbin/stack-tools/ could not fetch origin
✗ renaissance-learning/wg-soc2@pb/data-driven-findings needs to be pushed
! renaissance-learning/wg-soc2@pb/discussins needs to be force-pushed
✗ renaissance-learning/wg-soc2@pb/finalize needs to be pushed
✗ renaissance-learning/wg-soc2@pb/findings needs to be pushed
! RenaissancePlace/element-PIEItemAR@main needs to be force-pushed
✗ xmonad/xmonad-contrib@pb/desktop-viewport needs to be pushed
✗ yesodweb/clientsession@pb/crypton needs to be pushed
✗ yesodweb/clientsession@pb/git-save-all-wip needs to be pushed
```

If satisfied, I can add `--push` to have those `✗` entries automatically pushed.
The warnings and force-pushes are left to be dealt with manually.

Lastly, if I remove `--quiet` all of the branches that _are_ in sync will also
be reported.

## Usage

```console
% git-save-all --help
Usage: git-save-all [-r|--remote-name REMOTE] [-x|--exclude BRANCH] [-p|--push] 
                    [-q|--quiet] [DIRECTORY]

  Report, and optionally push, branches that are behind a remote

Available options:
  -r,--remote-name REMOTE  The name of the remote to use (default: "origin")
  -x,--exclude BRANCH      Branches to exclude from processing
  -p,--push                Actually push branches found to be behind
  -q,--quiet               Don't report in-sync branches
  DIRECTORY                Git repository to operate on
  -h,--help                Show this help text
```

## LICENSE

AGPLv3. See [COPYING](./COPYING).
