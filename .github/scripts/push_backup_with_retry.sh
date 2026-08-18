#!/usr/bin/env bash

set -euo pipefail

REPO_DIR="${1:?Repository directory is required}"
PATHSPEC="${2:?Git pathspec is required}"
COMMIT_MESSAGE="${3:?Commit message is required}"

if [ ! -d "${REPO_DIR}/.git" ]; then
  echo "ERROR: Not a Git repository: ${REPO_DIR}"
  exit 1
fi

cd "${REPO_DIR}"

git config user.name "teliti-github-actions"
git config user.email "teliti-github-actions@users.noreply.github.com"

echo ""
echo "Staging backup path:"
echo "  ${PATHSPEC}"

git add -- "${PATHSPEC}"

if git diff --cached --quiet; then
  echo "No backup changes to commit."
  exit 0
fi

echo ""
echo "Files to be committed:"
git diff --cached --name-status

git commit -m "${COMMIT_MESSAGE}"

echo ""
echo "Synchronizing with latest origin/main before push."

for attempt in 1 2 3 4 5
do
  echo ""
  echo "Push attempt ${attempt}/5"

  git fetch origin main

  if ! git rebase origin/main
  then
    echo ""
    echo "ERROR: Rebase conflict detected."
    git status
    git rebase --abort || true
    exit 1
  fi

  if git push origin HEAD:main
  then
    echo ""
    echo "Backup push succeeded."
    exit 0
  fi

  echo ""
  echo "Remote changed again before this push completed."
  echo "Fetching newest main and retrying."

  sleep $((attempt * 5))
done

echo ""
echo "ERROR: Could not push backup after 5 attempts."
exit 1