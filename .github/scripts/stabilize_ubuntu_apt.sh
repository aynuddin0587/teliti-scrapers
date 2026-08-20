#!/usr/bin/env bash
set -euo pipefail

# Teliti GitHub Actions network hardening for Ubuntu-hosted runners.
# GitHub runner images use /etc/apt/apt-mirrors.txt with the Azure Ubuntu
# mirror first.  A transient Azure mirror failure can otherwise keep apt-get
# retrying for a very long time during r-lib/actions/setup-r.

if [[ -f /etc/apt/apt-mirrors.txt ]]; then
  echo "Original /etc/apt/apt-mirrors.txt:"
  cat /etc/apt/apt-mirrors.txt

  {
    printf 'https://archive.ubuntu.com/ubuntu/\tpriority:1\n'
    printf 'https://security.ubuntu.com/ubuntu/\tpriority:2\n'
    printf 'http://azure.archive.ubuntu.com/ubuntu/\tpriority:3\n'
  } | sudo tee /etc/apt/apt-mirrors.txt >/dev/null

  echo
  echo "Teliti /etc/apt/apt-mirrors.txt:"
  cat /etc/apt/apt-mirrors.txt
else
  echo "WARNING: /etc/apt/apt-mirrors.txt not found; mirror order unchanged."
fi

# Override the runner image's generous retry behavior with bounded network
# retries/timeouts suitable for scheduled environmental collectors.
sudo tee /etc/apt/apt.conf.d/99teliti-network >/dev/null <<'APTCONF'
Acquire::Retries "2";
Acquire::http::Timeout "20";
Acquire::https::Timeout "20";
APTCONF

echo
echo "Teliti apt network limits:"
cat /etc/apt/apt.conf.d/99teliti-network