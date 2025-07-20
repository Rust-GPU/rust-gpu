#!/bin/bash
set -euo pipefail

# Check if rust-toolchain.toml exists
if [ ! -f "rust-toolchain.toml" ]; then
    echo "::error::rust-toolchain.toml not found"
    exit 1
fi

# Read current channel from rust-toolchain.toml
CURRENT_CHANNEL=$(grep '^channel = ' rust-toolchain.toml | head -1 | sed 's/channel = "\(.*\)"/\1/')
CURRENT_DATE=$(echo "$CURRENT_CHANNEL" | sed 's/nightly-//')

echo "Current toolchain: $CURRENT_CHANNEL"

# Fetch latest nightly manifest
MANIFEST_TOML_URL="https://static.rust-lang.org/dist/channel-rust-nightly.toml"
MANIFEST_CONTENT=$(curl -sf "$MANIFEST_TOML_URL") || {
    echo "::error::Failed to fetch manifest from $MANIFEST_TOML_URL"
    exit 1
}

LATEST_DATE=$(echo "$MANIFEST_CONTENT" | grep '^date = ' | head -1 | sed 's/date = "\(.*\)"/\1/')

if [ -z "$LATEST_DATE" ]; then
    echo "::error::Could not extract date from manifest"
    exit 1
fi

echo "Latest nightly date: $LATEST_DATE"

# Convert dates to comparable format (remove dashes for numeric comparison)
CURRENT_DATE_NUM=$(echo "$CURRENT_DATE" | tr -d '-')
LATEST_DATE_NUM=$(echo "$LATEST_DATE" | tr -d '-')

# Compare dates numerically
if [ "$LATEST_DATE_NUM" -gt "$CURRENT_DATE_NUM" ]; then
    echo "::notice::New nightly available: $LATEST_DATE (current: $CURRENT_DATE)"
    
    # Get commit hash from the already fetched manifest
    COMMIT_HASH=$(echo "$MANIFEST_CONTENT" | grep -A 50 '\[pkg.rust\]' | grep 'git_commit_hash = ' | head -1 | sed 's/git_commit_hash = "\(.*\)"/\1/')
    
    if [ -z "$COMMIT_HASH" ]; then
        echo "::error::Could not extract commit hash from manifest"
        exit 1
    fi
    
    echo "New commit hash: $COMMIT_HASH"
    
    # Write outputs for GitHub Actions
    echo "SHOULD_UPGRADE=true" >> "${GITHUB_OUTPUT:-/dev/stdout}"
    echo "NEW_DATE=$LATEST_DATE" >> "${GITHUB_OUTPUT:-/dev/stdout}"
    echo "NEW_COMMIT_HASH=$COMMIT_HASH" >> "${GITHUB_OUTPUT:-/dev/stdout}"
else
    echo "::notice::No newer nightly available (current: $CURRENT_DATE, latest: $LATEST_DATE)"
    echo "SHOULD_UPGRADE=false" >> "${GITHUB_OUTPUT:-/dev/stdout}"
fi