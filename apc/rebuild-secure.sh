#!/bin/bash

# Script to rebuild Docker image with security updates
# This addresses high and medium severity CVEs found in the Twistlock scan

echo "Rebuilding APC Docker image with security updates..."
echo "This will address both HIGH and MEDIUM severity vulnerabilities including:"
echo "  - CVE-2025-13151 (libtasn1)"
echo "  - CVE-2026-24882 (gnupg2)"
echo "  - CVE-2025-15467, CVE-2025-69421, CVE-2025-15468, CVE-2026-22796 (openssl)"
echo "  - CVE-2026-21441 (python-pip)"
echo "  - All other package vulnerabilities via comprehensive system update"
echo ""

# Stop existing container
echo "Stopping existing container..."
docker-compose down

# Remove old image to force rebuild
echo "Removing old image..."
docker-compose rm -f

# Rebuild with no cache to ensure latest packages
echo "Building new image with latest security patches..."
docker-compose build --no-cache --pull

# Start the container
echo "Starting container..."
docker-compose up -d

echo ""
echo "Rebuild complete!"
echo "Please run another Twistlock scan to verify the vulnerabilities are resolved."
echo ""
echo "Note: PRISMA-2022-0168 (pip --extra-index-url) is a disputed CVE"
echo "      and represents intended behavior, not a traditional vulnerability."
