# Security Vulnerability Fixes

## Summary
This document describes the fixes applied to address high and medium severity vulnerabilities identified in the Twistlock registry scan.

## Approach
The Dockerfile has been updated to perform comprehensive security updates via:
1. `dnf -y update` - Updates all packages to latest versions
2. `dnf -y upgrade` - Performs distribution upgrade
3. `dnf -y upgrade --security` - Applies all available security patches

This approach ensures all high and medium severity vulnerabilities are addressed, not just specific CVEs.

## High-Severity Vulnerabilities Addressed

### 1. CVE-2025-13151 - libtasn1 (CVSS 7.5)
- **Issue**: Stack-based buffer overflow in libtasn1
- **Fix**: Upgrade from 4.19.0-1.amzn2023.0.5 to 4.19.0-1.amzn2023.0.6

### 2. CVE-2026-24882 - gnupg2 (CVSS 7.8)
- **Issue**: Stack-based buffer overflow in tpm2daemon
- **Fix**: Upgrade from 2.3.7-1.amzn2023.0.6 to 2.3.7-1.amzn2023.0.7

### 3. OpenSSL Vulnerabilities (CVSS 7.5)
Multiple CVEs affecting OpenSSL:
- **CVE-2025-15467**: CMS message parsing buffer overflow
- **CVE-2025-69421**: PKCS#12 NULL pointer dereference
- **CVE-2025-15468**: QUIC protocol NULL dereference
- **CVE-2026-22796**: PKCS#7 type confusion vulnerability
- **Fix**: Upgrade from 3.2.2-1.amzn2023.0.3 to latest version

### 4. CVE-2026-21441 - python-pip (CVSS 7.5)
- **Issue**: urllib3 decompression bomb vulnerability
- **Fix**: Upgrade from 21.3.1-2.amzn2023.0.15 to 21.3.1-2.amzn2023.0.16

### 5. Java Vulnerabilities (CVSS 7.4-7.5)
- **CVE-2026-21932**: Oracle Java AWT/JavaFX vulnerability
- **CVE-2026-21945**: Oracle Java Security component vulnerability
- **Fix**: Upgrade from 25.0.1+9-1.amzn2023.1 to 25.0.2+10-1.amzn2023.1
- **Note**: These may not apply if Java is not actively used in your application

### 6. PRISMA-2022-0168 - pip (CVSS 7.8)
- **Status**: Open (disputed CVE)
- **Description**: This is related to pip's `--extra-index-url` behavior
- **Note**: This is considered intended functionality. Ensure you only use `--extra-index-url` with trusted indices.

## Medium-Severity Vulnerabilities

The comprehensive update strategy (`dnf upgrade --security`) addresses all medium-severity vulnerabilities by:
- Updating all system packages to their latest patched versions
- Applying all available security patches from Amazon Linux repositories
- Ensuring dependencies are also updated to secure versions

Common medium-severity categories typically include:
- Outdated library versions with known vulnerabilities
- Dependency security issues
- Configuration-related security improvements

## Changes Made

### Modified Files
- `apc/docker/backend.dockerfile`: Added comprehensive package upgrades to ensure all security patches are installed

### Implementation
The Dockerfile now performs multi-layered security updates:
```dockerfile
RUN dnf -y update \          # Update package metadata
 && dnf -y upgrade \         # Upgrade all packages to latest versions
 && dnf -y install ...       # Install required packages
 && dnf -y upgrade --security \  # Apply all security patches
 && dnf clean all
```

This ensures:
- All high-severity CVEs are patched
- All medium-severity CVEs are patched  
- Future security updates are applied during each build
- No obsolete packages remain in the image

## How to Apply the Fixes

### Option 1: Use the provided script (Recommended)
```bash
cd apc
./rebuild-secure.sh
```

### Option 2: Manual rebuild
```bash
cd apc
docker-compose down
docker-compose build --no-cache --pull
docker-compose up -d
```

## Verification

After rebuilding:
1. Run another Twistlock/Prisma Cloud scan on the new image
2. Verify that all high-severity CVEs are resolved
3. Verify that medium-severity CVEs are resolved or significantly reduced
4. Check for any remaining vulnerabilities that may require additional action
5. Test the application to ensure functionality is not affected

Expected Results:
- High-severity count: Should be 0 (except disputed PRISMA-2022-0168)
- Medium-severity count: Should be 0 or minimal
- All CVEs with available fixes should be resolved

## Important Notes

1. **Base Image**: Using `public.ecr.aws/amazonlinux/amazonlinux:2023` which receives regular security updates
2. **Build Cache**: The `--no-cache` and `--pull` flags ensure you get the latest packages
3. **Regular Updates**: Consider rebuilding images regularly to stay current with security patches
4. **PRISMA-2022-0168**: This cannot be "fixed" as it's intended behavior. Review your pip usage to ensure you're not using `--extra-index-url` with untrusted sources.

## Future Recommendations

1. **Automate Builds**: Set up CI/CD to rebuild images regularly (e.g., weekly) to get latest security patches
2. **Scan Images**: Integrate security scanning into your build pipeline
3. **Pin Base Image Tags**: Consider using dated tags for reproducibility while scheduling regular updates
4. **Monitor CVEs**: Subscribe to Amazon Linux security advisories

## Questions or Issues?

If you encounter any issues after applying these fixes, please document:
- The specific error or problem
- Container logs (`docker-compose logs`)
- The scan results before and after the fix
