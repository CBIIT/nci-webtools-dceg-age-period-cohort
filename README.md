### Web tools for Age-Period-Cohort Analysis


#### [Age Period Cohort (APC) Web Tool](https://analysistools.nci.nih.gov/apc/)

Age-Period-Cohort analysis identifies patterns in cancer incidence or mortality rates from population-based Count (numerator) and Population (denominator) data. Often the data come from a Cancer Registry (e.g., SEER) in the form of a table showing the numbers of cancer cases or cancer deaths (counts) and corresponding person-years at risk (population) for particular age groups and calendar time periods. This toolset provides a comprehensive solution to age-period-cohort analysis for cancer endpoints in defined populations and time periods.


#### [Comparative Age Period Cohort (CrossTalk) Web Tool](https://analysistools.nci.nih.gov/)


---

## CI/CD Process

### Overview

This project uses GitHub Actions to automate the deployment of the APC web tools to AWS ECS Fargate. The CI/CD pipeline is manually triggered via workflow dispatch and supports deployments to multiple environments.

### Workflow Trigger

The deployment workflow (`.github/workflows/apc-fargate-deploy.yml`) is triggered manually via the GitHub Actions UI with the following options:

- **Tier Selection**: Choose the target environment (dev, qa, stage, prod)

### Environment Mapping

The CI/CD pipeline supports four deployment tiers with the following configurations:

| Tier | Environment | Image Tag | AWS Account | Description |
|------|------------|-----------|-------------|-------------|
| `dev` | Development | `development-backend-*` | Configured via secrets | Development environment for testing new features |
| `qa` | QA | `development-backend-*` | Configured via secrets | Quality assurance testing environment |
| `stage` | Staging | `release-backend-*` | Configured via secrets | Pre-production staging environment |
| `prod` | Production | `release-backend-*` | Configured via secrets | Production environment |

**Image Tier Logic**:
- `dev` and `qa` tiers use `development` tagged images
- `stage` and `prod` tiers use `release` tagged images

### Deployment Process

The deployment workflow performs the following steps:

1. **Authentication**
   - Assumes AWS IAM role using OIDC
   - Session name includes tier, app name, and branch reference

2. **Build Configuration**
   - Sets environment variables including:
     - Branch name and Git tag
     - Timestamp for versioning
     - ECR repository URLs
     - Backend image tags (versioned and latest)

3. **AWS Parameter Retrieval**
   - Fetches deployment configuration from AWS Systems Manager Parameter Store:
     - ECS cluster name
     - Task definition name
     - ECS service name
     - Task execution and task role ARN

4. **Docker Build**
   - Uses Docker Buildx for multi-platform builds
   - Builds backend image from `./apc/docker/backend.dockerfile`
   - Tags images with both versioned tag and `latest` tag
   - Pushes to Amazon ECR with build cache optimization

5. **Task Definition**
   - Renders ECS task definition from template (`.github/aws/web.yml`)
   - Substitutes environment-specific variables using `envsubst`
   - Registers new task definition revision with AWS ECS

6. **Service Deployment**
   - Updates ECS service with new task definition
   - Forces new deployment to ensure latest image is used
   - Sets desired count to 1
   - Propagates tags from task definition

### Infrastructure Components

**ECS Fargate Configuration**:
- **CPU Units**: 2048 (2 vCPU)
- **Memory**: 4096 MB (4 GB)
- **Network Mode**: `awsvpc`
- **Backend Container Port**: 80

**Containers**:
1. **Backend Container**
   - Image: Latest versioned backend image from ECR
   - Environment: Production Flask environment
   - Logging: AWS FireLens with Datadog integration

2. **Logs Container**
   - Image: AWS public fluent-bit image
   - Purpose: Log routing via FireLens

### Required AWS Resources

Each tier requires the following AWS resources to be pre-configured:

**AWS Systems Manager Parameters**:
- ECS cluster configuration
- Task definition settings
- Service configuration
- IAM role ARNs

**Logging Integration**:
- CloudWatch log groups per tier
- Datadog configuration (endpoint and API key stored as SecureString parameters)

**IAM Roles**:
- GitHub Actions CICD role with OIDC trust relationship
- ECS task execution role with ECR and CloudWatch permissions
- ECS task role with application-specific permissions

**Amazon ECR**:
- Container image repository in us-east-1 region

### Deployment Commands

To deploy to a specific environment:

1. Navigate to the **Actions** tab in GitHub
2. Select the **APC Fargate Deploy** workflow
3. Click **Run workflow**
4. Select the desired tier (dev/qa/stage/prod)
5. Click **Run workflow** button

### Monitoring

- **CloudWatch Logs**: `/analysistools/{tier}/apc/web`
- **Datadog**: Logs tagTier-specific log groups
- **Datadog**: Logs tagged with project and tier informationloyments

