# Docker Setup Guide

This guide explains how to run the APC web tool using Docker.

## Prerequisites

- Docker Desktop or Docker Engine installed
- Docker Compose (usually included with Docker Desktop)

## Quick Start

From the `apc` directory, run:

```bash
cd apc
docker-compose up --build
```

The APC Tool will be available at: **http://localhost:10000**

## Development Mode

The docker-compose configuration mounts local directories as volumes, so changes to your code will be reflected in the containers. However, you may need to restart the services:

```bash
docker-compose restart
```

## Production Deployment

For production, modify the docker-compose.yml:

1. Remove volume mounts
2. Set `FLASK_ENV=production`
3. Change `restart: unless-stopped` to `restart: always`
4. Consider using a reverse proxy (nginx) in front

### Production Build Example

```bash
# Build production image
docker build -f apc/docker/backend.dockerfile -t apc-tool:latest ./apc

# Run without volumes
docker run -d -p 10000:80 --name apc-tool apc-tool:latest
```

## Useful Commands

```bash
# Stop the service
docker-compose down

# View logs
docker-compose logs -f

# Rebuild without cache
docker-compose build --no-cache

# Remove containers and volumes
docker-compose down -v
```

## Troubleshooting

### Port Already in Use

If you get a port conflict error:

```bash
# Change ports in docker-compose.yml
# For example, change "10000:80" to "10001:80"
```

### Permission Issues

If you encounter permission issues with mounted volumes:

```bash
# On Linux/Mac, ensure proper permissions
chmod -R 755 .
```

### R Package Issues

If R packages fail to install, you may need to update the Dockerfile with additional system dependencies or specific package versions.

## Container Architecture

- **Base Image**: public.ecr.aws/amazonlinux/amazonlinux:2023 (Amazon Linux 2023)
- **Web Server**: Apache with mod_wsgi-express
- **Python**: 3.x (from DNF package manager)
- **R**: Installed via DNF with required development libraries
- **Key Dependencies**: Flask, rpy2, mod_wsgi, R packages (jsonlite, MASS)
- **Working Directory**: /app
- **Runtime User**: apache
- **Port**: 80 (mapped to host port 10000)

## Custom Configuration

To modify R packages or Python dependencies, edit:
- APC Backend: `apc/docker/backend.dockerfile`
- Python packages: `apc/requirements.txt` (flask, rpy2, mod_wsgi)