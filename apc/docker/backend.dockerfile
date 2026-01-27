FROM rocker/r-ver:4.2.0

# Install system dependencies
RUN apt-get update && apt-get install -y \
    python3 \
    python3-pip \
    python3-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpcre2-dev \
    liblzma-dev \
    libbz2-dev \
    zlib1g-dev \
    libicu-dev \
    gcc \
    g++ \
    gfortran \
    make \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('jsonlite', 'MASS'), repos='https://cloud.r-project.org/')"

# Install Python packages
RUN pip3 install --no-cache-dir \
    flask \
    rpy2

# Set working directory
WORKDIR /app

# Copy application files
COPY apc.py apc.wsgi apcWrapper.R apc.R ./
COPY index.html help.html apc.css apc.js analytics.js ./
COPY images/ ./images/
COPY lib/ ./lib/
COPY example_data/ ./example_data/

# Create tmp directory for output files
RUN mkdir -p tmp

# Expose port
EXPOSE 10000

# Set environment variables
ENV FLASK_APP=apc.py
ENV PYTHONUNBUFFERED=1

# Run the application
CMD ["python3", "apc.py"]
