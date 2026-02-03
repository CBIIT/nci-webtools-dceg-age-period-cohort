FROM public.ecr.aws/amazonlinux/amazonlinux:2023

RUN dnf -y update \
 && dnf -y install \
    gcc \
    gcc-c++ \
    gcc-gfortran \
    glibc-langpack-en \
    httpd-devel \
    libffi-devel \
    make \
    openssl-devel \
    python3 \
    python3-devel \
    python3-pip \
    python3-setuptools \
    python3-wheel \
    R \
    libcurl-devel \
    libxml2-devel \
    pcre2-devel \
    xz-devel \
    bzip2-devel \
    zlib-devel \
    libicu-devel \
 && dnf clean all

# Install R packages
RUN R -e "install.packages(c('jsonlite', 'MASS'), repos='https://cloud.r-project.org/')"

# Create application directory
RUN mkdir -p /app

# Copy and install Python dependencies
COPY requirements.txt /app/requirements.txt
RUN pip3 install -r /app/requirements.txt

# Copy application files
COPY apc.py apc.wsgi apcWrapper.R apc.R /app/
COPY index.html help.html apc.css apc.js analytics.js /app/
COPY images/ /app/images/
COPY lib/ /app/lib/
COPY example_data/ /app/example_data/

# Create tmp directory for output files
RUN mkdir -p /app/tmp

# Create fontconfig cache directory for R graphics
RUN mkdir -p /var/cache/fontconfig && chmod 777 /var/cache/fontconfig

# Set proper permissions
RUN chown -R apache:apache /app

# Set fontconfig cache directory
ENV FONTCONFIG_PATH=/etc/fonts
ENV FONTCONFIG_FILE=/etc/fonts/fonts.conf
ENV FC_CACHEDIR=/var/cache/fontconfig

CMD mod_wsgi-express start-server /app/apc.wsgi \
    --user apache \
    --group apache \
    --port 80 \
    --processes 4 \
    --threads 1 \
    --max-clients 3000 \
    --socket-timeout 900 \
    --queue-timeout 900 \
    --shutdown-timeout 900 \
    --graceful-timeout 900 \
    --connect-timeout 900 \
    --request-timeout 900 \
    --keep-alive-timeout 60 \
    --compress-responses \
    --log-to-terminal \
    --access-log \
    --access-log-format "%h %{X-Forwarded-For}i %l %u %t \"%r\" %>s %b \"%{Referer}i\" \"%{User-Agent}i\"" combined \
    --mount-point /apc \
    --document-root /app \
    --working-directory /app
