FROM public.ecr.aws/amazonlinux/amazonlinux:2023

# --releasever=latest: AL2023 repos are locked to the base image's release, so a
# plain "dnf update" does not pick up security fixes published after that release.
RUN dnf -y --releasever=latest update \
   && dnf -y --releasever=latest install \
   R-4.3.2 \
   R-devel-4.3.2 \
   python3.13 \
   python3.13-devel \
   python3.13-pip \
   httpd \
   httpd-devel \
   && dnf clean all

# Restrict Python 3.9 to root only
RUN chmod 700 /usr/bin/python3.9

# Upgrade pip/setuptools/wheel into /usr/local, then drop the outdated OS copy of
# pip so only the current version remains.
RUN python3.13 -m pip install --upgrade pip setuptools wheel \
   && dnf -y remove python3.13-pip \
   && dnf clean all

RUN mkdir -p /app /app/tmp

# Copy and install Python dependencies
COPY apc/requirements.txt /app/requirements.txt
# pip is only needed at build time; remove it afterwards so it is not shipped
# in the runtime image.
RUN python3.13 -m pip install -r /app/requirements.txt \
   && python3.13 -m pip uninstall -y pip

# Install R packages
RUN R -e 'options(repos = c(CRAN = sprintf("https://packagemanager.posit.co/cran/latest/bin/linux/rhel9-%s/%s", R.version["arch"], substr(getRversion(), 1, 3)))); \
   install.packages(c("jsonlite", "MASS"))'

COPY apc /app

RUN chown -R apache:apache /app

CMD mod_wsgi-express start-server /app/apc.wsgi \
   --user apache \
   --group apache \
   --port 80 \
   --mount-point /apc \
   --processes 1 \
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
   --document-root /app \
   --working-directory /app
