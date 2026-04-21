FROM public.ecr.aws/amazonlinux/amazonlinux:2023

RUN dnf -y update \
   && dnf -y install \
   R-4.3.2 \
   R-devel-4.3.2 \
   python3.13 \
   python3.13-devel \
   python3.13-pip \
   httpd \
   httpd-devel \
   && dnf clean all

RUN ln -sf /usr/bin/pip3.13 /usr/bin/pip3 \
   && pip3 install --upgrade pip setuptools wheel

RUN mkdir -p /app /app/tmp

# Copy and install Python dependencies
COPY apc/requirements.txt /app/requirements.txt
RUN pip3 install -r /app/requirements.txt

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
