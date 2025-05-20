#!/bin/bash

URL=${1:-https://example.com}
LOGFILE="url_check_$(date +%F).log"

STATUS_CODE=$(curl -s -o /dev/null -w "%{http_code}" $URL)

echo "$(date) - Checking $URL - Status code: $STATUS_CODE" | tee -a "$LOGFILE"

if [ "$STATUS_CODE" -ne 200 ]; then
  echo "🚨 ALERT: $URL is down or returned $STATUS_CODE!" | tee -a "$LOGFILE"
  # Optionally send an alert (email, Slack, webhook)
fi
