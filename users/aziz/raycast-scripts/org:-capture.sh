#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Org: capture
# @raycast.mode compact

# Optional parameters:
# @raycast.icon 🤖

# Documentation:
# @raycast.description Run org capture
# @raycast.author abdalazizrashid
# @raycast.authorURL https://raycast.com/abdalaziz_rashid


emacsclient "org-protocol://capture"
