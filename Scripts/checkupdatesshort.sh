#!/bin/bash
# Get number of available updates
max=$(yum check-update | awk 'p;/^$/{p=1}' | grep -c "\.")

echo $max

exit 0