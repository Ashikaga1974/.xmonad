#!/bin/bash

checkupdatesshort(){
    #echo "huhu"
    max = yum check-update | awk 'p;/^$/{p=1}' | grep -c "\."
    echo max
}

checkupdatesshort

#echo yum check-update | awk 'p;/^$/{p=1}' | grep -c "\."