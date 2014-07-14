hostnamecolor=$(hostname | od | tr ' ' '\n' | awk '{total = total + $1}END{print 30 + (total % 7) + 1}')
# if root, color username red
	if [[ ${EUID} == 0 ]] ; then
		PS1='\[\033[01;31m\]\u@\[\e[${hostnamecolor}m\]\]\h\[\033[01;34m\] \w \$\[\033[00m\] '
	else
		PS1='\[\033[01;32m\]\u@\[\e[${hostnamecolor}m\]\h\[\033[01;34m\] \w \$\[\033[00m\] '
	fi
