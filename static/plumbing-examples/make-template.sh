
mkdir $1
touch $1/info.conf
touch $1/main.occ
touch $1/README.txt

cat << 'EOF' > $1/info.conf
name: ...
tweet: ...
category: Plumbing
author: Matt Jadud
email: matt@jadud.com
author-url: http://jadud.com/
path: .../main.occ
EOF
