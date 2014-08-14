if [ -z $1 ]; then
    echo "Usage: ./setup.sh <bashrc.XXXXX>"
    exit -1
fi 

if [ ! -f $1 ]; then
    echo "Error: $1 not exists."
    exit -1
fi

PWD=`pwd`

ln -sf $PWD/bashrc_common ~/.bashrc_common
ln -sf $PWD/$1 ~/.bashrc
ln -sf $PWD/bash_profile ~/.bash_profile
