#!/bin/sh

set -e

add () {
    case $1 in
        "super-major")
            awk -v FS=. -vOFS=. '{ $1 = $1 + '"$2"'; $2 = 0; $3 = 0; $4 = 0; print $0 }'
            ;;
        "major")
            awk -v FS=. -vOFS=. '{ $2 = $2 + '"$2"'; $3 = 0; $4 = 0; print $0 }'
            ;;
        "minor")
            awk -v FS=. -vOFS=. '{ $3 = $3 + '"$2"'; $4 = 0; print $0 }'
            ;;
        "patch")
            awk -v FS=. -vOFS=. '{ $4 = $4 + '"$2"'; print $0 }'
            ;;
        *)
            echo "Invalid bump level" >&2
            exit 1
            ;;
    esac
}

bump () {
    case ${1} in
        "dev")
            case $2 in
                "super-major") add major 1;;
                "major")       add major 1;;
                "minor")       cat;;
                "patch")       cat;;
            esac
            ;;

        *)
            case $2 in
                "super-major") add $2 1;;
                "major")       add $2 2;;
                "minor")       add $2 1;;
                "patch")       add $2 1;;
            esac
    esac
}

major () {
    sed -r 's/^([0-9]+\.[0-9]+).*$/\1/'
}

add_changelog () {
     emacs --batch \
           --eval "(setq user-full-name \"$DEBFULLNAME\")" \
           --eval "(setq user-mail-address \"$DEBEMAIL\")" \
           --eval '(add-change-log-entry nil "ChangeLog" nil t)' \
           --eval "(insert \"Release version $VERSION\")" \
           -f 'save-buffer'
}

if [ -z "$1" ]; then
    echo "Usage: $0 (super-major | major | minor | patch) " >&2
    exit 1
fi

LEVEL=$1

TCURR="$(git tag | grep "^v" | sort -V | tail -n1)"

if [ -n "$TCURR" ]; then
    CURR=$(echo $TCURR | sed 's/^v\(.*\)$/\1/')
else
    CURR=0.-2.0.0
fi

VERSION=$(echo "$CURR" | bump ver $LEVEL)
DEV_VERSION=$(echo "$VERSION" | bump dev $LEVEL)

echo "CURR \t\t $CURR"
echo "VERSION \t $VERSION"
echo "DEV_VERSION \t $DEV_VERSION"

if ! echo $VERSION | grep -Eq '^[0-9]+\.[0-9]*[02468][0-9.]*(-.+)?$'; then
    echo "invalid version";
    exit 1
fi

cd $(dirname $0)/..

git reset

[ -e ChangeLog.tmp ] && mv ChangeLog.tmp 'ChangeLog.tmp~'

#if ! grep -q "$(date '+%Y-%m-%d')" ChangeLog; then

#    ( printf "$(date '+%Y-%m-%d')  $DEBFULLNAME  <$DEBEMAIL>\n\n        * Release version $VERSION\n\n";  ) > ChangeLog.tmp

#    emacs -q -nw ChangeLog.tmp
#fi

if [ x"$LEVEL" = x"super-major" ] || [ x"$LEVEL" = x"major" ]; then
    git checkout -b "rts-loader-$(echo $VERSION | major)" master || true
fi

git checkout "rts-loader-$(echo $VERSION | major)"

add_changelog
git commit -m "ChangeLog for $VERSION on release branch" ChangeLog

sed -r -i 's/^(version:[[:space:]]*)[0-9.]+/\1'"$VERSION"'/' rts-loader.cabal
git add rts-loader.cabal

git commit -m "Release version $VERSION"

#TODO: remove -f
git tag -f "v$VERSION"

cabal sdist


git checkout master

add_changelog
git commit -m "ChangeLog for $VERSION on master" ChangeLog

if [ x"$LEVEL" = x"super-major" ] || [ x"$LEVEL" = x"major" ]; then
    sed -r -i 's/^(version:[[:space:]]*)[0-9.]+/\1'"$DEV_VERSION"'/' rts-loader.cabal
    git add rts-loader.cabal
    git commit -m "Bump version to $DEV_VERSION"
fi
