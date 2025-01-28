#! /usr/bin/env nix-shell
#! nix-shell -i bash -p coreutils unzip rsync mcrcon temurin-bin
set -e

# Defaults
REPLACE="false"

# Define the options
OPTIONS=$(getopt -o r --long replace,discord -- "$@")

# Exit if getopt fails
if [ $? -ne 0 ]; then
    echo "Failed to parse options."
    exit 1
fi

# Set the positional parameters to the parsed options
eval set -- "$OPTIONS"

# Parse the options
while true; do
    case "$1" in
        -r)
            REPLACE="true"
            shift
            ;;
        --replace)
            REPLACE="true"
            shift
            ;;
        --)
            shift
            break
            ;;
        *)
            echo "Unexpected option: $1"
            exit 1
            ;;
    esac
done


logfile=$(mktemp)
exec 3<> "$logfile"

echo "!!! Check $logfile for any errors"

SERVER_FILES=$(ls -1t ServerFiles*.zip Server-Files*.zip 2>/dev/null | head -n1)

echo ">>> $SERVER_FILES discovered. Extracting"
unzip -o $SERVER_FILES >&3 2>&1
pushd "${SERVER_FILES%.*}" &> /dev/null

installer=$(ls -1 neoforge-*-installer.jar)
neoforge_version=$(echo $installer | sed -e "s/neoforge-\([0-9]\+\.[0-9]\+\.[0-9]\+\)-installer.jar/\1/")

echo ">>> Running neoforge installer ($installer)"
java -jar $installer -installServer >&3 2>&1

popd &> /dev/null

echo ">>> Generating new start script"
cat <<EOF > start.sh
#!/usr/bin/env sh
# Forge requires a configured set of both JVM and program arguments.
# Add custom JVM arguments to the user_jvm_args.txt
# Add custom program arguments {such as nogui} to this file in the next line before the "\$@" or
#  pass them to this script directly
java @user_jvm_args.txt @libraries/net/neoforged/neoforge/${neoforge_version}/unix_args.txt "\$@"
EOF
chmod 755 start.sh

echo ">>> New release prepped!"

if [ "$REPLACE" == "true" ]
then
  echo "!!! Server replacement specified. Hope you made a backup! 😉"
  sleep 2

  echo ">>> Stopping the server (sudo mode)"
  sudo systemctl stop mc-atm10 >&3 2>&1

  pushd "${SERVER_FILES%.*}" &> /dev/null
  echo ">>> Putting discord mod and config in place"
  cp ~/atm10/dcintegration-neoforge-3.0.7-1.21.jar mods
  cp ~/atm10/Discord-Integration.toml config
  echo ">>> Putting new modpack in place (sudo mode)"
  sudo rsync -avhP --delete-after defaultconfigs kubejs libraries mods packmenu /var/lib/mc-atm10/ >&3 2>&1
  sudo rsync -avhP config /var/lib/mc-atm10/ >&3 2>&1
  sudo cp ~/atm10/start.sh /var/lib/mc-atm10 >&3 2>&1
  sudo chown -R mc-atm10:mc-atm10 /var/lib/mc-atm10
  popd &> /dev/null

  echo ">>> Starting the server (sudo mode)"
  sudo systemctl start mc-atm10 >&3 2>&1

fi

exec 3>&-
echo "Please see $logfile for additional output"

