# vim: set ts=4:

readonly PKG_NAME='sh-parser'
readonly TEMP_DIR="$(pwd)/.tmp"
readonly VENV_DIR="$(pwd)/.venv"

ask() {
	local msg="$1"
	local default="$2"

	printf '\n%s [%s] ' "$msg" "$default"
	local answer; read answer

	case "${answer:="$default"}" in
		y | yes) return 0;;
		*) return 1;;
	esac
}

einfo() {
	# bold cyan
	printf '\033[1;36m> %s\033[0m\n' "$@" >&2
}

ewarn() {
	# bold yellow
	printf '\033[1;33m> %s\033[0m\n' "$@" >&2
}

die() {
	# bold red
	printf '\033[1;31mERROR:\033[0m %s\n' "$1" >&2
	shift
	printf '  %s\n' "$@"
	exit 2
}

# Fetches the given URL and verifies SHA256 checksum.
wgets() {(
	local url="$1"
	local sha256="$2"
	local dest="${3:-.}"

	mkdir -p "$dest" \
		&& cd "$dest" \
		&& rm -f "${url##*/}" \
		&& wget -T 10 "$url" \
		&& echo "$sha256  ${url##*/}" | sha256sum -c
)}
