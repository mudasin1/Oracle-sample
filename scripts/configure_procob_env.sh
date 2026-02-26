#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
ENV_FILE="$REPO_ROOT/.procob.env"

MODE="auto"
ORACLE_HOME_HINT="${PROCOB_ORACLE_HOME:-}"
DOCKER_IMAGE="${PROCOB_DOCKER_IMAGE:-procob-runner:ol9}"
DOCKER_PLATFORM="${PROCOB_DOCKER_PLATFORM:-linux/amd64}"
FORCE=0

usage() {
  cat <<'EOF'
Usage: scripts/configure_procob_env.sh [options]

Options:
  --mode <auto|local|docker>   Preferred runtime mode (default: auto)
  --oracle-home <path>         Oracle home containing bin/procob (Linux build for docker mode)
  --docker-image <name>        Docker image tag for runner (default: procob-runner:ol9)
  --docker-platform <value>    Docker platform (default: linux/amd64)
  --env-file <path>            Output env file (default: .procob.env in repo root)
  --force                      Overwrite existing env file
  -h, --help                   Show this help
EOF
}

log() {
  printf '%s\n' "$*"
}

error() {
  printf 'ERROR: %s\n' "$*" >&2
}

is_elf_binary() {
  local bin_path="$1"
  file "$bin_path" 2>/dev/null | grep -q 'ELF'
}

can_run_local_procob() {
  local procob_cmd="$1"
  "$procob_cmd" ? >/dev/null 2>&1
}

detect_local_procob() {
  local candidate

  if [[ -n "${PROCOB_CMD:-}" && -x "${PROCOB_CMD:-}" ]]; then
    printf '%s\n' "$PROCOB_CMD"
    return 0
  fi

  candidate="$(command -v procob 2>/dev/null || true)"
  if [[ -n "$candidate" && -x "$candidate" ]]; then
    printf '%s\n' "$candidate"
    return 0
  fi

  if [[ -n "${ORACLE_HOME:-}" && -x "${ORACLE_HOME}/bin/procob" ]]; then
    printf '%s\n' "${ORACLE_HOME}/bin/procob"
    return 0
  fi

  return 1
}

docker_ready() {
  command -v docker >/dev/null 2>&1 && docker info >/dev/null 2>&1
}

validate_oracle_home_for_docker() {
  local oracle_home="$1"
  [[ -n "$oracle_home" && -d "$oracle_home" && -x "$oracle_home/bin/procob" ]] || return 1
  is_elf_binary "$oracle_home/bin/procob"
}

find_linux_oracle_home() {
  local candidate=""

  if [[ -n "$ORACLE_HOME_HINT" && -d "$ORACLE_HOME_HINT" ]]; then
    if validate_oracle_home_for_docker "$ORACLE_HOME_HINT"; then
      printf '%s\n' "$ORACLE_HOME_HINT"
      return 0
    fi
  fi

  for candidate in "${PROCOB_ORACLE_HOME:-}" "${ORACLE_HOME:-}" "$HOME/Oracle/fmw-output/app/fmw"; do
    [[ -n "$candidate" ]] || continue
    if validate_oracle_home_for_docker "$candidate"; then
      printf '%s\n' "$candidate"
      return 0
    fi
  done

  if [[ -d "$HOME/Oracle" ]]; then
    candidate="$(find "$HOME/Oracle" -maxdepth 6 -type f -path '*/bin/procob' 2>/dev/null | head -n 1 || true)"
    if [[ -n "$candidate" ]]; then
      candidate="${candidate%/bin/procob}"
      if validate_oracle_home_for_docker "$candidate"; then
        printf '%s\n' "$candidate"
        return 0
      fi
    fi
  fi

  return 1
}

build_runner_image() {
  local dockerfile="$REPO_ROOT/scripts/Dockerfile.procob-runner"

  [[ -f "$dockerfile" ]] || {
    error "Docker runner definition not found: $dockerfile"
    return 1
  }

  log "Building Docker runner image: $DOCKER_IMAGE ($DOCKER_PLATFORM)"
  docker build --platform "$DOCKER_PLATFORM" -f "$dockerfile" -t "$DOCKER_IMAGE" "$REPO_ROOT" >/dev/null
}

validate_docker_procob() {
  local oracle_home="$1"

  docker run --rm --platform "$DOCKER_PLATFORM" \
    -v "$oracle_home:/opt/oracle/fmw:ro" \
    "$DOCKER_IMAGE" bash -lc '
      set -euo pipefail
      export ORACLE_HOME=/opt/oracle/fmw
      export PATH="$ORACLE_HOME/bin:$PATH"
      export LD_LIBRARY_PATH="$ORACLE_HOME/lib:${LD_LIBRARY_PATH:-}"
      procob ? >/dev/null 2>&1
    '
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --mode)
      MODE="$2"
      shift 2
      ;;
    --oracle-home)
      ORACLE_HOME_HINT="$2"
      shift 2
      ;;
    --docker-image)
      DOCKER_IMAGE="$2"
      shift 2
      ;;
    --docker-platform)
      DOCKER_PLATFORM="$2"
      shift 2
      ;;
    --env-file)
      ENV_FILE="$2"
      shift 2
      ;;
    --force)
      FORCE=1
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      error "Unknown option: $1"
      usage
      exit 1
      ;;
  esac
done

case "$MODE" in
  auto|local|docker) ;;
  *)
    error "Invalid --mode value: $MODE"
    exit 1
    ;;
esac

if [[ -f "$ENV_FILE" && "$FORCE" -ne 1 ]]; then
  error "Env file already exists: $ENV_FILE (use --force to overwrite)"
  exit 1
fi

resolved_mode=""
resolved_procob_cmd=""
resolved_oracle_home=""

local_procob="$(detect_local_procob || true)"
local_procob_ok=0
if [[ -n "$local_procob" ]] && can_run_local_procob "$local_procob"; then
  local_procob_ok=1
fi

if [[ "$MODE" == "local" || "$MODE" == "auto" ]]; then
  if [[ "$local_procob_ok" -eq 1 ]]; then
    resolved_mode="local"
    resolved_procob_cmd="$local_procob"
  elif [[ "$MODE" == "local" ]]; then
    error "Local procob was requested but no runnable local procob was found."
    exit 1
  fi
fi

if [[ -z "$resolved_mode" ]]; then
  if [[ "$MODE" != "docker" && "$MODE" != "auto" ]]; then
    error "Unable to resolve procob mode."
    exit 1
  fi

  docker_ready || {
    error "Docker is not available/running, required for docker mode."
    exit 1
  }

  resolved_oracle_home="$(find_linux_oracle_home || true)"
  if [[ -z "$resolved_oracle_home" ]]; then
    error "Could not find a Linux Oracle home containing bin/procob for docker mode."
    error "Pass --oracle-home <path> to set it explicitly."
    exit 1
  fi

  build_runner_image

  log "Validating dockerized procob..."
  validate_docker_procob "$resolved_oracle_home"

  resolved_mode="docker"
fi

mkdir -p "$(dirname "$ENV_FILE")"
{
  printf '# Generated by %s on %s\n' "$(basename "$0")" "$(date -u '+%Y-%m-%dT%H:%M:%SZ')"
  printf '# Load with: source %q\n\n' "${ENV_FILE#$REPO_ROOT/}"
  printf 'export PROCOB_MODE=%q\n' "$resolved_mode"
  if [[ "$resolved_mode" == "local" ]]; then
    printf 'export PROCOB_CMD=%q\n' "$resolved_procob_cmd"
  else
    printf 'export PROCOB_ORACLE_HOME=%q\n' "$resolved_oracle_home"
    printf 'export PROCOB_DOCKER_IMAGE=%q\n' "$DOCKER_IMAGE"
    printf 'export PROCOB_DOCKER_PLATFORM=%q\n' "$DOCKER_PLATFORM"
  fi
  printf 'export PROCOB_FORMAT=%q\n' "terminal"
  printf 'export PROCOB_SQLCHECK=%q\n' "full"
  printf 'export PROCOB_IRECLEN=%q\n' "132"
  printf 'export PROCOB_ORECLEN=%q\n' "132"
} > "$ENV_FILE"

log "Wrote configuration: $ENV_FILE"
if [[ "$resolved_mode" == "local" ]]; then
  log "Configured local procob: $resolved_procob_cmd"
else
  log "Configured docker procob using Oracle home: $resolved_oracle_home"
fi
log "Next: source ${ENV_FILE#$REPO_ROOT/} && ./scripts/procob_flow.sh"
