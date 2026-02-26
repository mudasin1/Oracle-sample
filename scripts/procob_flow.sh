#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
ENV_FILE="$REPO_ROOT/.procob.env"

if [[ -f "$ENV_FILE" ]]; then
  # shellcheck source=/dev/null
  . "$ENV_FILE"
fi

SOURCE_DIR="$REPO_ROOT/SampleFiles"
MODE="${PROCOB_MODE:-auto}"
COPY_STEP=1
PRECOMPILE_STEP=1
OVERWRITE=1
DRY_RUN=0
COPY_EXT=".cbl"
PRECOMPILE_EXT=".procob.cbl"
PROCOB_CMD_OVERRIDE="${PROCOB_CMD:-}"
ORACLE_HOME_HINT="${PROCOB_ORACLE_HOME:-${ORACLE_HOME:-}}"
DOCKER_IMAGE="${PROCOB_DOCKER_IMAGE:-procob-runner:ol9}"
DOCKER_PLATFORM="${PROCOB_DOCKER_PLATFORM:-linux/amd64}"
FORMAT="${PROCOB_FORMAT:-terminal}"
SQLCHECK="${PROCOB_SQLCHECK:-full}"
IRECLEN="${PROCOB_IRECLEN:-132}"
ORECLEN="${PROCOB_ORECLEN:-132}"

usage() {
  cat <<'EOF'
Usage: scripts/procob_flow.sh [options]

Default behavior:
  1) Copy each .pco to .cbl (preserve originals)
  2) Precompile each .pco to .procob.cbl via procob

Options:
  --source-dir <path>              Directory to scan for .pco files (default: SampleFiles)
  --mode <auto|local|docker>       Procob runtime mode (default: from .procob.env or auto)
  --copy-only                      Only create .cbl copies
  --precompile-only                Only run procob precompile outputs
  --copy-ext <suffix>              Copy output suffix (default: .cbl)
  --precompile-ext <suffix>        Precompile output suffix (default: .procob.cbl)
  --procob-cmd <path>              Override local procob command path
  --oracle-home <path>             Oracle home for docker mode (contains bin/procob)
  --docker-image <name>            Docker image tag (default: procob-runner:ol9)
  --docker-platform <platform>     Docker platform (default: linux/amd64)
  --format <ansi|terminal|variable> Input format passed to procob (default: terminal)
  --sqlcheck <level>               sqlcheck value (default: full)
  --ireclen <n>                    Input record length (default: 132)
  --oreclen <n>                    Output record length (default: 132)
  --no-overwrite                   Do not overwrite existing generated files
  --dry-run                        Print actions without writing files
  -h, --help                       Show this help
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

detect_local_procob() {
  local candidate

  if [[ -n "$PROCOB_CMD_OVERRIDE" && -x "$PROCOB_CMD_OVERRIDE" ]]; then
    printf '%s\n' "$PROCOB_CMD_OVERRIDE"
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

can_run_local_procob() {
  local procob_cmd="$1"
  "$procob_cmd" ? >/dev/null 2>&1
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

  for candidate in "$ORACLE_HOME_HINT" "${PROCOB_ORACLE_HOME:-}" "${ORACLE_HOME:-}" "$HOME/Oracle/fmw-output/app/fmw"; do
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

ensure_runner_image() {
  local dockerfile="$REPO_ROOT/scripts/Dockerfile.procob-runner"

  if docker image inspect "$DOCKER_IMAGE" >/dev/null 2>&1; then
    return 0
  fi

  [[ -f "$dockerfile" ]] || {
    error "Docker runner definition not found: $dockerfile"
    return 1
  }

  log "Building Docker runner image $DOCKER_IMAGE ($DOCKER_PLATFORM)..."
  docker build --platform "$DOCKER_PLATFORM" -f "$dockerfile" -t "$DOCKER_IMAGE" "$REPO_ROOT" >/dev/null
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --source-dir)
      SOURCE_DIR="$2"
      shift 2
      ;;
    --mode)
      MODE="$2"
      shift 2
      ;;
    --copy-only)
      COPY_STEP=1
      PRECOMPILE_STEP=0
      shift
      ;;
    --precompile-only)
      COPY_STEP=0
      PRECOMPILE_STEP=1
      shift
      ;;
    --copy-ext)
      COPY_EXT="$2"
      shift 2
      ;;
    --precompile-ext)
      PRECOMPILE_EXT="$2"
      shift 2
      ;;
    --procob-cmd)
      PROCOB_CMD_OVERRIDE="$2"
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
    --format)
      FORMAT="$2"
      shift 2
      ;;
    --sqlcheck)
      SQLCHECK="$2"
      shift 2
      ;;
    --ireclen)
      IRECLEN="$2"
      shift 2
      ;;
    --oreclen)
      ORECLEN="$2"
      shift 2
      ;;
    --no-overwrite)
      OVERWRITE=0
      shift
      ;;
    --dry-run)
      DRY_RUN=1
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

SOURCE_DIR="$(cd "$SOURCE_DIR" && pwd)"

if [[ "$COPY_STEP" -eq 0 && "$PRECOMPILE_STEP" -eq 0 ]]; then
  error "Nothing to do. Use default mode, --copy-only, or --precompile-only."
  exit 1
fi

if [[ "$COPY_STEP" -eq 1 && "$PRECOMPILE_STEP" -eq 1 && "$COPY_EXT" == "$PRECOMPILE_EXT" ]]; then
  log "WARNING: copy and precompile suffix are identical; precompile will overwrite copied outputs."
fi

PCO_FILES=()
while IFS= read -r file_path; do
  PCO_FILES+=("$file_path")
done < <(find "$SOURCE_DIR" -type f -name '*.pco' | LC_ALL=C sort)

if [[ "${#PCO_FILES[@]}" -eq 0 ]]; then
  error "No .pco files found under $SOURCE_DIR"
  exit 1
fi

copied_count=0
copied_skipped=0
precompiled_count=0
precompiled_skipped=0

if [[ "$COPY_STEP" -eq 1 ]]; then
  for pco in "${PCO_FILES[@]}"; do
    copy_out="${pco%.pco}${COPY_EXT}"

    if [[ "$OVERWRITE" -eq 0 && -f "$copy_out" ]]; then
      log "SKIP copy (exists): ${copy_out#$REPO_ROOT/}"
      copied_skipped=$((copied_skipped + 1))
      continue
    fi

    if [[ "$DRY_RUN" -eq 1 ]]; then
      log "DRYRUN copy: ${pco#$REPO_ROOT/} -> ${copy_out#$REPO_ROOT/}"
    else
      cp -p "$pco" "$copy_out"
      log "Copied: ${pco#$REPO_ROOT/} -> ${copy_out#$REPO_ROOT/}"
    fi
    copied_count=$((copied_count + 1))
  done
fi

if [[ "$PRECOMPILE_STEP" -eq 1 ]]; then
  runner=""
  procob_cmd=""
  docker_oracle_home=""

  local_procob="$(detect_local_procob || true)"
  local_procob_ok=0
  if [[ -n "$local_procob" ]] && can_run_local_procob "$local_procob"; then
    local_procob_ok=1
  fi

  case "$MODE" in
    local)
      if [[ "$local_procob_ok" -ne 1 ]]; then
        error "Local procob mode selected, but no runnable local procob was found."
        exit 1
      fi
      runner="local"
      procob_cmd="$local_procob"
      ;;
    docker)
      docker_ready || {
        error "Docker mode selected, but Docker is not available/running."
        exit 1
      }
      docker_oracle_home="$(find_linux_oracle_home || true)"
      if [[ -z "$docker_oracle_home" ]]; then
        error "Docker mode selected, but no Linux Oracle home with bin/procob was found."
        error "Use --oracle-home <path> or run ./scripts/configure_procob_env.sh first."
        exit 1
      fi
      ensure_runner_image
      runner="docker"
      ;;
    auto)
      if [[ "$local_procob_ok" -eq 1 ]]; then
        runner="local"
        procob_cmd="$local_procob"
      else
        docker_ready || {
          error "Auto mode: no runnable local procob and Docker is unavailable."
          error "Run ./scripts/configure_procob_env.sh to configure docker mode."
          exit 1
        }
        docker_oracle_home="$(find_linux_oracle_home || true)"
        if [[ -z "$docker_oracle_home" ]]; then
          error "Auto mode: no runnable local procob and no Linux Oracle home for docker mode."
          error "Run ./scripts/configure_procob_env.sh --oracle-home <path>."
          exit 1
        fi
        ensure_runner_image
        runner="docker"
      fi
      ;;
  esac

  if [[ "$runner" == "local" ]]; then
    log "Using local procob: $procob_cmd"
    for pco in "${PCO_FILES[@]}"; do
      pre_out="${pco%.pco}${PRECOMPILE_EXT}"

      if [[ "$OVERWRITE" -eq 0 && -f "$pre_out" ]]; then
        log "SKIP precompile (exists): ${pre_out#$REPO_ROOT/}"
        precompiled_skipped=$((precompiled_skipped + 1))
        continue
      fi

      if [[ "$DRY_RUN" -eq 1 ]]; then
        log "DRYRUN precompile: ${pco#$REPO_ROOT/} -> ${pre_out#$REPO_ROOT/}"
      else
        procob_output="$(
          "$procob_cmd" \
            "iname=$pco" \
            "oname=$pre_out" \
            "format=$FORMAT" \
            "ireclen=$IRECLEN" \
            "oreclen=$ORECLEN" \
            "hold_cursor=no" \
            "release_cursor=yes" \
            "sqlcheck=$SQLCHECK" \
            "ltype=none" \
            "errors=yes" 2>&1
        )" || {
          printf '%s\n' "$procob_output" >&2
          error "Precompile failed: ${pco#$REPO_ROOT/}"
          exit 1
        }
        log "Precompiled: ${pco#$REPO_ROOT/} -> ${pre_out#$REPO_ROOT/}"
      fi
      precompiled_count=$((precompiled_count + 1))
    done
  else
    log "Using dockerized procob: image=$DOCKER_IMAGE, oracle_home=$docker_oracle_home"
    list_file="$(mktemp "$REPO_ROOT/.procob-file-list.XXXXXX")"
    cleanup_list() {
      rm -f "$list_file"
    }
    trap cleanup_list EXIT

    for pco in "${PCO_FILES[@]}"; do
      pre_out="${pco%.pco}${PRECOMPILE_EXT}"
      if [[ "$OVERWRITE" -eq 0 && -f "$pre_out" ]]; then
        log "SKIP precompile (exists): ${pre_out#$REPO_ROOT/}"
        precompiled_skipped=$((precompiled_skipped + 1))
        continue
      fi

      printf '%s\n' "${pco#$REPO_ROOT/}" >> "$list_file"
      precompiled_count=$((precompiled_count + 1))
    done

    if [[ ! -s "$list_file" ]]; then
      log "No files queued for precompile."
    elif [[ "$DRY_RUN" -eq 1 ]]; then
      while IFS= read -r rel; do
        log "DRYRUN precompile: $rel -> ${rel%.pco}${PRECOMPILE_EXT}"
      done < "$list_file"
    else
      list_rel="${list_file#$REPO_ROOT/}"
      docker run --rm --platform "$DOCKER_PLATFORM" \
        -e PROCOB_LIST_FILE="$list_rel" \
        -e PROCOB_PRECOMPILE_EXT="$PRECOMPILE_EXT" \
        -e PROCOB_FORMAT="$FORMAT" \
        -e PROCOB_SQLCHECK="$SQLCHECK" \
        -e PROCOB_IRECLEN="$IRECLEN" \
        -e PROCOB_ORECLEN="$ORECLEN" \
        -v "$REPO_ROOT:/work" \
        -v "$docker_oracle_home:/opt/oracle/fmw:ro" \
        -w /work \
        "$DOCKER_IMAGE" bash -lc '
          set -euo pipefail
          export ORACLE_HOME=/opt/oracle/fmw
          export PATH="$ORACLE_HOME/bin:$PATH"
          export LD_LIBRARY_PATH="$ORACLE_HOME/lib:${LD_LIBRARY_PATH:-}"
          while IFS= read -r rel; do
            [ -n "$rel" ] || continue
            in_file="/work/$rel"
            out_file="/work/${rel%.pco}${PROCOB_PRECOMPILE_EXT}"
            procob_output="$(
              procob \
                "iname=$in_file" \
                "oname=$out_file" \
                "format=$PROCOB_FORMAT" \
                "ireclen=$PROCOB_IRECLEN" \
                "oreclen=$PROCOB_ORECLEN" \
                "hold_cursor=no" \
                "release_cursor=yes" \
                "sqlcheck=$PROCOB_SQLCHECK" \
                "ltype=none" \
                "errors=yes" 2>&1
            )" || {
              printf "%s\n" "$procob_output" >&2
              printf "ERROR: precompile failed for %s\n" "$rel" >&2
              exit 1
            }
            printf "Precompiled: %s -> %s\n" "$rel" "${rel%.pco}${PROCOB_PRECOMPILE_EXT}"
          done < "/work/$PROCOB_LIST_FILE"
        '
    fi
  fi
fi

log ""
log "Summary:"
log "  Files discovered: ${#PCO_FILES[@]}"
log "  Copies created: $copied_count"
log "  Copies skipped: $copied_skipped"
log "  Precompiled: $precompiled_count"
log "  Precompile skipped: $precompiled_skipped"
