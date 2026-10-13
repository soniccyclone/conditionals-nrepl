#!/usr/bin/env bash
set -euo pipefail

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
root=$(cd "$here/../.." && pwd)
out="$root/target/ci-local"

LEG=${LEG:-}
LOOP=${LOOP:-}
TEST=${TEST:-}
JOBS=${JOBS:-8}
LEG_TIMEOUT=${LEG_TIMEOUT:-}
LEG_CPUS=${LEG_CPUS:-2}
M2_DIR=${M2_DIR:-$HOME/.m2}
GITLIBS_DIR=${GITLIBS_DIR:-$HOME/.gitlibs}
LOOP_DEFAULT_LEG=1.12-temurin-21
MATRIX_IMAGE=docker.io/library/clojure:temurin-21-noble
YAML_LIB_VERSION=1.0.29
CLR_BASE_IMAGE=nrepl-clr
CLR_BASE_DOTNET=8.0

run_id=$$
name_prefix="nrepl-ci-local-$run_id"

say() { printf '%s\n' "$*"; }
die() { printf 'ci-local: %s\n' "$*" >&2; exit 1; }

stop_started() {
  local name
  while IFS= read -r name; do
    [ -n "$name" ] || continue
    podman stop --time 5 "$name" >/dev/null 2>&1 || true
    podman rm --force "$name" >/dev/null 2>&1 || true
  done < <(podman ps --all --filter "name=^$name_prefix-" --format '{{.Names}}' 2>/dev/null)
}

on_signal() {
  trap - INT TERM
  say "ci-local: interrupted, stopping the containers it started" >&2
  jobs -p | xargs -r kill 2>/dev/null || true
  stop_started
  exit 130
}

trap on_signal INT TERM
trap stop_started EXIT

command -v podman >/dev/null || die "podman is required"
[ "${BASH_VERSINFO[0]}" -ge 4 ] || die "bash 4 or newer is required"

if [ -n "$LOOP" ] || [ -n "$TEST" ]; then
  [ -n "$LOOP" ] && [ -n "$TEST" ] || die "LOOP and TEST go together: make ci-local LOOP=50 TEST=nrepl.socket-test"
  case "$LOOP" in
    '' | *[!0-9]* | 0) die "LOOP must be a positive integer, got '$LOOP'" ;;
  esac
  LEG=${LEG:-$LOOP_DEFAULT_LEG}
  LEG_TIMEOUT=${LEG_TIMEOUT:-$((600 + LOOP))}
fi

LEG_TIMEOUT=${LEG_TIMEOUT:-600}

mkdir -p "$out" "$root/.cpcache" "$M2_DIR" "$GITLIBS_DIR"

command -v flock >/dev/null || die "flock (util-linux) is required"
exec 9>"$out/.lock"
flock --nonblock 9 || die "another make ci-local is running in this checkout (lock: target/ci-local/.lock); use a second worktree to run two at once"

now() { date +%s.%N; }
elapsed() { awk -v a="$1" -v b="$2" 'BEGIN { printf "%.0f", b - a }'; }

generate_matrix() {
  local name="$name_prefix-matrix"
  rm -f "$out/legs.tsv"
  podman run --rm --name "$name" --security-opt label=disable \
    -v "$here":/verify:ro \
    -v "$root/.github/workflows":/workflows:ro \
    -v "$out":/out \
    -v "$M2_DIR":/root/.m2 \
    -w /tmp \
    "$MATRIX_IMAGE" \
    clojure -Sdeps "{:deps {clj-commons/clj-yaml {:mvn/version \"$YAML_LIB_VERSION\"}}}" \
    -M /verify/ci_matrix.clj /workflows /out \
    || die "could not read the workflow matrix from .github/workflows"
  [ -s "$out/legs.tsv" ] || die "the workflow matrix came back empty"
}

declare -A kind jdk clojure aliases cli dotnet clojure_main cljr informational image
all_legs=()

load_matrix() {
  local line field leg
  declare -A row
  while IFS= read -r line; do
    row=()
    IFS=$'\t' read -r -a fields <<<"$line"
    for field in "${fields[@]}"; do
      row[${field%%=*}]=${field#*=}
    done
    leg=${row[leg]}
    all_legs+=("$leg")
    kind[$leg]=${row[kind]}
    informational[$leg]=${row[informational]}
    case "${row[kind]}" in
      jvm | lint)
        jdk[$leg]=${row[jdk]}
        clojure[$leg]=${row[clojure]}
        aliases[$leg]=${row[aliases]}
        cli[$leg]=${row[cli]}
        image[$leg]="nrepl-ci-jvm-${row[jdk]}-${row[cli]}"
        ;;
      clr)
        dotnet[$leg]=${row[dotnet]}
        clojure_main[$leg]=${row[clojure-main]}
        cljr[$leg]=${row[cljr]}
        if [ "${row[dotnet]}" = "$CLR_BASE_DOTNET" ]; then
          image[$leg]=$CLR_BASE_IMAGE
        else
          image[$leg]="nrepl-ci-clr-${row[dotnet]}"
        fi
        ;;
    esac
  done <"$out/legs.tsv"
}

select_legs() {
  selected=()
  local leg
  if [ -z "$LEG" ]; then
    selected=("${all_legs[@]}")
    return
  fi
  for leg in "${all_legs[@]}"; do
    [ "$leg" = "$LEG" ] && selected=("$leg")
  done
  [ "${#selected[@]}" -eq 1 ] || die "unknown LEG '$LEG'; the legs are: ${all_legs[*]}"
  if [ -n "$LOOP" ] && [ "${kind[$LEG]}" != jvm ]; then
    die "LOOP works on the JVM legs only; '$LEG' is a ${kind[$LEG]} leg"
  fi
}

build_image_for() {
  local leg=$1 tag=${image[$1]}
  local log="$out/build-$tag.log"
  if [ "$tag" = "$CLR_BASE_IMAGE" ] && podman image exists "$tag"; then
    return 0
  fi
  case "${kind[$leg]}" in
    jvm | lint)
      podman build --quiet --tag "$tag" \
        --build-arg "JDK=${jdk[$leg]}" --build-arg "CLI_VERSION=${cli[$leg]}" \
        --file "$here/Containerfile.jvm" "$here" >"$log" 2>&1
      ;;
    clr)
      podman build --quiet --tag "$tag" \
        --build-arg "DOTNET=${dotnet[$leg]}" \
        --build-arg "CLOJURE_MAIN_VERSION=${clojure_main[$leg]}" \
        --build-arg "CLJR_VERSION=${cljr[$leg]}" \
        --file "$here/Containerfile.clr" "$here" >"$log" 2>&1
      ;;
  esac
}

build_images() {
  local leg tag pid
  declare -A building_leg building_pid
  for leg in "${selected[@]}"; do
    tag=${image[$leg]}
    [ -n "${building_leg[$tag]:-}" ] && continue
    building_leg[$tag]=$leg
    build_image_for "$leg" &
    building_pid[$tag]=$!
  done
  local failed=0
  for tag in "${!building_pid[@]}"; do
    if ! wait "${building_pid[$tag]}"; then
      say "[FAIL] image $tag (for leg ${building_leg[$tag]}) did not build; see target/ci-local/build-$tag.log" >&2
      tail -n 15 "$out/build-$tag.log" >&2 || true
      failed=1
    fi
  done
  [ "$failed" -eq 0 ] || die "image build failed"
}

git_common_dir() {
  local common
  common=$(git -C "$root" rev-parse --path-format=absolute --git-common-dir 2>/dev/null) || return 0
  case "$common" in
    "$root"/*) ;;
    *) printf '%s' "$common" ;;
  esac
}

leg_dir() { printf '%s/%s' "$out" "$1"; }

container_args() {
  local leg=$1 name=$2 timeout_seconds=$3
  local dir
  dir=$(leg_dir "$leg")
  mkdir -p "$dir/test-results"
  args=(run --rm --init --name "$name" --timeout "$timeout_seconds" --pids-limit -1 --security-opt label=disable
    -v "$root":/work:O
    -v "$root/.cpcache":/work/.cpcache
    -v "$dir/test-results":/work/test-results
    --tmpfs /work/target
    -v "$dir":/ci
    -v "$M2_DIR":/root/.m2
    -v "$GITLIBS_DIR":/root/.gitlibs
    -w /work)
  if [ "$LEG_CPUS" != 0 ]; then
    args+=(--cpus "$LEG_CPUS")
  fi
  if [ -n "$common_git_dir" ]; then
    args+=(-v "$common_git_dir:$common_git_dir:O")
  fi
  args+=("${image[$leg]}")
}

phase_files() {
  local leg=$1 phase=$2
  awk -F'\t' -v phase="$phase" '$1 == phase { print $2 }' "$(leg_dir "$leg")/phases.tsv"
}

script_command() {
  local f
  printf 'set -eo pipefail;'
  for f in "$@"; do
    printf ' bash -eo pipefail /ci/%s || exit $?;' "$f"
  done
}

loop_script() {
  local leg=$1 dir
  dir=$(leg_dir "$leg")
  {
    grep '^export ' "$dir/run-00.sh"
    printf '%s\n' \
      'clojure -T:build javac :with-tests true' \
      "clojure -M:${clojure[$leg]}:dev${aliases[$leg]} plan/verify/loop_test.clj '$TEST' '$LOOP'"
  } >"$dir/loop-00.sh"
}

run_in_container() {
  local leg=$1 name=$2 timeout_seconds=$3
  shift 3
  local args
  container_args "$leg" "$name" "$timeout_seconds"
  timeout "$((timeout_seconds + 60))" podman "${args[@]}" bash -c "$(script_command "$@")"
}

prefetch() {
  declare -A seen
  local leg signature files n=0 failed=0
  for leg in "${selected[@]}"; do
    mapfile -t files < <(phase_files "$leg" prepare)
    signature=$(cd "$(leg_dir "$leg")" && cat "${files[@]}" | sha256sum | cut -c1-16)
    [ -n "${seen[$signature]:-}" ] && continue
    seen[$signature]=1
    n=$((n + 1))
    say "prefetch $n: $leg"
    if ! run_in_container "$leg" "$name_prefix-prefetch-$n" "$LEG_TIMEOUT" "${files[@]}" >"$out/prefetch-$n.log" 2>&1; then
      say "[FAIL] prefetch for $leg failed; see target/ci-local/prefetch-$n.log" >&2
      tail -n 15 "$out/prefetch-$n.log" >&2 || true
      failed=1
    fi
  done
  [ "$failed" -eq 0 ] || die "dependency prefetch failed"
}

run_leg() {
  local leg=$1 dir files start finish status=0
  dir=$(leg_dir "$leg")
  if [ -n "$LOOP" ]; then
    loop_script "$leg"
    files=(loop-00.sh)
  else
    mapfile -t files < <(phase_files "$leg" run)
  fi
  rm -rf "$dir/test-results" "$dir/result.env"
  start=$(now)
  run_in_container "$leg" "$name_prefix-$leg" "$LEG_TIMEOUT" "${files[@]}" >"$out/$leg.log" 2>&1 || status=$?
  finish=$(now)
  printf 'status=%s\nseconds=%s\n' "$status" "$(elapsed "$start" "$finish")" >"$dir/result.env"
}

longest_first() {
  local wanted leg
  for wanted in clr jvm lint; do
    for leg in "${selected[@]}"; do
      [ "${kind[$leg]}" = "$wanted" ] && printf '%s\n' "$leg"
    done
  done
}

run_legs() {
  local leg limit=$JOBS running=0 order
  mapfile -t order < <(longest_first)
  for leg in "${order[@]}"; do
    while [ "$running" -ge "$limit" ]; do
      wait -n || true
      running=$((running - 1))
    done
    run_leg "$leg" &
    running=$((running + 1))
  done
  wait || true
}

strip_log() { sed 's/\x1b\[[0-9;]*m//g' "$1" | tr -d '\r'; }

field_after() { sed -n "s/.* \\([0-9][0-9]*\\) $2.*/\\1/p" <<<" $1" | head -n 1; }

declare -A r_tests r_assertions r_failures r_errors r_status r_seconds r_note r_exit

evaluate_leg() {
  local leg=$1 dir count result status=0 seconds=0
  dir=$(leg_dir "$leg")
  if [ -f "$dir/result.env" ]; then
    status=$(sed -n 's/^status=//p' "$dir/result.env")
    seconds=$(sed -n 's/^seconds=//p' "$dir/result.env")
  else
    status=missing
  fi
  r_exit[$leg]=$status
  r_seconds[$leg]=$seconds
  r_tests[$leg]=-
  r_assertions[$leg]=-
  r_failures[$leg]=-
  r_errors[$leg]=-
  r_note[$leg]=

  local ok=1
  case "${kind[$leg]}" in
    jvm)
      count=$(strip_log "$out/$leg.log" | grep -E '^[0-9]+ tests?, [0-9]+ assertions?, ' | tail -n 1 || true)
      if [ -z "$count" ]; then
        r_note[$leg]="no kaocha count line"
        ok=0
      else
        r_tests[$leg]=$(field_after "$count" 'tests\?')
        r_assertions[$leg]=$(field_after "$count" 'assertions\?')
        r_failures[$leg]=$(field_after "$count" 'failures\?')
        r_errors[$leg]=$(field_after "$count" 'errors\?')
        [ -n "${r_failures[$leg]}" ] || r_failures[$leg]=0
        [ -n "${r_errors[$leg]}" ] || r_errors[$leg]=0
      fi
      ;;
    clr)
      count=$(strip_log "$out/$leg.log" | grep -E '^Ran [0-9]+ tests containing [0-9]+ assertions\.' | tail -n 1 || true)
      result=$(strip_log "$out/$leg.log" | grep -E '^[0-9]+ failures, [0-9]+ errors\.' | tail -n 1 || true)
      if [ -z "$count" ] || [ -z "$result" ]; then
        r_note[$leg]="NO COUNT LINE"
        ok=0
      else
        r_tests[$leg]=$(field_after "$count" 'tests')
        r_assertions[$leg]=$(field_after "$count" 'assertions')
        r_failures[$leg]=$(field_after "$result" 'failures')
        r_errors[$leg]=$(field_after "$result" 'errors')
      fi
      ;;
  esac

  if [ "$ok" -eq 1 ] && [ "$status" = 0 ] && { [ "${kind[$leg]}" = lint ] || { [ "${r_failures[$leg]}" = 0 ] && [ "${r_errors[$leg]}" = 0 ]; }; }; then
    r_status[$leg]=PASS
  else
    if [ "$status" = 124 ] || [ "$status" = 137 ]; then
      r_note[$leg]="timed out after ${LEG_TIMEOUT}s"
    elif [ -z "${r_note[$leg]}" ]; then
      r_note[$leg]="exit $status"
    fi
    r_status[$leg]=FAIL
  fi
  if [ "${informational[$leg]}" = true ] && [ "${r_status[$leg]}" = FAIL ]; then
    r_status[$leg]=INFO
  fi
}

print_table() {
  local leg
  say
  printf '%-22s %6s %10s %9s %7s  %-6s %6s  %s\n' LEG TESTS ASSERTIONS FAILURES ERRORS RESULT SECONDS LOG
  for leg in "${selected[@]}"; do
    printf '%-22s %6s %10s %9s %7s  %-6s %6s  %s\n' "$leg" \
      "${r_tests[$leg]}" "${r_assertions[$leg]}" "${r_failures[$leg]}" "${r_errors[$leg]}" \
      "${r_status[$leg]}" "${r_seconds[$leg]}" "target/ci-local/$leg.log"
    if [ "${r_status[$leg]}" != PASS ]; then
      [ -z "${r_note[$leg]}" ] || printf '%-22s %s\n' '' "${r_note[$leg]}"
      failed_tests "$leg" | sed 's/^/                       failed: /'
      printf '%-22s rerun: make ci-local LEG=%s\n' '' "$leg"
    fi
  done
}

failed_tests() {
  strip_log "$out/$1.log" | grep -a -E '^(FAIL|ERROR) in ' | sed -E 's/^(FAIL|ERROR) in //; s/ \(.*//' | sort -u
}

print_failure_tails() {
  local leg
  for leg in "${selected[@]}"; do
    if [ "${r_status[$leg]}" = FAIL ] && [ -z "$(failed_tests "$leg")" ]; then
      say
      say "--- tail of target/ci-local/$leg.log (${r_note[$leg]})"
      strip_log "$out/$leg.log" | tail -n 25
    fi
  done
}

print_loop_report() {
  local leg=$1 log="$out/$1.log" result
  result=$(strip_log "$log" | grep '^LOOP-RESULT ' | tail -n 1 || true)
  say
  if [ -z "$result" ]; then
    say "LOOP: no LOOP-RESULT line; the run died or timed out (${r_note[$leg]:-exit ${r_exit[$leg]}}). Last progress: $(strip_log "$log" | grep LOOP-PROGRESS | tail -n 1). Tail of target/ci-local/$leg.log:"
    strip_log "$log" | tail -n 25
    return 1
  fi
  say "LOOP $TEST on $leg: ${result#LOOP-RESULT } (${r_seconds[$leg]} s wall, including JVM start and compilation)"
  if strip_log "$log" | grep -q '^LOOP-FIRST-FAILURE-BEGIN'; then
    say "--- first failing run"
    strip_log "$log" | sed -n '/^LOOP-FIRST-FAILURE-BEGIN/,/^LOOP-FIRST-FAILURE-END/p'
  fi
  case "$result" in
    *" failing-runs=0 "*) return 0 ;;
    *) return 1 ;;
  esac
}

overall_start=$(now)
common_git_dir=$(git_common_dir)

generate_matrix
load_matrix
select_legs

say "ci-local: ${#selected[@]} leg(s), run $run_id, containers named $name_prefix-*"
build_start=$(now)
build_images
build_finish=$(now)
prefetch
prefetch_finish=$(now)
run_legs
run_finish=$(now)

for leg in "${selected[@]}"; do
  evaluate_leg "$leg"
done

exit_code=0

if [ -n "$LOOP" ]; then
  print_loop_report "${selected[0]}" || exit_code=1
else
  print_table
  for leg in "${selected[@]}"; do
    [ "${r_status[$leg]}" = FAIL ] && exit_code=1
  done
  print_failure_tails
fi

say
say "wall time: $(elapsed "$overall_start" "$run_finish") s total (images $(elapsed "$build_start" "$build_finish") s, prefetch $(elapsed "$build_finish" "$prefetch_finish") s, legs $(elapsed "$prefetch_finish" "$run_finish") s)"
if [ "$exit_code" -ne 0 ]; then
  say "ci-local: FAILED" >&2
fi
exit "$exit_code"
