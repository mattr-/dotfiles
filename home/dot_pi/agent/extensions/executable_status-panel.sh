#!/bin/bash
# Pi status panel — runs in a cmux right split pane
# Shows modified files, git status, and session stats
# Rebuilds data every second and shows a single Session state indicator
# Requires bash 4+ for associative arrays (brew install bash)

if [[ "${BASH_VERSINFO[0]}" -lt 4 ]]; then
  # Fall back to homebrew bash if available
  if [[ -x /opt/homebrew/bin/bash ]]; then
    exec /opt/homebrew/bin/bash "$0" "$@"
  elif [[ -x /usr/local/bin/bash ]]; then
    exec /usr/local/bin/bash "$0" "$@"
  else
    echo "Requires bash 4+ (brew install bash)"
    exit 1
  fi
fi

set +e

# Colors
DIM='\033[2m'
BOLD='\033[1m'
RESET='\033[0m'
GREEN='\033[32m'
YELLOW='\033[33m'
BLUE='\033[34m'
MAGENTA='\033[35m'
CYAN='\033[36m'
RED='\033[31m'
GRAY='\033[90m'
BORDER_GRAY='\033[38;2;59;66;82m'

# Soft panel palette
SAGE_GREEN='\033[38;5;114m'
BUTTER_YELLOW='\033[38;5;228m'
SOFT_GRAY='\033[38;5;245m'
PALE_CYAN='\033[38;5;159m'
PALE_AMBER='\033[38;5;223m'
PALE_ROSE='\033[38;5;217m'

# Refresh cadence
SESSION_START=$(date +%s)
FRAME_COUNT=0
ANIMATION_INTERVAL=0.125
REFRESH_EVERY_FRAMES=8
SESSION_ANIM_TOKEN="__PI_SESSION_ANIM__"
PANEL_TEMPLATE=""
LAST_COLS=0
LAST_ROWS=0
# Stats file scoped per project directory (matches notify.ts)
if [[ -z "$PI_SESSION_DIR" ]]; then
  echo "PI_SESSION_DIR not set"
  exit 1
fi
PI_PID="${PI_PID:-$$}"
STATS_FILE="${PI_SESSION_DIR}/${PI_PID}-stats.json"

# Hide cursor during draws
tput civis 2>/dev/null || true
trap 'tput cnorm 2>/dev/null; clear; exit 0' INT TERM EXIT

build_panel_template() {
  local session_anim="${1:-$SESSION_ANIM_TOKEN}"
  local cols="${2:-$(tput cols)}"
  local rows="${3:-$(tput lines)}"
  local buf=""

  # Append a line to buffer, clear rest of line
  p() { buf+="$*"$'\033[K\n'; }
  # Helper: wrap text in OSC 8 clickable link
  link() {
    local url="$1" text="$2"
    echo -ne "\033]8;;${url}\a${text}\033]8;;\a"
  }
  # Repeat a Unicode character without relying on locale-sensitive tr(1).
  repeat_char() {
    local count="$1" char="$2" out=""
    [[ -z "$count" || "$count" -le 0 ]] && return 0
    printf -v out '%*s' "$count" ''
    printf '%s' "${out// /$char}"
  }

  # Horizontal rule — match tmux pane border gray (#3b4252).
  hr() { local r; r=$(repeat_char "$cols" '─'); p "${BORDER_GRAY}${r}${RESET}"; }

  # ── Pi session info ─────────────────────────────────
  if [[ -f "$STATS_FILE" ]]; then
    local stats
    stats=$(cat "$STATS_FILE" 2>/dev/null)
    if [[ -n "$stats" ]]; then
      local model state ctx_pct ctx_window
      model=$(echo "$stats" | python3 -c "import sys,json; print(json.load(sys.stdin).get('model',''))" 2>/dev/null || true)
      state=$(echo "$stats" | python3 -c "import sys,json; print(json.load(sys.stdin).get('state',''))" 2>/dev/null || true)
      ctx_pct=$(echo "$stats" | python3 -c "import sys,json; d=json.load(sys.stdin); p=d.get('contextPercent'); print(f'{p:.0f}' if p is not None else '')" 2>/dev/null || true)
      ctx_window=$(echo "$stats" | python3 -c "import sys,json; print(json.load(sys.stdin).get('contextWindow',0))" 2>/dev/null || true)

      local session_indicator
      case "$state" in
        working) session_indicator="$session_anim" ;;
        error)   session_indicator="${PALE_ROSE}✗${RESET}" ;;
        *)       session_indicator="${SOFT_GRAY}· · ·${RESET}" ;;
      esac

      p ""
      hr
      p " ${BOLD}Session${RESET} ${session_indicator}"
      hr
      p ""

      [[ -n "$model" ]] && p " ${BOLD}${model}${RESET}"

      # Context window bar
      if [[ -n "$ctx_pct" && "$ctx_pct" != "0" ]]; then
        local bar_width=$((cols - 16))
        local filled=$(( (${ctx_pct%.*} * bar_width) / 100 ))
        [[ $filled -gt $bar_width ]] && filled=$bar_width
        local empty=$((bar_width - filled))
        local bar_color="$PALE_CYAN"
        [[ ${ctx_pct%.*} -gt 70 ]] && bar_color="$PALE_AMBER"
        [[ ${ctx_pct%.*} -gt 90 ]] && bar_color="$PALE_ROSE"
        local bar="${bar_color}$(repeat_char "$filled" '█')${SOFT_GRAY}$(repeat_char "$empty" '░')${RESET}"
        # Format context window size
        local win_label=""
        if [[ -n "$ctx_window" && "$ctx_window" != "0" ]]; then
          local cwint=${ctx_window%.*}
          if [[ $cwint -ge 1000000 ]]; then
            win_label=" / $((cwint / 1000000))M"
          elif [[ $cwint -ge 1000 ]]; then
            win_label=" / $((cwint / 1000))k"
          fi
        fi
        p ""
        p " ${bar} ${ctx_pct}%${GRAY}${win_label}${RESET}"
        p ""
      fi

      # Token counts — match Pi's footer format: ↑input ↓output R{cache} W{cacheWrite} $cost
      local in_tok out_tok cr_tok cw_tok cost turns
      in_tok=$(echo "$stats" | python3 -c "import sys,json; v=json.load(sys.stdin).get('inputTokens',0); print(f'{v/1000:.1f}k' if v>=1000 else v)" 2>/dev/null || true)
      out_tok=$(echo "$stats" | python3 -c "import sys,json; v=json.load(sys.stdin).get('outputTokens',0); print(f'{v/1000:.1f}k' if v>=1000 else v)" 2>/dev/null || true)
      cr_tok=$(echo "$stats" | python3 -c "import sys,json; v=json.load(sys.stdin).get('cacheRead',0); print(f'{v/1000:.1f}k' if v>=1000 else ('' if v==0 else v))" 2>/dev/null || true)
      cw_tok=$(echo "$stats" | python3 -c "import sys,json; v=json.load(sys.stdin).get('cacheWrite',0); print(f'{v/1000:.1f}k' if v>=1000 else ('' if v==0 else v))" 2>/dev/null || true)
      cost=$(echo "$stats" | python3 -c "import sys,json; v=json.load(sys.stdin).get('cost',0); print(f'\${v:.3f}' if v>0 else '')" 2>/dev/null || true)
      turns=$(echo "$stats" | python3 -c "import sys,json; print(json.load(sys.stdin).get('turns',0))" 2>/dev/null || true)

      local tok_line=" ${GRAY}↑${in_tok} ↓${out_tok}"
      [[ -n "$cr_tok" ]] && tok_line+=" R${cr_tok}"
      [[ -n "$cw_tok" ]] && tok_line+=" W${cw_tok}"
      [[ -n "$cost" ]] && tok_line+="  ${cost}"
      [[ "$turns" != "0" ]] && tok_line+="  ${turns} turn$([[ "$turns" != "1" ]] && echo 's')"
      tok_line+="${RESET}"
      p "$tok_line"
    fi
  fi
  p ""

  # ── Todos ────────────────────────────────────────────
  local TODOS_FILE="${PI_SESSION_DIR}/${PI_PID}-todos.json"
  if [[ -f "$TODOS_FILE" ]]; then
    local todos_json
    todos_json=$(cat "$TODOS_FILE" 2>/dev/null)
    local task_count
    task_count=$(echo "$todos_json" | python3 -c "import sys,json; t=json.load(sys.stdin).get('tasks',[]); print(len(t))" 2>/dev/null || echo "0")

    if [[ "$task_count" -gt 0 ]]; then
      hr
      p " ${BOLD}Tasks${RESET}"
      hr
      p ""

      local done_count active_count total_count
      done_count=$(echo "$todos_json" | python3 -c "
import sys,json
tasks=json.load(sys.stdin).get('tasks',[])
print(sum(1 for t in tasks if t['status'] in ('completed','cancelled')))
" 2>/dev/null || echo "0")
      active_count=$(echo "$todos_json" | python3 -c "
import sys,json
tasks=json.load(sys.stdin).get('tasks',[])
print(sum(1 for t in tasks if t['status'] == 'in_progress'))
" 2>/dev/null || echo "0")
      total_count="$task_count"

      # Progress bar: done = sage green, in-progress = butter yellow, pending = soft gray
      local bar_width=$((cols - 12))
      local done_width=0
      local active_width=0
      local done_end=0
      local active_end=0
      if [[ "$total_count" -gt 0 ]]; then
        done_end=$(( (done_count * bar_width + total_count / 2) / total_count ))
        active_end=$(( ((done_count + active_count) * bar_width + total_count / 2) / total_count ))
      fi
      [[ $done_end -gt $bar_width ]] && done_end=$bar_width
      [[ $active_end -gt $bar_width ]] && active_end=$bar_width
      done_width=$done_end
      active_width=$((active_end - done_width))
      [[ $active_width -lt 0 ]] && active_width=0
      local empty=$((bar_width - done_width - active_width))
      [[ $empty -lt 0 ]] && empty=0
      local tbar="${SAGE_GREEN}$(repeat_char "$done_width" '█')${BUTTER_YELLOW}$(repeat_char "$active_width" '█')${SOFT_GRAY}$(repeat_char "$empty" '░')${RESET}"
      p " ${tbar} ${done_count}/${total_count}"
      p ""
      p ""

      # Task list — wrap with hanging indent so multi-line titles stay aligned
      local task_lines
      task_lines=$(echo "$todos_json" | python3 -c "
import sys,json,textwrap
width=max(10, int(sys.argv[1]))
tasks=json.load(sys.stdin).get('tasks',[])
icons={'pending':'○','in_progress':'▸','completed':'✓','cancelled':'✗'}
colors={'pending':'\033[38;5;245m','in_progress':'\033[38;5;228m','completed':'\033[38;5;114m','cancelled':'\033[2m'}
reset='\033[0m'
dim='\033[2m'
for i, t in enumerate(tasks):
    s=t['status']
    ic=icons.get(s,'?')
    co=colors.get(s,'')
    title=' '.join(str(t.get('title','')).split())
    prefix=f' {co}{ic}{reset} '
    indent='   '
    wrap_width=max(8, width - 3)
    wrapped=textwrap.wrap(title, width=wrap_width, break_long_words=False, break_on_hyphens=False) or ['']
    if s in ('completed','cancelled'):
        print(f'{prefix}{dim}{wrapped[0]}{reset}')
        for line in wrapped[1:]:
            print(f'{indent}{dim}{line}{reset}')
    elif s == 'in_progress':
        print(f'{prefix}{co}{wrapped[0]}{reset}')
        for line in wrapped[1:]:
            print(f'{indent}{co}{line}{reset}')
    else:
        print(f'{prefix}{wrapped[0]}')
        for line in wrapped[1:]:
            print(f'{indent}{line}')
    if i < len(tasks) - 1:
        print('')
" "$cols" 2>/dev/null)
      while IFS= read -r tline; do
        p "$tline"
      done <<< "$task_lines"
      p ""
    fi
  fi
  p ""

  # ── Changes (branch + files) ────────────────────────
  hr
  p " ${BOLD}Changes${RESET}"
  hr
  p ""

  local branch
  branch=$(git branch --show-current 2>/dev/null || echo "detached")
  local ahead=0 behind=0
  local ab
  ab=$(git rev-list --left-right --count "origin/${branch}...HEAD" 2>/dev/null) && {
    behind=$(echo "$ab" | cut -f1)
    ahead=$(echo "$ab" | cut -f2)
  }

  local bi="${branch}"
  [[ "$ahead" != "0" ]] && bi+=" ↑${ahead}"
  [[ "$behind" != "0" ]] && bi+=" ↓${behind}"
  p " ${CYAN}⎇${RESET} ${BOLD}${bi}${RESET}"
  p ""

  local git_root
  git_root=$(git rev-parse --show-toplevel 2>/dev/null)
  local diff_files=""
  local diff_args=""

  local file_count=0
  local max_files=$((rows - 30))
  [[ $max_files -lt 5 ]] && max_files=5
  local maxpath=$((cols - 16))

  short() {
    local f="$1"
    if [[ ${#f} -gt $maxpath ]]; then
      local name="${f##*/}"
      local dir="${f%/*}"
      local avail=$((maxpath - ${#name} - 4))
      if [[ $avail -gt 2 ]]; then
        echo "${dir:0:$avail}…/${name}"
      else
        echo "…${f: -$((maxpath - 1))}"
      fi
    else
      echo "$f"
    fi
  }

  # Get per-file diff stats into an associative array
  declare -A file_stats
  while IFS=$'\t' read -r adds dels sfile; do
    [[ -z "$sfile" ]] && continue
    file_stats["$sfile"]="${GREEN}+${adds}${RESET} ${RED}-${dels}${RESET}"
  done < <(git diff --numstat HEAD 2>/dev/null)

  # Stats for untracked files (line count)
  while IFS= read -r uf; do
    [[ -z "$uf" ]] && continue
    local lc
    lc=$(wc -l < "$uf" 2>/dev/null | tr -d ' ' || echo "0")
    file_stats["$uf"]="${GREEN}+${lc}${RESET}"
  done < <(git ls-files --others --exclude-standard 2>/dev/null)

  # Staged
  while IFS= read -r line; do
    [[ -z "$line" ]] && continue
    local st="${line%%$'\t'*}" f="${line#*$'\t'}" ic co lb
    case "$st" in
      A) ic="+"; co="$GREEN";  lb="staged" ;;
      M) ic="~"; co="$YELLOW"; lb="staged" ;;
      D) ic="-"; co="$RED";    lb="staged" ;;
      R) ic="→"; co="$BLUE";   lb="staged" ;;
      *) ic="?"; co="$GRAY";   lb="staged" ;;
    esac
    local ds="${file_stats[$f]:-}"
    [[ $file_count -lt $max_files ]] && p " ${co}${ic}${RESET} $(link "file://${git_root}/${f}" "$(short "$f")") ${ds} ${GRAY}${lb}${RESET}"
    diff_files+=" ${git_root}/${f}"
    file_count=$((file_count + 1))
  done < <(git diff --cached --name-status 2>/dev/null)

  # Unstaged
  while IFS= read -r line; do
    [[ -z "$line" ]] && continue
    local st="${line%%$'\t'*}" f="${line#*$'\t'}" ic co
    case "$st" in
      M) ic="~"; co="$YELLOW" ;; D) ic="-"; co="$RED" ;; *) ic="?"; co="$GRAY" ;;
    esac
    local ds="${file_stats[$f]:-}"
    [[ $file_count -lt $max_files ]] && p " ${co}${ic}${RESET} $(link "file://${git_root}/${f}" "$(short "$f")") ${ds}"
    diff_files+=" ${git_root}/${f}"
    file_count=$((file_count + 1))
  done < <(git diff --name-status 2>/dev/null)

  # Untracked
  while IFS= read -r f; do
    [[ -z "$f" ]] && continue
    local ds="${file_stats[$f]:-}"
    [[ $file_count -lt $max_files ]] && p " ${GREEN}+${RESET} $(link "file://${git_root}/${f}" "$(short "$f")") ${ds}"
    diff_files+=" ${git_root}/${f}"
    file_count=$((file_count + 1))
  done < <(git ls-files --others --exclude-standard 2>/dev/null)

  if [[ $file_count -eq 0 ]]; then
    p " ${GRAY}No changes${RESET}"
  elif [[ $file_count -gt $max_files ]]; then
    p " ${GRAY}… +$((file_count - max_files)) more${RESET}"
  fi

  PANEL_TEMPLATE="$buf"
}

session_refresh_indicator() {
  local idx=$((FRAME_COUNT % REFRESH_EVERY_FRAMES))
  local frames=(
    "${SOFT_GRAY}· · ·${RESET}"
    "${PALE_CYAN}• · ·${RESET}"
    "${PALE_CYAN}• • ·${RESET}"
    "${BOLD}${CYAN}• • •${RESET}"
    "${PALE_CYAN}• • ·${RESET}"
    "${PALE_CYAN}• · ·${RESET}"
    "${SOFT_GRAY}· · ·${RESET}"
    "${SOFT_GRAY}· · ·${RESET}"
  )
  printf '%s' "${frames[$((idx % ${#frames[@]}))]}"
}

render_panel() {
  local session_anim="$1"
  local rendered="${PANEL_TEMPLATE//${SESSION_ANIM_TOKEN}/${session_anim}}"
  printf '\033[H%b\033[J' "$rendered"
}

# ── Main loop ─────────────────────────────────────────

if ! git rev-parse --is-inside-work-tree &>/dev/null; then
  echo "Not a git repository"
  exit 1
fi

clear
while true; do
  local_cols=$(tput cols)
  local_rows=$(tput lines)

  if [[ -z "$PANEL_TEMPLATE" || $((FRAME_COUNT % REFRESH_EVERY_FRAMES)) -eq 0 || "$local_cols" != "$LAST_COLS" || "$local_rows" != "$LAST_ROWS" ]]; then
    build_panel_template "$SESSION_ANIM_TOKEN" "$local_cols" "$local_rows"
    LAST_COLS="$local_cols"
    LAST_ROWS="$local_rows"
  fi

  render_panel "$(session_refresh_indicator)"

  FRAME_COUNT=$((FRAME_COUNT + 1))
  sleep "$ANIMATION_INTERVAL"
done
