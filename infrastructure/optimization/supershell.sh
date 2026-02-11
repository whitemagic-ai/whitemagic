#!/usr/bin/env bash
set -euo pipefail

# supershell.sh - Ultimate Laptop Optimization Script
# Combines the best optimization techniques from all shell scripts
# Usage: sudo ./supershell.sh [adaptive|auto|performance|dev|balanced|powersave|status]

readonly SCRIPT_NAME="$(basename "$0")"
readonly DEFAULT_LOG_FILE="/var/log/supershell.log"
LOG_FILE=""
BACKUP_DIR=""
REAL_USER=""
REAL_HOME=""
DROP_CACHES=0
OPT_QUICK=0
IO_THROTTLE=0
NO_CLEAN=0
NO_CLEAN_SET=0
REQUESTED_MODE=""
DROP_SWAP=0
SNAPD_QUIET=0
PERSIST_SYSCTL=1
SYSCTL_CONF="/etc/sysctl.d/99-supershell.conf"
STATE_DIR="/var/lib/supershell"
SNAPD_MARKER="$STATE_DIR/snapd_quiet"
readonly BATTERY_START_THRESH=60
readonly BATTERY_STOP_THRESH=80
readonly IO_UDEV_RULE="/etc/udev/rules.d/60-supershell-io-scheduler.rules"
readonly IO_HELPER="/usr/local/sbin/supershell-io-scheduler"
CPU_MIN_PERF_OVERRIDE=""
CPU_MAX_PERF_OVERRIDE=""
SWAPPINESS_OVERRIDE=""
VFS_CACHE_PRESSURE_OVERRIDE=""
ZRAM_PERCENT_OVERRIDE=""

# Colors for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m' # No Color

# Logging function
log() {
    local msg="${GREEN}[SUPER]${NC} $1"
    if [[ -n "${LOG_FILE:-}" ]]; then
        echo -e "$msg" | tee -a "$LOG_FILE" >/dev/null || echo -e "$msg"
    else
        echo -e "$msg"
    fi
}

warn() {
    local msg="${YELLOW}[WARN]${NC} $1"
    if [[ -n "${LOG_FILE:-}" ]]; then
        echo -e "$msg" | tee -a "$LOG_FILE" >/dev/null || echo -e "$msg"
    else
        echo -e "$msg"
    fi
}

error() {
    local msg="${RED}[ERROR]${NC} $1"
    if [[ -n "${LOG_FILE:-}" ]]; then
        echo -e "$msg" | tee -a "$LOG_FILE" >/dev/null || echo -e "$msg"
    else
        echo -e "$msg"
    fi
}

init_context() {
    REAL_USER="${SUPERSHELL_USER:-${SUDO_USER:-$USER}}"
    if command -v getent >/dev/null 2>&1; then
        REAL_HOME="$(getent passwd "$REAL_USER" | cut -d: -f6)"
    fi
    REAL_HOME="${REAL_HOME:-$HOME}"

    if [[ ${EUID:-0} -eq 0 ]]; then
        LOG_FILE="$DEFAULT_LOG_FILE"
    else
        LOG_FILE="$REAL_HOME/.supershell.log"
    fi

    BACKUP_DIR="$REAL_HOME/.supershell_backup_$(date +%Y%m%d_%H%M%S)"
}

run_as_user() {
    if [[ ${EUID:-0} -eq 0 && -n "$REAL_USER" && "$REAL_USER" != "root" ]] && command -v sudo >/dev/null 2>&1; then
        local env_args=()
        local uid runtime_dir
        uid="$(id -u "$REAL_USER" 2>/dev/null || true)"
        runtime_dir="/run/user/${uid:-}"
        if [[ -n "${uid:-}" && -d "$runtime_dir" ]]; then
            env_args+=("XDG_RUNTIME_DIR=$runtime_dir")
            if [[ -S "$runtime_dir/bus" ]]; then
                env_args+=("DBUS_SESSION_BUS_ADDRESS=unix:path=$runtime_dir/bus")
            fi
        fi
        if [[ "${#env_args[@]}" -gt 0 ]]; then
            sudo -u "$REAL_USER" -E env "${env_args[@]}" "$@"
        else
            sudo -u "$REAL_USER" -E "$@"
        fi
    else
        "$@"
    fi
}

have_cmd() {
    command -v "$1" >/dev/null 2>&1
}

persist_sysctl() {
    local key="$1"
    local value="$2"

    if [[ "$PERSIST_SYSCTL" -ne 1 ]]; then
        return 0
    fi

    if [[ ${EUID:-0} -ne 0 ]]; then
        warn "Skipping persistent sysctl for ${key} (need root)"
        return 0
    fi

    local conf="${SYSCTL_CONF:-/etc/sysctl.d/99-supershell.conf}"
    mkdir -p "$(dirname "$conf")" 2>/dev/null || true
    if [[ ! -f "$conf" ]]; then
        printf "# Supershell sysctl overrides\n" > "$conf" 2>/dev/null || true
    fi
    set_config_kv "$conf" "$key" "$value"
}

set_sysctl() {
    local key="$1"
    local value="$2"

    if [[ ${EUID:-0} -ne 0 ]]; then
        warn "Skipping ${key}=${value} (need root)"
        return 0
    fi

    if sysctl -w "${key}=${value}" >/dev/null 2>&1; then
        log "Set ${key} to ${value}"
        persist_sysctl "$key" "$value"
    else
        warn "Failed to set ${key}=${value}"
    fi
}

set_config_kv() {
    local file="$1"
    local key="$2"
    local value="$3"

    if [[ ! -f "$file" ]]; then
        touch "$file" 2>/dev/null || true
    fi

    if grep -qE "^[[:space:]#]*${key}=" "$file" 2>/dev/null; then
        sed -i "s|^[[:space:]#]*${key}=.*|${key}=${value}|" "$file" || true
    else
        echo "${key}=${value}" >> "$file"
    fi
}

clamp_pct() {
    local value="$1"
    if [[ "$value" -lt 0 ]]; then
        value=0
    elif [[ "$value" -gt 100 ]]; then
        value=100
    fi
    echo "$value"
}

calc_zram_percent() {
    local mem_kb
    local mem_mb
    mem_kb="$(awk '/MemTotal/ {print $2}' /proc/meminfo 2>/dev/null || echo 0)"
    mem_mb=$((mem_kb / 1024))

    if (( mem_mb <= 0 )); then
        echo 70
    elif (( mem_mb <= 8192 )); then
        echo 70
    elif (( mem_mb <= 16384 )); then
        echo 50
    elif (( mem_mb <= 32768 )); then
        echo 40
    else
        echo 33
    fi
}

is_low_power_cpu() {
    local model="$1"
    local normalized
    normalized="$(tr '[:upper:]' '[:lower:]' <<< "${model:-}")"
    if [[ -z "$normalized" ]]; then
        return 1
    fi
    if [[ "$normalized" =~ n[0-9]{3,4} ]] || [[ "$normalized" =~ j[0-9]{3,4} ]] || \
       [[ "$normalized" =~ celeron ]] || [[ "$normalized" =~ atom ]]; then
        return 0
    fi
    return 1
}

get_mem_kib() {
    awk '/MemTotal/ {print $2}' /proc/meminfo 2>/dev/null || echo 0
}

get_mem_gib() {
    local mem_kb
    mem_kb="$(get_mem_kib)"
    if (( mem_kb <= 0 )); then
        echo 0
    else
        echo $(( (mem_kb + 1048575) / 1048576 ))
    fi
}

get_swap_kib() {
    awk '/SwapTotal/ {print $2}' /proc/meminfo 2>/dev/null || echo 0
}

get_cpu_model() {
    awk -F: '/model name/ {print $2; exit}' /proc/cpuinfo 2>/dev/null | sed 's/^[[:space:]]*//'
}

get_cpu_temp_c() {
    local zone
    local type
    local temp
    local max=0

    for zone in /sys/class/thermal/thermal_zone*; do
        [[ -d "$zone" ]] || continue
        type="$(cat "$zone/type" 2>/dev/null || true)"
        if ! echo "$type" | grep -Eqi 'cpu|pkg|coretemp'; then
            continue
        fi
        temp="$(cat "$zone/temp" 2>/dev/null || true)"
        if [[ "$temp" =~ ^[0-9]+$ ]]; then
            if (( temp > 1000 )); then
                temp=$(( temp / 1000 ))
            fi
            if (( temp > max )); then
                max="$temp"
            fi
        fi
    done

    echo "$max"
}

format_bytes() {
    local bytes="$1"
    local mib
    local gib
    if (( bytes <= 0 )); then
        echo "0MiB"
        return 0
    fi
    mib=$(( bytes / 1024 / 1024 ))
    gib=$(( bytes / 1024 / 1024 / 1024 ))
    if (( gib >= 1 )); then
        echo "${gib}GiB"
    else
        echo "${mib}MiB"
    fi
}

has_zram_generator() {
    if [[ -f /etc/systemd/zram-generator.conf || -f /usr/lib/systemd/zram-generator.conf ]]; then
        return 0
    fi
    if compgen -G "/etc/systemd/zram-generator.conf.d/*.conf" > /dev/null; then
        return 0
    fi
    return 1
}

has_zram() {
    [[ -d /sys/block/zram0 || -f /etc/default/zramswap ]] || has_zram_generator
}

select_best_scheduler() {
    local sched_file="$1"
    shift
    local available
    available="$(tr -d '[]' < "$sched_file" 2>/dev/null || true)"
    for cand in "$@"; do
        if echo "$available" | grep -qw "$cand"; then
            echo "$cand"
            return 0
        fi
    done
    echo ""
}

current_scheduler() {
    local sched_file="$1"
    sed -n 's/.*\[\([^]]*\)\].*/\1/p' "$sched_file" 2>/dev/null || true
}

is_on_ac() {
    local supply
    local type
    local online

    for supply in /sys/class/power_supply/*; do
        [[ -d "$supply" ]] || continue
        type="$(cat "$supply/type" 2>/dev/null || true)"
        case "$type" in
            Mains|USB)
                online="$(cat "$supply/online" 2>/dev/null || true)"
                if [[ "$online" == "1" ]]; then
                    return 0
                fi
                ;;
        esac
    done

    for supply in /sys/class/power_supply/BAT*; do
        [[ -d "$supply" ]] || continue
        case "$(cat "$supply/status" 2>/dev/null || true)" in
            Charging|Full)
                return 0
                ;;
        esac
    done

    return 1
}

select_adaptive_mode() {
    if is_on_ac; then
        log "Adaptive mode: AC detected, using dev"
        echo "dev"
    else
        log "Adaptive mode: AC not detected, using powersave"
        echo "powersave"
    fi
}

select_auto_mode() {
    CPU_MIN_PERF_OVERRIDE=""
    CPU_MAX_PERF_OVERRIDE=""
    SWAPPINESS_OVERRIDE=""
    VFS_CACHE_PRESSURE_OVERRIDE=""
    ZRAM_PERCENT_OVERRIDE=""

    local mem_gib
    local mem_kib
    local cpu_model
    local cpu_cores
    local u_series=0
    local has_battery=0
    local base_mode
    local swap_kib
    local swap_pct
    local temp_c
    local low_power_cpu=0

    mem_gib="$(get_mem_gib)"
    mem_kib="$(get_mem_kib)"
    cpu_model="$(get_cpu_model)"
    cpu_cores="$(nproc 2>/dev/null || echo 1)"

    if echo "$cpu_model" | grep -Eq '[0-9]{4,5}U'; then
        u_series=1
    fi
    if is_low_power_cpu "$cpu_model"; then
        low_power_cpu=1
    fi

    for bat in /sys/class/power_supply/BAT*; do
        if [[ -d "$bat" ]]; then
            has_battery=1
            break
        fi
    done

    if is_on_ac; then
        if [[ "$u_series" -eq 1 || "$has_battery" -eq 1 ]]; then
            base_mode="dev"
        else
            base_mode="performance"
        fi
    else
        base_mode="powersave"
    fi

    if [[ "$low_power_cpu" -eq 1 && "$base_mode" =~ ^(performance|dev)$ ]]; then
        log "Auto mode: low-power CPU detected; defaulting to balanced mode"
        base_mode="balanced"
    fi

    if [[ "$u_series" -eq 1 ]]; then
        case "$base_mode" in
            performance) CPU_MIN_PERF_OVERRIDE=45 ;;
            dev) CPU_MIN_PERF_OVERRIDE=40 ;;
            balanced) CPU_MIN_PERF_OVERRIDE=20 ;;
            powersave) CPU_MIN_PERF_OVERRIDE=5 ;;
        esac
        CPU_MAX_PERF_OVERRIDE=100
    fi

    if has_zram; then
        if (( mem_gib <= 8 )); then
            ZRAM_PERCENT_OVERRIDE=70
        elif (( mem_gib <= 16 )); then
            ZRAM_PERCENT_OVERRIDE=50
        elif (( mem_gib <= 32 )); then
            ZRAM_PERCENT_OVERRIDE=40
        else
            ZRAM_PERCENT_OVERRIDE=33
        fi
    fi

    if has_zram; then
        if (( mem_gib <= 8 )); then
            SWAPPINESS_OVERRIDE=20
        elif (( mem_gib <= 16 )); then
            SWAPPINESS_OVERRIDE=15
        else
            SWAPPINESS_OVERRIDE=10
        fi
    else
        if (( mem_gib <= 8 )); then
            SWAPPINESS_OVERRIDE=10
        else
            SWAPPINESS_OVERRIDE=5
        fi
    fi

    swap_kib="$(get_swap_kib)"
    if (( mem_kib > 0 )); then
        swap_pct=$(( swap_kib * 100 / mem_kib ))
    else
        swap_pct=0
    fi
    if [[ -n "${SWAPPINESS_OVERRIDE:-}" ]]; then
        if (( swap_kib == 0 )); then
            SWAPPINESS_OVERRIDE=1
        elif (( swap_pct < 10 )); then
            SWAPPINESS_OVERRIDE=$(( SWAPPINESS_OVERRIDE - 5 ))
        elif (( swap_pct >= 50 )); then
            SWAPPINESS_OVERRIDE=$(( SWAPPINESS_OVERRIDE + 5 ))
        fi
        SWAPPINESS_OVERRIDE="$(clamp_pct "$SWAPPINESS_OVERRIDE")"
    fi

    if [[ "$low_power_cpu" -eq 1 ]]; then
        if [[ -z "${CPU_MAX_PERF_OVERRIDE:-}" ]]; then
            CPU_MAX_PERF_OVERRIDE=85
        elif [[ "${CPU_MAX_PERF_OVERRIDE}" =~ ^[0-9]+$ ]] && (( CPU_MAX_PERF_OVERRIDE > 85 )); then
            CPU_MAX_PERF_OVERRIDE=85
        fi
        if [[ -z "${CPU_MIN_PERF_OVERRIDE:-}" ]]; then
            CPU_MIN_PERF_OVERRIDE=20
        elif [[ "${CPU_MIN_PERF_OVERRIDE}" =~ ^[0-9]+$ ]] && (( CPU_MIN_PERF_OVERRIDE < 20 )); then
            CPU_MIN_PERF_OVERRIDE=20
        fi
    fi

    temp_c="$(get_cpu_temp_c)"
    if (( temp_c >= 85 )); then
        CPU_MAX_PERF_OVERRIDE=80
        if [[ -z "${CPU_MIN_PERF_OVERRIDE:-}" || "$CPU_MIN_PERF_OVERRIDE" -gt 20 ]]; then
            CPU_MIN_PERF_OVERRIDE=20
        fi
        CPU_MIN_PERF_OVERRIDE="$(clamp_pct "$CPU_MIN_PERF_OVERRIDE")"
    elif (( temp_c >= 75 )); then
        if [[ -n "${CPU_MIN_PERF_OVERRIDE:-}" ]]; then
            CPU_MIN_PERF_OVERRIDE=$(( CPU_MIN_PERF_OVERRIDE - 10 ))
        else
            CPU_MIN_PERF_OVERRIDE=30
        fi
        CPU_MIN_PERF_OVERRIDE="$(clamp_pct "$CPU_MIN_PERF_OVERRIDE")"
    fi
    if [[ -n "${CPU_MIN_PERF_OVERRIDE:-}" && -n "${CPU_MAX_PERF_OVERRIDE:-}" ]]; then
        if (( CPU_MIN_PERF_OVERRIDE > CPU_MAX_PERF_OVERRIDE )); then
            CPU_MIN_PERF_OVERRIDE=$(( CPU_MAX_PERF_OVERRIDE - 5 ))
            CPU_MIN_PERF_OVERRIDE="$(clamp_pct "$CPU_MIN_PERF_OVERRIDE")"
        fi
    fi

    local zram_status="no"
    if has_zram; then
        zram_status="yes"
    fi
    local auto_note=""
    if [[ "$low_power_cpu" -eq 1 ]]; then
        local guard_pct="${CPU_MAX_PERF_OVERRIDE:-auto}"
        auto_note=", low-power guard (max=${guard_pct}%)"
    fi
    if (( temp_c > 0 )); then
        log "Auto mode: cpu=${cpu_model:-unknown}, cores=${cpu_cores}, mem=${mem_gib}GiB, swap=${swap_pct}%, temp=${temp_c}C, zram=${zram_status}, base=${base_mode}${auto_note}"
    else
        log "Auto mode: cpu=${cpu_model:-unknown}, cores=${cpu_cores}, mem=${mem_gib}GiB, swap=${swap_pct}%, zram=${zram_status}, base=${base_mode}${auto_note}"
    fi
    echo "$base_mode"
}

configure_battery_limits() {
    local start="$BATTERY_START_THRESH"
    local stop="$BATTERY_STOP_THRESH"
    local applied=0

    log "Configuring battery charge thresholds (${start}-${stop}%)"

    if [[ ${EUID:-0} -ne 0 ]]; then
        warn "Skipping battery thresholds (need root)"
        return 0
    fi

    if [[ "$start" -ge "$stop" ]]; then
        warn "Invalid battery thresholds: start >= stop"
        return 0
    fi

    local battery_paths=()
    local battery_names=()
    for bat in /sys/class/power_supply/BAT*; do
        [[ -d "$bat" ]] || continue
        battery_paths+=("$bat")
        battery_names+=("$(basename "$bat")")
    done

    for bat in "${battery_paths[@]}"; do
        if [[ -w "$bat/charge_control_start_threshold" && -w "$bat/charge_control_end_threshold" ]]; then
            echo "$start" > "$bat/charge_control_start_threshold" 2>/dev/null || true
            echo "$stop" > "$bat/charge_control_end_threshold" 2>/dev/null || true
            log "Set thresholds via sysfs for $(basename "$bat")"
            applied=1
        fi
    done

    if command -v tlp >/dev/null 2>&1; then
        local conf="/etc/tlp.conf"
        mkdir -p "$BACKUP_DIR"
        if [[ -f "$conf" ]]; then
            cp -a "$conf" "$BACKUP_DIR/tlp.conf.bak" 2>/dev/null || true
        fi

        if [[ "${#battery_names[@]}" -gt 0 ]]; then
            for bat_name in "${battery_names[@]}"; do
                set_config_kv "$conf" "START_CHARGE_THRESH_${bat_name}" "$start"
                set_config_kv "$conf" "STOP_CHARGE_THRESH_${bat_name}" "$stop"
            done
        else
            set_config_kv "$conf" "START_CHARGE_THRESH_BAT0" "$start"
            set_config_kv "$conf" "STOP_CHARGE_THRESH_BAT0" "$stop"
            set_config_kv "$conf" "START_CHARGE_THRESH_BAT1" "$start"
            set_config_kv "$conf" "STOP_CHARGE_THRESH_BAT1" "$stop"
        fi

        if [[ "${#battery_names[@]}" -gt 0 ]]; then
            tlp setcharge "$start" "$stop" "${battery_names[@]}" >/dev/null 2>&1 || true
        else
            tlp setcharge "$start" "$stop" >/dev/null 2>&1 || true
        fi

        if command -v systemctl >/dev/null 2>&1; then
            systemctl enable tlp.service >/dev/null 2>&1 || true
        fi

        log "Updated TLP thresholds (${start}-${stop}%)"
        applied=1
    fi

    if [[ "$applied" -eq 0 ]]; then
        warn "Battery charge threshold controls not available"
    fi
}

# Check if running as root for operations that need it
check_root() {
    if [[ ${EUID:-0} -ne 0 ]]; then
        error "This operation requires root privileges. Use sudo."
        exit 1
    fi
}

# Show system status
show_status() {
    log "=== SYSTEM STATUS ==="
    
    echo -e "\n${BLUE}CPU & Power Profile:${NC}"
    if have_cmd powerprofilesctl; then
        powerprofilesctl get || echo "Power profiles not available"
    else
        echo "Power profiles not available"
        grep -H . /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor 2>/dev/null | head -n 2 || true
    fi
    
    echo -e "\n${BLUE}CPU Frequency:${NC}"
    if have_cmd cpupower; then
        cpupower frequency-info || true
    else
        grep -m2 'cpu MHz' /proc/cpuinfo | awk '{print $4}' | tr '\n' ' '; echo
    fi

    if [[ -r /sys/devices/system/cpu/intel_pstate/status ]]; then
        echo -e "\n${BLUE}intel_pstate:${NC}"
        printf "status: %s\n" "$(cat /sys/devices/system/cpu/intel_pstate/status 2>/dev/null || true)"
        printf "min/max perf pct: %s/%s\n" \
            "$(cat /sys/devices/system/cpu/intel_pstate/min_perf_pct 2>/dev/null || true)" \
            "$(cat /sys/devices/system/cpu/intel_pstate/max_perf_pct 2>/dev/null || true)"
        printf "no_turbo: %s\n" "$(cat /sys/devices/system/cpu/intel_pstate/no_turbo 2>/dev/null || true)"
    fi

    echo -e "\n${BLUE}CPU Temperature:${NC}"
    local temp_c
    temp_c="$(get_cpu_temp_c)"
    if (( temp_c > 0 )); then
        echo "${temp_c}C"
    else
        echo "unavailable"
    fi
    
    echo -e "\n${BLUE}Memory Usage:${NC}"
    free -h

    echo -e "\n${BLUE}Memory Tuning:${NC}"
    printf "swappiness: %s\n" "$(cat /proc/sys/vm/swappiness 2>/dev/null || echo n/a)"
    printf "vfs_cache_pressure: %s\n" "$(cat /proc/sys/vm/vfs_cache_pressure 2>/dev/null || echo n/a)"
    
    echo -e "\n${BLUE}Swap Usage:${NC}"
    swapon --show

    echo -e "\n${BLUE}Zram:${NC}"
    if [[ -d /sys/block/zram0 ]]; then
        local zdev
        local zsize
        for zdev in /sys/block/zram*; do
            [[ -d "$zdev" ]] || continue
            zsize="$(cat "$zdev/disksize" 2>/dev/null || echo 0)"
            echo "$(basename "$zdev"): size=$(format_bytes "$zsize")"
        done
    elif [[ -f /etc/default/zramswap ]] || has_zram_generator; then
        echo "configured (no active device)"
    else
        echo "not detected"
    fi

    echo -e "\n${BLUE}I/O Schedulers:${NC}"
    local dev_path
    local name
    local rota
    local sched_file
    local current
    for dev_path in /sys/block/*; do
        name="$(basename "$dev_path")"
        case "$name" in
            loop*|ram*|zram*|dm-*|md*|sr*)
                continue
                ;;
        esac
        sched_file="$dev_path/queue/scheduler"
        [[ -r "$sched_file" ]] || continue
        current="$(current_scheduler "$sched_file")"
        rota="$(cat "$dev_path/queue/rotational" 2>/dev/null || echo 1)"
        echo "${name}: ${current:-unknown} (rotational=${rota})"
    done

    if [[ -f "$IO_UDEV_RULE" ]]; then
        echo "udev rule: installed"
    else
        echo "udev rule: not installed"
    fi

    echo -e "\n${BLUE}Battery Thresholds:${NC}"
    for bat in /sys/class/power_supply/BAT*; do
        [[ -d "$bat" ]] || continue
        if [[ -r "$bat/charge_control_start_threshold" && -r "$bat/charge_control_end_threshold" ]]; then
            printf "%s: start=%s stop=%s\n" "$(basename "$bat")" \
                "$(cat "$bat/charge_control_start_threshold" 2>/dev/null || true)" \
                "$(cat "$bat/charge_control_end_threshold" 2>/dev/null || true)"
        else
            echo "$(basename "$bat"): threshold controls not available"
        fi
    done
    
    echo -e "\n${BLUE}Top CPU Processes:${NC}"
    ps -eo pid,ppid,cmd,%mem,%cpu --sort=-%cpu | head -n 10
    
    echo -e "\n${BLUE}Active Services:${NC}"
    for service in tracker-miner-fs-3 tracker-extract-3 packagekit snapd apt-daily; do
        if systemctl -q is-active "$service" 2>/dev/null; then
            echo "  • $service: active"
        fi
    done
    
    echo -e "\n${BLUE}GNOME Extensions:${NC}"
    if command -v gnome-extensions >/dev/null 2>&1; then
        echo -n "Enabled extensions: "
        run_as_user gnome-extensions list --enabled 2>/dev/null | wc -l || true
        echo -n "Total extensions: "
        run_as_user gnome-extensions list 2>/dev/null | wc -l || true
    fi
}

# Optimize CPU performance
optimize_cpu() {
    local mode="$1"
    log "Optimizing CPU for $mode mode"
    
    local min_perf_pct
    local max_perf_pct

    case "$mode" in
        performance|dev)
            # Set performance governor
            if command -v powerprofilesctl >/dev/null 2>&1; then
                powerprofilesctl set performance || true
            fi
            
            if command -v cpupower >/dev/null 2>&1; then
                cpupower frequency-set -g performance || true
            fi
            
            # Direct governor setting fallback
            for gov in /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor; do
                [[ -w "$gov" ]] && echo performance > "$gov" || true
            done
            
            # Intel HWP EPP
            for f in /sys/devices/system/cpu/cpu*/cpufreq/energy_performance_preference; do
                [[ -w "$f" ]] && echo performance > "$f" || true
            done

            if [[ "$mode" == "dev" ]]; then
                min_perf_pct="$(clamp_pct 45)"
            else
                min_perf_pct="$(clamp_pct 50)"
            fi
            max_perf_pct="$(clamp_pct 100)"
            [[ -w /sys/devices/system/cpu/intel_pstate/min_perf_pct ]] && echo "$min_perf_pct" > /sys/devices/system/cpu/intel_pstate/min_perf_pct || true
            [[ -w /sys/devices/system/cpu/intel_pstate/max_perf_pct ]] && echo "$max_perf_pct" > /sys/devices/system/cpu/intel_pstate/max_perf_pct || true
            [[ -w /sys/devices/system/cpu/intel_pstate/no_turbo ]] && echo 0 > /sys/devices/system/cpu/intel_pstate/no_turbo || true
            [[ -w /sys/devices/system/cpu/intel_pstate/hwp_dynamic_boost ]] && echo 1 > /sys/devices/system/cpu/intel_pstate/hwp_dynamic_boost || true
            ;;
            
        balanced)
            if command -v powerprofilesctl >/dev/null 2>&1; then
                powerprofilesctl set balanced || true
            fi

            for f in /sys/devices/system/cpu/cpu*/cpufreq/energy_performance_preference; do
                [[ -w "$f" ]] && echo balance_performance > "$f" || true
            done

            min_perf_pct="$(clamp_pct 15)"
            max_perf_pct="$(clamp_pct 100)"
            [[ -w /sys/devices/system/cpu/intel_pstate/min_perf_pct ]] && echo "$min_perf_pct" > /sys/devices/system/cpu/intel_pstate/min_perf_pct || true
            [[ -w /sys/devices/system/cpu/intel_pstate/max_perf_pct ]] && echo "$max_perf_pct" > /sys/devices/system/cpu/intel_pstate/max_perf_pct || true
            [[ -w /sys/devices/system/cpu/intel_pstate/no_turbo ]] && echo 0 > /sys/devices/system/cpu/intel_pstate/no_turbo || true
            [[ -w /sys/devices/system/cpu/intel_pstate/hwp_dynamic_boost ]] && echo 0 > /sys/devices/system/cpu/intel_pstate/hwp_dynamic_boost || true
            ;;
            
        powersave)
            if command -v powerprofilesctl >/dev/null 2>&1; then
                powerprofilesctl set power-saver || true
            fi
            
            if command -v cpupower >/dev/null 2>&1; then
                cpupower frequency-set -g powersave || true
            fi
            
            for gov in /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor; do
                [[ -w "$gov" ]] && echo powersave > "$gov" || true
            done

            for f in /sys/devices/system/cpu/cpu*/cpufreq/energy_performance_preference; do
                [[ -w "$f" ]] && echo power > "$f" || true
            done

            min_perf_pct="$(clamp_pct 5)"
            max_perf_pct="$(clamp_pct 60)"
            [[ -w /sys/devices/system/cpu/intel_pstate/min_perf_pct ]] && echo "$min_perf_pct" > /sys/devices/system/cpu/intel_pstate/min_perf_pct || true
            [[ -w /sys/devices/system/cpu/intel_pstate/max_perf_pct ]] && echo "$max_perf_pct" > /sys/devices/system/cpu/intel_pstate/max_perf_pct || true
            [[ -w /sys/devices/system/cpu/intel_pstate/no_turbo ]] && echo 1 > /sys/devices/system/cpu/intel_pstate/no_turbo || true
            [[ -w /sys/devices/system/cpu/intel_pstate/hwp_dynamic_boost ]] && echo 0 > /sys/devices/system/cpu/intel_pstate/hwp_dynamic_boost || true
            ;;
    esac

    if [[ -n "${CPU_MIN_PERF_OVERRIDE:-}" ]]; then
        min_perf_pct="$(clamp_pct "$CPU_MIN_PERF_OVERRIDE")"
    fi
    if [[ -n "${CPU_MAX_PERF_OVERRIDE:-}" ]]; then
        max_perf_pct="$(clamp_pct "$CPU_MAX_PERF_OVERRIDE")"
    fi
    if [[ -n "${CPU_MIN_PERF_OVERRIDE:-}" || -n "${CPU_MAX_PERF_OVERRIDE:-}" ]]; then
        [[ -w /sys/devices/system/cpu/intel_pstate/min_perf_pct ]] && echo "$min_perf_pct" > /sys/devices/system/cpu/intel_pstate/min_perf_pct || true
        [[ -w /sys/devices/system/cpu/intel_pstate/max_perf_pct ]] && echo "$max_perf_pct" > /sys/devices/system/cpu/intel_pstate/max_perf_pct || true
    fi
}

# Optimize developer limits for heavy parallel workloads
optimize_dev_limits() {
    log "Optimizing developer limits"

    set_sysctl fs.inotify.max_user_watches 1048576
    set_sysctl fs.inotify.max_user_instances 1024
    set_sysctl fs.inotify.max_queued_events 65536
    set_sysctl fs.file-max 2097152
    set_sysctl vm.max_map_count 262144
}

# Optimize memory and swap
optimize_memory() {
    local mode="$1"
    local swappiness
    local vfs_cache_pressure

    log "Optimizing memory settings"
    
    # Optimize zram if available
    if [[ -f /etc/default/zramswap && ${EUID:-0} -eq 0 ]]; then
        local zram_percent
        if [[ -n "${ZRAM_PERCENT_OVERRIDE:-}" ]]; then
            zram_percent="$(clamp_pct "$ZRAM_PERCENT_OVERRIDE")"
        else
            zram_percent="$(calc_zram_percent)"
        fi
        cp -a /etc/default/zramswap "$BACKUP_DIR/zramswap.bak" 2>/dev/null || true
        sed -i 's/^#\?ALGO=.*/ALGO=zstd/' /etc/default/zramswap
        sed -i "s/^#\?PERCENT=.*/PERCENT=${zram_percent}/" /etc/default/zramswap
        sed -i 's/^#\?PRIORITY=.*/PRIORITY=100/' /etc/default/zramswap
        if have_cmd systemctl; then
            systemctl restart zramswap.service || true
        else
            warn "systemctl not available; skipped zramswap restart"
        fi
        log "Optimized zram with zstd algorithm (${zram_percent}% of RAM)"
    elif [[ -f /etc/default/zramswap ]]; then
        warn "Skipping zram config (need root)"
    elif has_zram_generator; then
        if [[ ${EUID:-0} -eq 0 ]]; then
            log "zram-generator detected; leaving config unchanged"
        else
            warn "zram-generator detected (need root for any changes)"
        fi
    elif [[ -d /sys/block/zram0 ]]; then
        log "zram device detected; leaving config unchanged"
    fi
    
    if [[ "$DROP_CACHES" -eq 1 ]]; then
        if [[ ${EUID:-0} -eq 0 ]]; then
            sync
            echo 3 > /proc/sys/vm/drop_caches || true
            log "Dropped page cache (DROP_CACHES=1)"
        else
            warn "Skipping drop_caches (need root)"
        fi
    fi

    case "$mode" in
        performance|dev)
            swappiness=5
            vfs_cache_pressure=50
            ;;
        balanced)
            swappiness=10
            vfs_cache_pressure=75
            ;;
        powersave)
            swappiness=20
            vfs_cache_pressure=100
            ;;
        *)
            swappiness=10
            vfs_cache_pressure=100
            ;;
    esac

    if [[ -n "${SWAPPINESS_OVERRIDE:-}" ]]; then
        swappiness="$(clamp_pct "$SWAPPINESS_OVERRIDE")"
    fi
    if [[ -n "${VFS_CACHE_PRESSURE_OVERRIDE:-}" ]]; then
        vfs_cache_pressure="$(clamp_pct "$VFS_CACHE_PRESSURE_OVERRIDE")"
    fi
    set_sysctl vm.swappiness "$swappiness"
    
    # Optimize dirty ratios for SSD
    set_sysctl vm.dirty_ratio 10
    set_sysctl vm.dirty_background_ratio 5
    set_sysctl vm.vfs_cache_pressure "$vfs_cache_pressure"

    case "$mode" in
        performance|dev)
            optimize_dev_limits
            ;;
    esac
}

tune_io_schedulers() {
    if [[ ${EUID:-0} -ne 0 ]]; then
        warn "Skipping I/O scheduler tuning (need root)"
        return 0
    fi

    log "Tuning I/O schedulers"

    local dev_path
    local name
    local rota
    local sched_file
    local current
    local desired

    for dev_path in /sys/block/*; do
        name="$(basename "$dev_path")"
        case "$name" in
            loop*|ram*|zram*|dm-*|md*|sr*)
                continue
                ;;
        esac

        sched_file="$dev_path/queue/scheduler"
        [[ -w "$sched_file" ]] || continue
        rota="$(cat "$dev_path/queue/rotational" 2>/dev/null || echo 1)"

        if [[ "$name" == nvme* ]]; then
            desired="$(select_best_scheduler "$sched_file" none kyber mq-deadline deadline)"
        elif [[ "$rota" == "0" ]]; then
            desired="$(select_best_scheduler "$sched_file" mq-deadline none kyber deadline)"
        else
            desired="$(select_best_scheduler "$sched_file" bfq mq-deadline deadline none)"
        fi

        [[ -n "$desired" ]] || continue
        current="$(current_scheduler "$sched_file")"
        if [[ -n "$current" && "$current" == "$desired" ]]; then
            continue
        fi

        echo "$desired" > "$sched_file" 2>/dev/null || true
        current="$(current_scheduler "$sched_file")"
        if [[ "$current" == "$desired" ]]; then
            log "Set I/O scheduler for $name to $desired"
        else
            warn "Failed to set I/O scheduler for $name to $desired"
        fi
    done

    persist_io_schedulers
}

persist_io_schedulers() {
    if [[ ${EUID:-0} -ne 0 ]]; then
        warn "Skipping I/O scheduler persistence (need root)"
        return 0
    fi

    local helper="$IO_HELPER"
    local rule="$IO_UDEV_RULE"

    cat > "$helper" << 'EOF'
#!/usr/bin/env bash
set -euo pipefail

dev="${1:-}"
[[ -n "$dev" ]] || exit 0

case "$dev" in
    loop*|ram*|zram*|dm-*|md*|sr*)
        exit 0
        ;;
esac

sched_file="/sys/block/$dev/queue/scheduler"
[[ -w "$sched_file" ]] || exit 0

available="$(tr -d '[]' < "$sched_file" 2>/dev/null || true)"
choose() {
    for cand in "$@"; do
        if echo "$available" | grep -qw "$cand"; then
            echo "$cand"
            return 0
        fi
    done
    echo ""
}

rota="$(cat /sys/block/$dev/queue/rotational 2>/dev/null || echo 1)"
if [[ "$dev" == nvme* ]]; then
    desired="$(choose none kyber mq-deadline deadline)"
elif [[ "$rota" == "0" ]]; then
    desired="$(choose mq-deadline none kyber deadline)"
else
    desired="$(choose bfq mq-deadline deadline none)"
fi

[[ -n "$desired" ]] || exit 0
current="$(sed -n 's/.*\\[\\([^]]*\\)\\].*/\\1/p' "$sched_file" 2>/dev/null || true)"
if [[ "$current" != "$desired" ]]; then
    echo "$desired" > "$sched_file" 2>/dev/null || true
fi
EOF

    chmod 0755 "$helper" 2>/dev/null || true

    cat > "$rule" << EOF
# Supershell I/O scheduler tuning
ACTION=="add|change", SUBSYSTEM=="block", ENV{DEVTYPE}=="disk", KERNEL=="sd*", RUN+="${helper} %k"
ACTION=="add|change", SUBSYSTEM=="block", ENV{DEVTYPE}=="disk", KERNEL=="nvme*", RUN+="${helper} %k"
ACTION=="add|change", SUBSYSTEM=="block", ENV{DEVTYPE}=="disk", KERNEL=="mmcblk*", RUN+="${helper} %k"
ACTION=="add|change", SUBSYSTEM=="block", ENV{DEVTYPE}=="disk", KERNEL=="vd*", RUN+="${helper} %k"
ACTION=="add|change", SUBSYSTEM=="block", ENV{DEVTYPE}=="disk", KERNEL=="xvd*", RUN+="${helper} %k"
EOF

    if have_cmd udevadm; then
        udevadm control --reload-rules >/dev/null 2>&1 || true
        udevadm trigger --type=devices --subsystem-match=block >/dev/null 2>&1 || true
    fi

    log "Installed udev rule for I/O scheduler persistence"
}

reset_swap() {
    log "Resetting swap"

    if [[ ${EUID:-0} -ne 0 ]]; then
        warn "Skipping swap reset (need root)"
        return 0
    fi

    if ! have_cmd swapoff || ! have_cmd swapon; then
        warn "swapoff/swapon not available; skipping swap reset"
        return 0
    fi

    if ! swapon --show --noheadings | grep -q .; then
        warn "No active swap devices; skipping swap reset"
        return 0
    fi

    swapoff -a >/dev/null 2>&1 || { warn "swapoff failed"; return 0; }
    swapon -a >/dev/null 2>&1 || { warn "swapon failed"; return 0; }
    log "Swap reset complete"
}

silence_snapd() {
    if [[ ${EUID:-0} -ne 0 ]]; then
        warn "Skipping snapd quiet (need root)"
        return 0
    fi

    if ! have_cmd systemctl; then
        warn "systemctl not available; skipping snapd quiet"
        return 0
    fi

    systemctl stop snapd.service snapd.socket >/dev/null 2>&1 || true
    systemctl disable snapd.service snapd.socket >/dev/null 2>&1 || true
    systemctl mask snapd.service snapd.socket >/dev/null 2>&1 || true

    mkdir -p "$STATE_DIR" 2>/dev/null || true
    printf "%s\n" "$(date -Is)" > "$SNAPD_MARKER" 2>/dev/null || true

    log "snapd masked (service and socket)"
}

restore_snapd() {
    local force="${1:-0}"

    if [[ ${EUID:-0} -ne 0 ]]; then
        warn "Skipping snapd restore (need root)"
        return 0
    fi

    if ! have_cmd systemctl; then
        warn "systemctl not available; skipping snapd restore"
        return 0
    fi

    if [[ "$force" -ne 1 && ! -f "$SNAPD_MARKER" ]]; then
        warn "snapd not managed by supershell; skipping restore"
        return 0
    fi

    systemctl unmask snapd.service snapd.socket >/dev/null 2>&1 || true
    systemctl enable snapd.service snapd.socket >/dev/null 2>&1 || true
    systemctl start snapd.socket >/dev/null 2>&1 || true
    systemctl start snapd.service >/dev/null 2>&1 || true

    rm -f "$SNAPD_MARKER" 2>/dev/null || true

    log "snapd restored"
}

# Stop resource-heavy services
stop_heavy_services() {
    log "Stopping heavy background services"
    
    # User services
    run_as_user systemctl --user stop tracker-miner-fs-3.service tracker-extract-3.service tracker-writeback-3.service 2>/dev/null || true
    
    # System services
    if [[ ${EUID:-0} -eq 0 ]]; then
        for service in packagekit.service apt-daily.service apt-daily.timer apt-daily-upgrade.service apt-daily-upgrade.timer whoopsie.service; do
            systemctl -q is-active "$service" && systemctl stop "$service" || true
        done

        if [[ "$SNAPD_QUIET" -eq 1 ]]; then
            silence_snapd
        else
            for service in snapd.service snapd.socket; do
                systemctl -q is-active "$service" && systemctl stop "$service" || true
            done
        fi
    else
        warn "Skipping system services (need root)"
    fi
    
    # Lower priority of stubborn processes
    for proc in tracker-miner-fs tracker-extract packagekitd updatedb; do
        local pids
        pids="$(pgrep -x "$proc" 2>/dev/null || true)"
        if [[ -n "$pids" ]]; then
            renice +19 -p $pids >/dev/null 2>&1 || true
            ionice -c3 -p $pids >/dev/null 2>&1 || true
        fi
    done
    
    log "Heavy services stopped and prioritized"
}

throttle_io() {
    log "Throttling background I/O"

    local use_ionice=0
    if have_cmd ionice; then
        use_ionice=1
    else
        warn "ionice not available; using renice only"
    fi

    if ! have_cmd renice; then
        warn "renice not available; skipping I/O throttle"
        return 0
    fi

    local proc
    local pids
    local targets=(
        tracker-miner-fs
        tracker-extract
        tracker-writeback
        packagekitd
        snapd
        updatedb
        locate
        mlocate
        baloo_file
        baloo_file_extractor
    )

    for proc in "${targets[@]}"; do
        pids="$(pgrep -x "$proc" 2>/dev/null || true)"
        if [[ -n "$pids" ]]; then
            renice +19 -p $pids >/dev/null 2>&1 || true
            if [[ "$use_ionice" -eq 1 ]]; then
                ionice -c3 -p $pids >/dev/null 2>&1 || true
            fi
        fi
    done

    log "I/O throttle applied"
}

# Restore services
restore_services() {
    log "Restoring background services"
    
    # System services
    if [[ ${EUID:-0} -eq 0 ]]; then
        for service in packagekit.service snapd.service snapd.socket apt-daily.service apt-daily.timer apt-daily-upgrade.service apt-daily-upgrade.timer; do
            systemctl -q is-enabled "$service" >/dev/null 2>&1 && systemctl start "$service" || true
        done
    else
        warn "Skipping system services restore (need root)"
    fi
    
    # User services
    run_as_user systemctl --user start tracker-miner-fs-3.service tracker-extract-3.service tracker-writeback-3.service 2>/dev/null || true
    
    log "Services restored"
}

# Optimize GNOME desktop
optimize_gnome() {
    local mode="$1"
    log "Optimizing GNOME for $mode mode"
    local has_gsettings=0
    
    # Backup current settings
    mkdir -p "$BACKUP_DIR"
    if have_cmd dconf; then
        run_as_user dconf dump / > "$BACKUP_DIR/dconf_full_backup.ini" 2>/dev/null || true
    else
        warn "dconf not available; skipping GNOME backup"
    fi

    if have_cmd gsettings; then
        has_gsettings=1
    else
        warn "gsettings not available; skipping GNOME tweaks"
    fi
    
    case "$mode" in
        performance|dev)
            # Disable animations
            if [[ "$has_gsettings" -eq 1 ]]; then
                run_as_user gsettings set org.gnome.desktop.interface enable-animations false || true
            fi
            
            # Disable effects
            if have_cmd gnome-extensions; then
                while IFS= read -r ext; do
                    [[ -n "$ext" ]] || continue
                    run_as_user gnome-extensions disable "$ext" || true
                done < <(run_as_user gnome-extensions list 2>/dev/null | grep -iE 'burn|jelly|magic|lamp|wobbl|compiz|effect' || true)
            fi
            
            # Optimize Nautilus
            if [[ "$has_gsettings" -eq 1 ]]; then
                run_as_user gsettings set org.gnome.nautilus.preferences show-image-thumbnails 'never' || true
                run_as_user gsettings set org.gnome.nautilus.preferences thumbnail-limit 1048576 || true
            fi
            ;;
            
        balanced)
            if [[ "$has_gsettings" -eq 1 ]]; then
                run_as_user gsettings set org.gnome.desktop.interface enable-animations true || true
                run_as_user gsettings set org.gnome.nautilus.preferences show-image-thumbnails 'local-only' || true
            fi
            ;;
            
        powersave)
            if [[ "$has_gsettings" -eq 1 ]]; then
                run_as_user gsettings set org.gnome.desktop.interface enable-animations false || true
                run_as_user gsettings set org.gnome.nautilus.preferences show-image-thumbnails 'never' || true
            fi
            # Additional power saving settings
            if [[ "$has_gsettings" -eq 1 ]]; then
                run_as_user gsettings set org.gnome.desktop.session idle-delay 300 || true
                run_as_user gsettings set org.gnome.settings-daemon.plugins.power sleep-inactive-ac-timeout 900 || true
            fi
            ;;
    esac
    
    log "GNOME settings optimized"
}

# Optimize storage drives
optimize_storage() {
    check_root
    log "Optimizing storage drives"
    
    # Enable weekly TRIM for SSDs
    if have_cmd systemctl && systemctl list-unit-files | grep -q '^fstrim.timer'; then
        systemctl enable --now fstrim.timer >/dev/null 2>&1 || true
        log "Enabled fstrim.timer for SSD optimization"
    fi
    
    # Enumerate and optimize filesystems
    while read -r name fstype mount ro_flag rota; do
        [[ -z "$mount" || "$mount" == "/" || "$mount" =~ ^/(sys|proc|run|dev) ]] && continue
        [[ "$ro_flag" == "1" ]] && continue
        
        log "Optimizing $name ($fstype) at $mount"
        
        # TRIM SSDs
        if [[ "$rota" == "0" ]] && command -v fstrim >/dev/null 2>&1; then
            fstrim -v "$mount" 2>/dev/null || true
        fi
        
        # Filesystem-specific optimizations
        case "$fstype" in
            ext4)
                if command -v e4defrag >/dev/null 2>&1; then
                    SCORE=$(e4defrag -c "$mount" 2>/dev/null | awk '/Fragmentation score/ {print $3}' || echo 0)
                    if [[ "${SCORE:-0}" -ge 30 ]]; then
                        e4defrag -v "$mount" 2>/dev/null || true
                        log "Defragmented ext4 filesystem (score: $SCORE)"
                    fi
                fi
                ;;
            xfs)
                if command -v xfs_fsr >/dev/null 2>&1; then
                    xfs_fsr -v "$mount" 2>/dev/null || true
                fi
                ;;
            btrfs)
                if command -v btrfs >/dev/null 2>&1; then
                    btrfs scrub start -Bd "$mount" 2>/dev/null || true
                fi
                ;;
        esac
    done < <(lsblk -nrpo NAME,FSTYPE,MOUNTPOINT,RO,ROTA | awk '$3!=""')
    
    log "Storage optimization complete"
}

# Clean system caches
clean_system() {
    log "Cleaning system caches"
    
    # GNOME caches
    rm -rf "$REAL_HOME/.cache/gnome-shell" "$REAL_HOME/.cache/mesa_shader_cache" "$REAL_HOME/.cache/fontconfig" "$REAL_HOME/.cache/thumbnails"/* 2>/dev/null || true
    
    # Tracker caches
    rm -rf "$REAL_HOME/.cache/tracker" "$REAL_HOME/.cache/tracker3" "$REAL_HOME/.local/share/tracker" "$REAL_HOME/.local/share/tracker3" 2>/dev/null || true
    
    # Package cache (if root)
    if [[ ${EUID:-0} -eq 0 ]]; then
        if have_cmd apt-get; then
            apt-get clean 2>/dev/null || true
        fi
        if have_cmd journalctl; then
            journalctl --vacuum-size=100M 2>/dev/null || true
        fi
    fi
    
    log "System caches cleaned"
}

# Apply all optimizations
apply_optimizations() {
    local mode="$1"
    local display_mode="$mode"

    if [[ "$mode" == "dev" && "$NO_CLEAN_SET" -eq 0 ]]; then
        NO_CLEAN=1
    fi

    if [[ "${REQUESTED_MODE:-}" == "adaptive" ]]; then
        display_mode="adaptive ($mode)"
    elif [[ "${REQUESTED_MODE:-}" == "auto" ]]; then
        display_mode="auto ($mode)"
    fi
    
    log "=== APPLYING $display_mode OPTIMIZATIONS ==="
    log "Timestamp: $(date -Is)"
    
    configure_battery_limits
    optimize_cpu "$mode"
    optimize_memory "$mode"
    tune_io_schedulers
    if [[ "$DROP_SWAP" -eq 1 ]]; then
        reset_swap
    fi
    stop_heavy_services
    if [[ "$IO_THROTTLE" -eq 1 ]]; then
        throttle_io
    fi
    optimize_gnome "$mode"
    if [[ "$NO_CLEAN" -eq 0 ]]; then
        clean_system
    else
        log "Skipping cache cleaning"
    fi
    
    # Storage optimization requires root and is optional for quick runs
    if [[ ${EUID:-0} -eq 0 ]] && [[ "$OPT_QUICK" -eq 0 ]]; then
        optimize_storage
    fi
    
    log "=== OPTIMIZATION COMPLETE ==="
    
    # Show summary
    echo -e "\n${GREEN}Optimization applied successfully!${NC}"
    echo -e "Mode: ${BLUE}$display_mode${NC}"
    echo -e "Backup saved to: ${YELLOW}$BACKUP_DIR${NC}"
    echo -e "Log file: ${YELLOW}$LOG_FILE${NC}"
    
    # Quick performance hint
    echo -e "\n${BLUE}Quick performance check:${NC}"
    echo -n "CPU Governor: "
    local governor
    governor="$(grep -H . /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor 2>/dev/null | head -n 1 | cut -d: -f2 || true)"
    if [[ -n "${governor:-}" ]]; then
        echo "$governor"
    else
        echo "unknown"
    fi
    echo -n "Memory Usage: "
    free | awk '/^Mem:/ {printf("%.1f%%\n", $3/$2 * 100.0)}'
}

# Restore original settings
restore_settings() {
    log "Restoring original settings"
    
    if [[ -d "$BACKUP_DIR" ]] && [[ -f "$BACKUP_DIR/dconf_full_backup.ini" ]]; then
        run_as_user dconf load / < "$BACKUP_DIR/dconf_full_backup.ini" || true
        log "GNOME settings restored from backup"
    else
        warn "No backup found to restore from"
    fi
    
    restore_snapd
    restore_services

    if [[ ${EUID:-0} -eq 0 && -f "$SYSCTL_CONF" ]]; then
        rm -f "$SYSCTL_CONF" 2>/dev/null || true
        if have_cmd sysctl; then
            sysctl --system >/dev/null 2>&1 || sysctl -p >/dev/null 2>&1 || true
        fi
        log "Removed persistent sysctl overrides"
    fi

    if [[ ${EUID:-0} -eq 0 ]]; then
        if [[ -f "$IO_UDEV_RULE" ]]; then
            rm -f "$IO_UDEV_RULE" 2>/dev/null || true
            if have_cmd udevadm; then
                udevadm control --reload-rules >/dev/null 2>&1 || true
            fi
            log "Removed I/O scheduler udev rule"
        fi
        if [[ -f "$IO_HELPER" ]]; then
            rm -f "$IO_HELPER" 2>/dev/null || true
            log "Removed I/O scheduler helper"
        fi
    fi
    
    # Restore balanced power profile
    optimize_cpu balanced
    
    log "Settings restored to original state"
}

install_startup() {
    local mode="adaptive"
    if [[ $# -gt 0 ]]; then
        case "$1" in
            adaptive|auto|performance|dev|balanced|powersave)
                mode="$1"
                shift
                ;;
        esac
    fi
    local opts=("$@")
    local opts_str=""
    if [[ "${#opts[@]}" -gt 0 ]]; then
        opts_str="${opts[*]}"
    fi

    check_root

    local script_path=""
    if command -v readlink >/dev/null 2>&1; then
        script_path="$(readlink -f "$0" 2>/dev/null || true)"
    fi
    if [[ -z "${script_path:-}" ]] && command -v realpath >/dev/null 2>&1; then
        script_path="$(realpath "$0" 2>/dev/null || true)"
    fi
    script_path="${script_path:-$0}"

    local env_file="/etc/default/supershell"
    local unit_file="/etc/systemd/system/supershell.service"
    local user_name="${SUDO_USER:-$USER}"

    cat > "$env_file" << EOF
# Supershell startup settings
SUPERSHELL_USER="${user_name}"
SUPERSHELL_MODE="${mode}"
SUPERSHELL_OPTS="${opts_str}"
EOF

    cat > "$unit_file" << EOF
[Unit]
Description=Supershell startup optimization
After=graphical.target systemd-user-sessions.service
Wants=graphical.target

[Service]
Type=oneshot
EnvironmentFile=-/etc/default/supershell
ExecStart="${script_path}" \$SUPERSHELL_MODE \$SUPERSHELL_OPTS
RemainAfterExit=yes

[Install]
WantedBy=graphical.target
EOF

    if have_cmd systemctl; then
        systemctl daemon-reload >/dev/null 2>&1 || true
        systemctl enable supershell.service >/dev/null 2>&1 || true
    fi

    log "Installed supershell startup service"
    log "Edit ${env_file} to change mode/options"
}

remove_startup() {
    check_root

    local env_file="/etc/default/supershell"
    local unit_file="/etc/systemd/system/supershell.service"

    if have_cmd systemctl; then
        systemctl disable --now supershell.service >/dev/null 2>&1 || true
        systemctl daemon-reload >/dev/null 2>&1 || true
    fi

    rm -f "$unit_file" "$env_file" 2>/dev/null || true
    log "Removed supershell startup service"
}

# Show help
show_help() {
    cat << EOF
${GREEN}Supershell - Ultimate Laptop Optimization${NC}

Usage: sudo $SCRIPT_NAME [MODE] [OPTIONS]

${BLUE}MODES:${NC}
  adaptive      Dev on AC, powersave on battery
  auto          Hardware-aware auto tuning
  performance   Max performance (high power usage)
  dev           Sustained performance for heavy development workloads
  balanced      Balanced performance and power
  powersave     Maximum power saving
  status        Show current system status
  
${BLUE}OPTIONS:${NC}
  --quick         Skip storage optimization (faster)
  --drop-caches   Drop page cache (requires root)
  --drop-swap     Reset swap usage (requires root)
  --io-throttle   Throttle background I/O hogs (ionice/renice)
  --snapd-quiet   Disable/mask snapd.service and snapd.socket
  --no-clean      Skip cache cleaning
  --clean         Force cache cleaning
  --no-persist    Do not persist sysctl settings
  --persist       Persist sysctl settings (default)
  --restore       Restore settings from backup
  --snapd-restore Re-enable snapd.service and snapd.socket
  --install-startup [mode] [opts...]  Install systemd startup service
  --remove-startup                  Remove systemd startup service
  
${BLUE}EXAMPLES:${NC}
  sudo $SCRIPT_NAME adaptive --io-throttle
  sudo $SCRIPT_NAME auto            # Auto mode with hardware-aware tuning
  sudo $SCRIPT_NAME performance      # Full performance mode
  sudo $SCRIPT_NAME dev --io-throttle # Dev mode with I/O throttling
  sudo $SCRIPT_NAME balanced --quick # Balanced mode without storage check
  sudo $SCRIPT_NAME performance --drop-caches
  sudo $SCRIPT_NAME performance --io-throttle
  sudo $SCRIPT_NAME status           # Show current status
  sudo $SCRIPT_NAME --restore        # Restore original settings

${BLUE}FEATURES:${NC}
  • CPU governor optimization
  • Memory and swap tuning
  • Adaptive mode (dev on AC, powersave on battery)
  • Auto mode (hardware-aware tuning)
  • Service management
  • GNOME desktop tweaks
  • Storage drive optimization
  • Per-device I/O scheduler tuning
  • Persistent I/O scheduler tuning via udev
  • Battery charge thresholds (60-80%)
  • Developer sysctl limits (inotify/file-max)
  • Persistent sysctl overrides (99-supershell.conf)
  • System cache cleaning
  • Backup and restore functionality
  • Optional startup service installer

EOF
}

# Main script logic
main() {
    init_context

    # Create log file
    mkdir -p "$(dirname "$LOG_FILE")" 2>/dev/null || true
    touch "$LOG_FILE" 2>/dev/null || true

    if [[ $# -eq 0 ]]; then
        set -- adaptive
    fi
    
    # Parse arguments
    case "${1:-}" in
        adaptive|auto|performance|balanced|powersave|dev)
            local mode="$1"
            shift || true
            REQUESTED_MODE="$mode"
            if [[ "$mode" == "adaptive" ]]; then
                mode="$(select_adaptive_mode)"
            elif [[ "$mode" == "auto" ]]; then
                mode="$(select_auto_mode)"
            fi
            while [[ $# -gt 0 ]]; do
                case "$1" in
                    --quick) OPT_QUICK=1 ;;
                    --drop-caches) DROP_CACHES=1 ;;
                    --drop-swap) DROP_SWAP=1 ;;
                    --io-throttle) IO_THROTTLE=1 ;;
                    --snapd-quiet) SNAPD_QUIET=1 ;;
                    --no-clean) NO_CLEAN=1; NO_CLEAN_SET=1 ;;
                    --clean) NO_CLEAN=0; NO_CLEAN_SET=1 ;;
                    --no-persist) PERSIST_SYSCTL=0 ;;
                    --persist) PERSIST_SYSCTL=1 ;;
                    *) warn "Unknown option: $1" ;;
                esac
                shift || true
            done
            apply_optimizations "$mode"
            ;;
        status)
            show_status
            ;;
        --restore)
            restore_settings
            ;;
        --snapd-restore)
            restore_snapd 1
            ;;
        --install-startup)
            shift || true
            install_startup "$@"
            ;;
        --remove-startup)
            remove_startup
            ;;
        --help|-h)
            show_help
            ;;
        *)
            show_help
            exit 1
            ;;
    esac
}

# Run main function with all arguments
main "$@"
