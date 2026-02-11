#!/usr/bin/env python3
"""
Workflow Dashboard - Terminal UI for monitoring multi-device workflow
Shows system status, active sessions, recent tasks
"""

import curses
import subprocess
import json
import time
from pathlib import Path
from datetime import datetime

WORKSPACE = Path.home() / "SharedWorkspace"


class WorkflowDashboard:
    """Terminal-based dashboard for multi-device workflows"""
    
    def __init__(self):
        self.workspace = WORKSPACE
        self.last_update = time.time()
    
    def get_system_status(self, host):
        """Get status from a laptop"""
        try:
            if host == 'localhost':
                with open('/proc/loadavg', 'r') as f:
                    load = f.read().split()[0]
                
                result = subprocess.run(
                    ['df', '-h', str(Path.home())],
                    capture_output=True,
                    text=True,
                    timeout=2
                )
                disk_line = result.stdout.split('\n')[1].split()
                disk_used = disk_line[4]
                disk_avail = disk_line[3]
            else:
                result = subprocess.run(
                    ['ssh', f'{host}.local', 'cat /proc/loadavg && df -h ~ | tail -1'],
                    capture_output=True,
                    text=True,
                    timeout=5
                )
                lines = result.stdout.strip().split('\n')
                load = lines[0].split()[0]
                disk_parts = lines[1].split()
                disk_used = disk_parts[4]
                disk_avail = disk_parts[3]
            
            return {
                'load': load,
                'disk_used': disk_used,
                'disk_avail': disk_avail,
                'status': 'online'
            }
        except:
            return {'status': 'offline'}
    
    def get_active_sessions(self):
        """Get active AI sessions"""
        sessions_dir = self.workspace / ".ai_sessions"
        if not sessions_dir.exists():
            return []
        
        sessions = []
        for session_file in sessions_dir.glob("*.json"):
            try:
                with open(session_file, 'r') as f:
                    data = json.load(f)
                    if data.get('status') == 'active':
                        sessions.append(data)
            except:
                pass
        
        return sorted(sessions, key=lambda x: x.get('last_updated', ''), reverse=True)
    
    def get_recent_tasks(self):
        """Get recent task distribution history"""
        log_file = self.workspace / ".logs" / "task_distribution.jsonl"
        if not log_file.exists():
            return []
        
        tasks = []
        try:
            with open(log_file, 'r') as f:
                for line in f:
                    try:
                        tasks.append(json.load(line.strip()))
                    except:
                        pass
        except:
            pass
        
        return tasks[-10:]  # Last 10 tasks
    
    def draw_dashboard(self, stdscr):
        """Draw the dashboard"""
        curses.curs_set(0)
        stdscr.nodelay(1)
        stdscr.timeout(1000)
        
        # Colors
        curses.init_pair(1, curses.COLOR_GREEN, curses.COLOR_BLACK)
        curses.init_pair(2, curses.COLOR_RED, curses.COLOR_BLACK)
        curses.init_pair(3, curses.COLOR_YELLOW, curses.COLOR_BLACK)
        curses.init_pair(4, curses.COLOR_CYAN, curses.COLOR_BLACK)
        
        while True:
            stdscr.clear()
            h, w = stdscr.getmaxyx()
            
            try:
                # Title
                title = "╔═══ Multi-Device Workflow Dashboard ═══╗"
                stdscr.addstr(0, max(0, (w - len(title)) // 2), title, curses.A_BOLD | curses.color_pair(4))
                
                # Current time
                current_time = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
                stdscr.addstr(1, w - len(current_time) - 2, current_time, curses.color_pair(3))
                
                # System Status
                stdscr.addstr(3, 2, "SYSTEM STATUS", curses.A_BOLD | curses.color_pair(4))
                stdscr.addstr(4, 2, "─" * 50)
                
                main_status = self.get_system_status('localhost')
                inspiron_status = self.get_system_status('inspiron')
                
                # Main laptop
                y = 5
                if main_status['status'] == 'online':
                    stdscr.addstr(y, 4, f"🖥️  Main Laptop:", curses.A_BOLD)
                    stdscr.addstr(y, 20, f"Load: {main_status.get('load', 'N/A')}", curses.color_pair(1))
                    stdscr.addstr(y, 35, f"Disk: {main_status.get('disk_used', 'N/A')}", curses.color_pair(1))
                else:
                    stdscr.addstr(y, 4, "🖥️  Main Laptop: OFFLINE", curses.color_pair(2))
                
                # Inspiron
                y += 1
                if inspiron_status['status'] == 'online':
                    stdscr.addstr(y, 4, f"💻 Inspiron:", curses.A_BOLD)
                    stdscr.addstr(y, 20, f"Load: {inspiron_status.get('load', 'N/A')}", curses.color_pair(1))
                    stdscr.addstr(y, 35, f"Disk: {inspiron_status.get('disk_used', 'N/A')}", curses.color_pair(1))
                else:
                    stdscr.addstr(y, 4, "💻 Inspiron: OFFLINE", curses.color_pair(2))
                
                # Active Sessions
                y += 3
                stdscr.addstr(y, 2, "ACTIVE AI SESSIONS", curses.A_BOLD | curses.color_pair(4))
                stdscr.addstr(y + 1, 2, "─" * 50)
                
                sessions = self.get_active_sessions()
                if sessions:
                    for i, session in enumerate(sessions[:5], start=y + 2):
                        if i >= h - 10:
                            break
                        project = session.get('project', 'Unknown')[:25]
                        ai_tool = session.get('ai_tool', '?')[:10]
                        laptop = session.get('laptop', '?')[:15]
                        stdscr.addstr(i, 4, f"• {project:<25} ({ai_tool}) on {laptop}")
                else:
                    stdscr.addstr(y + 2, 4, "No active sessions", curses.color_pair(3))
                
                # Recent Tasks
                y = max(y + 8, 16)
                if y < h - 8:
                    stdscr.addstr(y, 2, "RECENT TASKS", curses.A_BOLD | curses.color_pair(4))
                    stdscr.addstr(y + 1, 2, "─" * 50)
                    
                    tasks = self.get_recent_tasks()
                    if tasks:
                        for i, task in enumerate(tasks[-5:], start=y + 2):
                            if i >= h - 5:
                                break
                            task_type = task.get('task_type', '?')[:15]
                            host = task.get('host', '?')[:10]
                            success = task.get('success', False)
                            status_icon = "✓" if success else "✗"
                            status_color = curses.color_pair(1) if success else curses.color_pair(2)
                            
                            stdscr.addstr(i, 4, f"{status_icon} {task_type:<15} on {host}", status_color)
                    else:
                        stdscr.addstr(y + 2, 4, "No tasks yet", curses.color_pair(3))
                
                # Controls
                if h > 5:
                    stdscr.addstr(h - 3, 2, "─" * min(60, w - 4))
                    controls = "Press 'q' to quit  |  'r' to refresh  |  's' for sessions  |  't' for tasks"
                    stdscr.addstr(h - 2, 2, controls[:w-4], curses.color_pair(3))
                
                stdscr.refresh()
            
            except curses.error:
                pass  # Ignore drawing errors from small terminals
            
            # Handle input
            key = stdscr.getch()
            if key == ord('q'):
                break
            elif key == ord('r'):
                continue
            elif key == ord('s'):
                self.show_sessions_detail(stdscr)
            elif key == ord('t'):
                self.show_tasks_detail(stdscr)
    
    def show_sessions_detail(self, stdscr):
        """Show detailed session view"""
        sessions = self.get_active_sessions()
        
        stdscr.clear()
        h, w = stdscr.getmaxyx()
        
        stdscr.addstr(0, 2, "Active Sessions Detail", curses.A_BOLD)
        stdscr.addstr(1, 2, "=" * 60)
        
        if sessions:
            y = 3
            for session in sessions:
                if y >= h - 5:
                    break
                
                stdscr.addstr(y, 2, f"Project: {session.get('project', 'Unknown')}")
                stdscr.addstr(y + 1, 2, f"AI Tool: {session.get('ai_tool', '?')}")
                stdscr.addstr(y + 2, 2, f"Laptop: {session.get('laptop', '?')}")
                stdscr.addstr(y + 3, 2, f"Created: {session.get('created', '?')[:19]}")
                y += 5
        else:
            stdscr.addstr(3, 2, "No active sessions")
        
        stdscr.addstr(h - 2, 2, "Press any key to return...")
        stdscr.refresh()
        stdscr.getch()
    
    def show_tasks_detail(self, stdscr):
        """Show detailed task view"""
        tasks = self.get_recent_tasks()
        
        stdscr.clear()
        h, w = stdscr.getmaxyx()
        
        stdscr.addstr(0, 2, "Recent Tasks Detail", curses.A_BOLD)
        stdscr.addstr(1, 2, "=" * 60)
        
        if tasks:
            y = 3
            for task in tasks[-10:]:
                if y >= h - 5:
                    break
                
                status = "✓" if task.get('success') else "✗"
                stdscr.addstr(y, 2, f"{status} {task.get('task_type', '?')}")
                stdscr.addstr(y + 1, 4, f"Host: {task.get('host', '?')} | Time: {task.get('timestamp', '?')[:19]}")
                y += 3
        else:
            stdscr.addstr(3, 2, "No tasks yet")
        
        stdscr.addstr(h - 2, 2, "Press any key to return...")
        stdscr.refresh()
        stdscr.getch()
    
    def run(self):
        """Run the dashboard"""
        try:
            curses.wrapper(self.draw_dashboard)
        except KeyboardInterrupt:
            pass


if __name__ == '__main__':
    dashboard = WorkflowDashboard()
    dashboard.run()
