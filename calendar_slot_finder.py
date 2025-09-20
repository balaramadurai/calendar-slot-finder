#!/usr/bin/env python3
"""
Calendar Slot Finder - Find empty time slots in your schedule
Integrates with Org-mode agenda files and org-gcal entries
"""

import datetime
import re
import json
import sys
import argparse
from typing import List, Tuple, Dict, Optional
from dataclasses import dataclass
from pathlib import Path

@dataclass
class TimeSlot:
    start: datetime.datetime
    end: datetime.datetime
    title: str = ""
    
    def overlaps_with(self, other: 'TimeSlot') -> bool:
        return self.start < other.end and self.end > other.start
    
    def duration_minutes(self) -> int:
        return int((self.end - self.start).total_seconds() / 60)

@dataclass
class WeeklyRestriction:
    days: List[int]  # 0=Monday, 6=Sunday
    start_time: datetime.time
    end_time: datetime.time
    description: str

class CalendarSlotFinder:
    def __init__(self):
        self.busy_slots: List[TimeSlot] = []
        self.restrictions: List[WeeklyRestriction] = []
        self.working_hours = (datetime.time(9, 0), datetime.time(17, 0))
        self.min_slot_duration = 60  # minutes
        self.meeting_cushion = 15  # minutes buffer before/after meetings
        
    def add_restriction(self, restriction: WeeklyRestriction):
        """Add a weekly recurring restriction (e.g., meeting hours)"""
        self.restrictions.append(restriction)
    
    def parse_org_timestamp(self, timestamp_str: str) -> Optional[Tuple[datetime.datetime, datetime.datetime]]:
        """Parse org-mode timestamp format"""
        # Match patterns like: <2024-01-15 Mon 14:30-15:30>
        patterns = [
            r'<(\d{4}-\d{2}-\d{2})\s+\w+\s+(\d{1,2}:\d{2})-(\d{1,2}:\d{2})>',
            r'<(\d{4}-\d{2}-\d{2})\s+\w+\s+(\d{1,2}:\d{2})>',
            r'\[(\d{4}-\d{2}-\d{2})\s+\w+\s+(\d{1,2}:\d{2})-(\d{1,2}:\d{2})\]',
            r'\[(\d{4}-\d{2}-\d{2})\s+\w+\s+(\d{1,2}:\d{2})\]',
        ]
        
        for pattern in patterns:
            match = re.search(pattern, timestamp_str)
            if match:
                date_str = match.group(1)
                start_time_str = match.group(2)
                end_time_str = match.group(3) if len(match.groups()) > 2 else None
                
                try:
                    date = datetime.datetime.strptime(date_str, "%Y-%m-%d").date()
                    start_time = datetime.datetime.strptime(start_time_str, "%H:%M").time()
                    start_dt = datetime.datetime.combine(date, start_time)
                    
                    if end_time_str:
                        end_time = datetime.datetime.strptime(end_time_str, "%H:%M").time()
                        end_dt = datetime.datetime.combine(date, end_time)
                    else:
                        # Default 1 hour duration if no end time
                        end_dt = start_dt + datetime.timedelta(hours=1)
                    
                    return start_dt, end_dt
                except ValueError:
                    continue
        return None
    
    def parse_org_file(self, filepath: str) -> List[TimeSlot]:
        """Parse org file for scheduled items and deadlines"""
        slots = []
        try:
            with open(filepath, 'r', encoding='utf-8') as f:
                content = f.read()
            
            # Find entries with timestamps
            lines = content.split('\n')
            current_heading = ""
            
            for line in lines:
                # Track current heading
                if line.strip().startswith('*'):
                    current_heading = re.sub(r'^\*+\s*', '', line.strip())
                
                # Look for scheduled items and timestamps
                if any(keyword in line for keyword in ['SCHEDULED:', 'DEADLINE:', '<', '[']):
                    timestamp_result = self.parse_org_timestamp(line)
                    if timestamp_result:
                        start_dt, end_dt = timestamp_result
                        title = current_heading if current_heading else "Org entry"
                        slots.append(TimeSlot(start_dt, end_dt, title))
        
        except FileNotFoundError:
            pass  # File doesn't exist, skip silently
        except Exception as e:
            print(f"⚠️  Error parsing org file {filepath}: {e}")
            
        return slots
    
    def parse_gcal_entry(self, entry: Dict) -> Optional[TimeSlot]:
        """Parse a Google Calendar entry (from org-gcal format)"""
        try:
            # org-gcal stores entries in org format, look for timestamp patterns
            if 'timestamp' in entry:
                result = self.parse_org_timestamp(entry['timestamp'])
                if result:
                    start_dt, end_dt = result
                    title = entry.get('summary', 'GCal event')
                    return TimeSlot(start_dt, end_dt, title)
        except Exception as e:
            pass  # Skip silently for cleaner output
        return None
    
    def load_calendar_data(self, org_files: List[str], gcal_file: Optional[str] = None):
        """Load calendar data from org files and gcal"""
        # Load org agenda files
        for org_file in org_files:
            slots = self.parse_org_file(org_file)
            self.busy_slots.extend(slots)
            # print(f"Loaded {len(slots)} entries from {org_file}")
        
        # Load gcal data if available
        if gcal_file and Path(gcal_file).exists():
            try:
                with open(gcal_file, 'r') as f:
                    gcal_data = json.load(f)
                
                if isinstance(gcal_data, list):
                    for entry in gcal_data:
                        slot = self.parse_gcal_entry(entry)
                        if slot:
                            self.busy_slots.append(slot)
                print(f"Loaded Google Calendar entries from {gcal_file}")
            except Exception as e:
                print(f"Warning: Could not load gcal file: {e}")
    
    def apply_meeting_cushions(self, slots: List[TimeSlot]) -> List[TimeSlot]:
        """Apply meeting cushions to busy slots to create buffer time"""
        if self.meeting_cushion <= 0:
            return slots
            
        cushioned_slots = []
        cushion_delta = datetime.timedelta(minutes=self.meeting_cushion)
        
        for slot in slots:
            # Extend the slot by cushion time before and after
            new_start = slot.start - cushion_delta
            new_end = slot.end + cushion_delta
            
            cushioned_slot = TimeSlot(
                new_start, 
                new_end, 
                f"{slot.title} (+ {self.meeting_cushion}min buffer)"
            )
            cushioned_slots.append(cushioned_slot)
        
        return cushioned_slots
    
    def apply_restrictions_to_week(self, week_start: datetime.date) -> List[TimeSlot]:
        """Apply weekly restrictions to generate blocked slots for the week"""
        blocked_slots = []
        
        for restriction in self.restrictions:
            for day_offset in range(7):
                current_date = week_start + datetime.timedelta(days=day_offset)
                weekday = current_date.weekday()  # 0=Monday
                
                if weekday in restriction.days:
                    start_dt = datetime.datetime.combine(current_date, restriction.start_time)
                    end_dt = datetime.datetime.combine(current_date, restriction.end_time)
                    
                    blocked_slots.append(TimeSlot(
                        start_dt, end_dt, f"Restricted: {restriction.description}"
                    ))
        
        return blocked_slots
    
    def find_free_slots(self, start_date: datetime.date, end_date: datetime.date) -> List[TimeSlot]:
        """Find free time slots in the given date range"""
        # Get all busy slots in the date range
        relevant_slots = []
        
        # Add existing busy slots (meetings, appointments, etc.)
        busy_meetings = []
        for slot in self.busy_slots:
            if start_date <= slot.start.date() <= end_date:
                busy_meetings.append(slot)
        
        # Apply cushions to meeting slots (not to restrictions)
        cushioned_meetings = self.apply_meeting_cushions(busy_meetings)
        relevant_slots.extend(cushioned_meetings)
        
        # Add restriction-based blocked slots (no cushion needed for these)
        current_date = start_date
        while current_date <= end_date:
            week_start = current_date - datetime.timedelta(days=current_date.weekday())
            restriction_slots = self.apply_restrictions_to_week(week_start)
            for slot in restriction_slots:
                if start_date <= slot.start.date() <= end_date:
                    relevant_slots.append(slot)
            current_date += datetime.timedelta(days=7)
        
        # Sort busy slots by start time
        relevant_slots.sort(key=lambda x: x.start)
        
        # Find free slots
        free_slots = []
        current_date = start_date
        
        while current_date <= end_date:
            # Skip weekends if specified in working hours
            if current_date.weekday() >= 5:  # Saturday, Sunday
                current_date += datetime.timedelta(days=1)
                continue
            
            day_start = datetime.datetime.combine(current_date, self.working_hours[0])
            day_end = datetime.datetime.combine(current_date, self.working_hours[1])
            
            # Get busy slots for this day
            day_busy_slots = [s for s in relevant_slots 
                            if s.start.date() == current_date and s.overlaps_with(TimeSlot(day_start, day_end))]
            day_busy_slots.sort(key=lambda x: x.start)
            
            # Find gaps between busy slots
            current_time = day_start
            
            for busy_slot in day_busy_slots:
                # Check if there's a gap before this busy slot
                gap_start = max(current_time, day_start)
                gap_end = min(busy_slot.start, day_end)
                
                if gap_end > gap_start:
                    gap_duration = int((gap_end - gap_start).total_seconds() / 60)
                    if gap_duration >= self.min_slot_duration:
                        free_slots.append(TimeSlot(gap_start, gap_end, f"Free ({gap_duration} min)"))
                
                current_time = max(current_time, busy_slot.end)
            
            # Check for gap after last busy slot
            if current_time < day_end:
                gap_duration = int((day_end - current_time).total_seconds() / 60)
                if gap_duration >= self.min_slot_duration:
                    free_slots.append(TimeSlot(current_time, day_end, f"Free ({gap_duration} min)"))
            
            current_date += datetime.timedelta(days=1)
        
        return free_slots

def load_config_from_file(config_file: str) -> CalendarSlotFinder:
    """Load configuration from JSON file created by Elisp wrapper"""
    finder = CalendarSlotFinder()
    
    try:
        with open(config_file, 'r') as f:
            config = json.load(f)
        
        # Set working hours
        if 'working_hours' in config and len(config['working_hours']) == 2:
            start_time_str, end_time_str = config['working_hours']
            try:
                finder.working_hours = (
                    datetime.datetime.strptime(start_time_str, "%H:%M").time(),
                    datetime.datetime.strptime(end_time_str, "%H:%M").time()
                )
            except ValueError as e:
                print(f"⚠️  Invalid working hours format: {e}")
        
        # Set minimum slot duration
        if 'min_slot_duration' in config:
            finder.min_slot_duration = int(config['min_slot_duration'])
        
        # Set meeting cushion
        if 'meeting_cushion' in config:
            finder.meeting_cushion = int(config['meeting_cushion'])
        
        # Add restrictions
        if 'restrictions' in config and isinstance(config['restrictions'], list):
            for restriction_data in config['restrictions']:
                try:
                    days = restriction_data.get('days', [])
                    start_time_str = restriction_data.get('start-time') or restriction_data.get('start_time')
                    end_time_str = restriction_data.get('end-time') or restriction_data.get('end_time')
                    description = restriction_data.get('description', 'Restriction')
                    
                    if days and start_time_str and end_time_str:
                        start_time = datetime.datetime.strptime(start_time_str, "%H:%M").time()
                        end_time = datetime.datetime.strptime(end_time_str, "%H:%M").time()
                        
                        restriction = WeeklyRestriction(days, start_time, end_time, description)
                        finder.add_restriction(restriction)
                    else:
                        print(f"⚠️  Incomplete restriction data: {restriction_data}")
                        
                except (ValueError, KeyError) as e:
                    print(f"⚠️  Error parsing restriction: {e}")
        
    except (FileNotFoundError, json.JSONDecodeError) as e:
        print(f"⚠️  Could not load config file {config_file}: {e}")
        print("   Using default configuration")
    
    return finder

def interactive_setup() -> CalendarSlotFinder:
    """Interactive setup to get user preferences"""
    finder = CalendarSlotFinder()
    
    print("=== Calendar Slot Finder Setup ===\n")
    
    # Working hours
    print("Set your working hours (default: 9:00-17:00)")
    start_hour = input("Start hour (HH:MM, default 09:00): ").strip() or "09:00"
    end_hour = input("End hour (HH:MM, default 17:00): ").strip() or "17:00"
    
    try:
        finder.working_hours = (
            datetime.datetime.strptime(start_hour, "%H:%M").time(),
            datetime.datetime.strptime(end_hour, "%H:%M").time()
        )
    except ValueError:
        print("Invalid time format, using defaults")
    
    # Minimum slot duration
    min_duration = input("Minimum slot duration in minutes (default 60): ").strip()
    if min_duration.isdigit():
        finder.min_slot_duration = int(min_duration)
    
    # Meeting cushion
    cushion = input("Meeting cushion in minutes (buffer before/after meetings, default 15): ").strip()
    if cushion.isdigit():
        finder.meeting_cushion = int(cushion)
    
    # Weekly restrictions
    print("\n=== Weekly Restrictions ===")
    print("Add recurring weekly restrictions (e.g., meeting hours)")
    
    while True:
        add_restriction = input("Add a weekly restriction? (y/n): ").lower()
        if add_restriction != 'y':
            break
            
        description = input("Description (e.g., 'Team meeting hours'): ")
        
        print("Days (0=Monday, 1=Tuesday, ..., 6=Sunday)")
        days_input = input("Enter day numbers separated by commas (e.g., 0,1,2,3,4 for weekdays): ")
        try:
            days = [int(d.strip()) for d in days_input.split(',') if d.strip().isdigit()]
        except:
            print("Invalid day format, skipping restriction")
            continue
        
        start_time_str = input("Start time (HH:MM): ")
        end_time_str = input("End time (HH:MM): ")
        
        try:
            start_time = datetime.datetime.strptime(start_time_str, "%H:%M").time()
            end_time = datetime.datetime.strptime(end_time_str, "%H:%M").time()
            
            restriction = WeeklyRestriction(days, start_time, end_time, description)
            finder.add_restriction(restriction)
            print(f"Added restriction: {description}")
        except ValueError:
            print("Invalid time format, skipping restriction")
    
    return finder

def print_boxed_header(text, padding=2):
    total_width = len(text) + (padding * 2)
    print(f"{' '}{'━' * total_width}")
    print(f"┃{' ' * padding}{text}{' ' * padding}┃")
    print(f"{' '}{'━' * total_width}")

def main():
    parser = argparse.ArgumentParser(description='Find empty calendar slots')
    parser.add_argument('--week-offset', type=int, default=1, 
                       help='Week offset from current week (default: 1 for next week)')
    parser.add_argument('--org-files', nargs='*', 
                       help='Org agenda files to check')
    parser.add_argument('--gcal-file', 
                       help='JSON file with Google Calendar data')
    parser.add_argument('--non-interactive', action='store_true',
                       help='Skip interactive setup')
    parser.add_argument('--config-file',
                       help='JSON file with configuration (working hours, restrictions, etc.)')
    
    args = parser.parse_args()
    
    if args.config_file and Path(args.config_file).exists():
        finder = load_config_from_file(args.config_file)
    elif args.non_interactive:
        finder = CalendarSlotFinder()
        # Add example restriction for demo
        finder.add_restriction(WeeklyRestriction(
            days=[0, 1, 2, 3, 4],  # Weekdays
            start_time=datetime.time(14, 30),
            end_time=datetime.time(17, 30),
            description="Meeting hours"
        ))
    else:
        finder = interactive_setup()
    
    # Determine org files to check
    org_files = args.org_files or []
    if not org_files:
        # Try to find common org files
        common_paths = ['~/org/agenda.org', '~/org/schedule.org', '~/org/tasks.org']
        for path in common_paths:
            expanded_path = Path(path).expanduser()
            if expanded_path.exists():
                org_files.append(str(expanded_path))
    
    # Load calendar data
    # print(f"\n=== Loading Calendar Data ===")
    finder.load_calendar_data(org_files, args.gcal_file)
    
    # Calculate date range
    today = datetime.date.today()
    week_start = today + datetime.timedelta(days=(7 * args.week_offset) - today.weekday())
    week_end = week_start + datetime.timedelta(days=6)

    free_slots = finder.find_free_slots(week_start, week_end)

    # Display results FIRST

    print_boxed_header(f"FREE TIME SLOTS - {week_start.strftime('%A, %B %d')} to {week_end.strftime('%A, %B %d, %Y')}")

    if not free_slots:
        print("\n🚫 No free slots found in the specified period.")
        print("\nTips:")
        print("   • Try adjusting your working hours")
        print("   • Reduce minimum slot duration")
        print("   • Check for conflicting restrictions")
    else:
        print(f"\n✅ Found {len(free_slots)} available time slots:\n")
        
        current_date = None
        for slot in free_slots:
            if slot.start.date() != current_date:
                current_date = slot.start.date()
                print(f"📅 {current_date.strftime('%A, %B %d, %Y')}:")
            
            print(f"   🕐 {slot.start.strftime('%H:%M')} - {slot.end.strftime('%H:%M')} "
                  f"({slot.duration_minutes()} min)")
    
    # Then show configuration details
    print_boxed_header(f"📊 SEARCH CONFIGURATION")
    print(f"🗓️  Search period: {week_start.strftime('%a %m/%d')} - {week_end.strftime('%a %m/%d/%Y')}")
    print(f"⏰ Working hours: {finder.working_hours[0]} - {finder.working_hours[1]}")
    print(f"⌛ Minimum slot: {finder.min_slot_duration} minutes")
    print(f"🛡️  Meeting buffer: {finder.meeting_cushion} minutes")
    
    if finder.restrictions:
        print(f"🚫 Active restrictions ({len(finder.restrictions)}):")
        for restriction in finder.restrictions:
            day_names = ['Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun']
            days_str = ', '.join([day_names[d] for d in restriction.days])
            print(f"   • {restriction.description}: {days_str} {restriction.start_time}-{restriction.end_time}")
    else:
        print("🚫 No restrictions active")
    
    # Show data sources
    print_boxed_header(f"📁 DATA SOURCES")
    total_entries = len(finder.busy_slots)
    if total_entries > 0:
        print(f"📋 Processed {total_entries} calendar entries")
    else:
        print("📋 No calendar entries found")

if __name__ == "__main__":
    main()
