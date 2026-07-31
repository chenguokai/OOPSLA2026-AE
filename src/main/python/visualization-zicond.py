import matplotlib.pyplot as plt
from matplotlib.patches import FancyArrowPatch
from matplotlib.ticker import FuncFormatter
from dataclasses import dataclass
from typing import List
import os
import random

# --- Constants for Visualization Control ---
LINE_WIDTH = 1.5
MARKER_HEIGHT_FACTOR = 0.20
# Reduced Y_UNIT_SPACING for better packing of tracks
Y_UNIT_SPACING = 0.25 
RANDOM_Y_OFFSET_MAGNITUDE = 0 # 0.1

# --- FORCED TIME RANGE ---
FORCED_X_MIN = 76000
FORCED_X_MAX = 76500
# --- END FORCED TIME RANGE ---

# --- Helper function for formatting axis ticks ---
def format_ticks(x, pos):
    """Formats ticks to use 'K' for thousands."""
    if x >= 1000:
        return f'{(x / 1000):.1f}'.rstrip('0').rstrip('.') + 'K'
    return int(x)

# Use a dataclass for a clean way to store event information
@dataclass
class Event:
    """Represents a single 'block in exec' event."""
    start: float
    end: float
    desc: str
    source_label: str # Added to distinguish sources

    @property
    def duration(self) -> float:
        return self.end - self.start

def parse_events(file_path: str, source_label: str) -> List[Event]:
    """
    Parses the input file according to the specified format,
    assigning a source label to each event.
    """
    events: List[Event] = []
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
    except FileNotFoundError:
        print(f"Error: File not found at '{file_path}'")
        return []
    except Exception as e:
        print(f"Error reading file: {e}")
        return []

    try:
        data_block = content.split('@', 1)[1]
        data_block = data_block.split('@', 1)[0]
    except IndexError:
        print("Error: Could not find two '@' delimiters in the file.")
        return []

    # Process each line
    for line in data_block.strip().split('\n'):
        line = line.strip()
        if not line:
            continue
            
        parts = line.split(',', 4)
        if len(parts) < 5:
            continue
        
        event_type = parts[0].strip()
        
        if event_type == "block in exec":
            try:
                start_time = float(parts[2].strip())
                end_time = float(parts[3].strip())
                description = parts[4].strip()
                
                if end_time < start_time:
                    continue

                events.append(Event(start=start_time, end=end_time, desc=description, source_label=source_label))
            except ValueError:
                continue
                
    return events

def assign_tracks(events: List[Event]) -> List[List[Event]]:
    """
    Assigns events to non-overlapping tracks (swim lanes) for a specific source.
    """
    if not events:
        return []
        
    sorted_events = sorted(events, key=lambda e: e.start)
    
    tracks: List[List[Event]] = []
    
    for event in sorted_events:
        best_track_index = -1
        earliest_available_time = float('inf') 

        for i, track in enumerate(tracks):
            if event.start >= track[-1].end:
                if track[-1].end < earliest_available_time:
                    earliest_available_time = track[-1].end
                    best_track_index = i
        
        if best_track_index != -1:
            tracks[best_track_index].append(event)
        else:
            tracks.append([event])
            
    return tracks

def draw_combined_visualization_image(
    file1_events: List[Event], file1_name: str,
    file2_events: List[Event], file2_name: str,
    output_filename: str = "events_visualization.png"
):
    """
    Draws a combined event visualization with two distinct Y-axis labels
    and summary statistics, all in a single graph.
    """
    
    # Assign tracks for each file independently
    tracks1 = assign_tracks(file1_events)
    tracks2 = assign_tracks(file2_events)

    # Calculate height for each group
    height1 = len(tracks1) * Y_UNIT_SPACING
    height2 = len(tracks2) * Y_UNIT_SPACING

    # Add a small buffer between the two groups of tracks
    buffer_height = Y_UNIT_SPACING * 1.3 
    total_plot_height = height1 + height2 + buffer_height

    # Figure size - dynamically adjust height
    fig_height = max(3.5, total_plot_height * 1.6) # Factor of 2 for better visual spacing
    fig, ax = plt.subplots(figsize=(12, fig_height))

    x_min = FORCED_X_MIN
    x_max = FORCED_X_MAX

    normal_exits1_count = 0
    normal_exits2_count = 0

    # --- Draw tracks for file1 (Zicond) ---
    # The first group starts at y=0 or slightly above
    current_y_offset = 0.5 * Y_UNIT_SPACING # Small offset from bottom
    for i, track in enumerate(tracks1):
        base_y_pos = current_y_offset + i * Y_UNIT_SPACING + Y_UNIT_SPACING / 2

        for event in track:
            if event.end < x_min or event.start > x_max:
                continue

            is_clipped_start = event.start < x_min
            is_clipped_end = event.end > x_max

            draw_start = max(event.start, x_min)
            draw_end = min(event.end, x_max)

            random_y_offset = random.uniform(-RANDOM_Y_OFFSET_MAGNITUDE, RANDOM_Y_OFFSET_MAGNITUDE)
            event_y_center = base_y_pos + random_y_offset

            is_interrupted = "redirect before block" in event.desc
            event_color = 'red' if is_interrupted else 'dodgerblue'

            arrow_style = '->' if not is_interrupted else '-'

            arrow = FancyArrowPatch(
                (draw_start, event_y_center), (draw_end, event_y_center),
                arrowstyle=arrow_style,
                mutation_scale=15,
                color=event_color,
                linewidth=LINE_WIDTH,
                zorder=2
            )
            ax.add_patch(arrow)

            marker_delta = Y_UNIT_SPACING * MARKER_HEIGHT_FACTOR
            
            if not is_clipped_start:
                ax.plot([event.start, event.start],
                        [event_y_center - marker_delta, event_y_center + marker_delta],
                        color=event_color,
                        linewidth=LINE_WIDTH, zorder=3)

            if not is_clipped_end:
                if is_interrupted:
                    ax.text(event.end, event_y_center, '⚡',
                            fontsize=14, ha='center', va='center', color='red', zorder=4)
                else:
                    ax.plot([event.end, event.end],
                            [event_y_center - marker_delta, event_y_center + marker_delta],
                            color=event_color,
                            linewidth=LINE_WIDTH, zorder=3)
                    # Count normal exits *entirely* within the visible range
                    if not is_interrupted and event.start >= x_min and event.end <= x_max:
                        normal_exits1_count += 1
    
    # Store the y-position where the Zicond section ends for y-axis label
    zicond_section_mid_y = current_y_offset + height1 / 2

    # Update current_y_offset for the second group (Branch)
    current_y_offset += height1 + buffer_height

    # --- Draw tracks for file2 (Branch) ---
    for i, track in enumerate(tracks2):
        base_y_pos = current_y_offset + i * Y_UNIT_SPACING + Y_UNIT_SPACING / 2

        for event in track:
            if event.end < x_min or event.start > x_max:
                continue

            is_clipped_start = event.start < x_min
            is_clipped_end = event.end > x_max

            draw_start = max(event.start, x_min)
            draw_end = min(event.end, x_max)

            random_y_offset = random.uniform(-RANDOM_Y_OFFSET_MAGNITUDE, RANDOM_Y_OFFSET_MAGNITUDE)
            event_y_center = base_y_pos + random_y_offset

            is_interrupted = "redirect before block" in event.desc
            event_color = 'red' if is_interrupted else 'dodgerblue'

            arrow_style = '->' if not is_interrupted else '-'

            arrow = FancyArrowPatch(
                (draw_start, event_y_center), (draw_end, event_y_center),
                arrowstyle=arrow_style,
                mutation_scale=15,
                color=event_color,
                linewidth=LINE_WIDTH,
                zorder=2
            )
            ax.add_patch(arrow)

            marker_delta = Y_UNIT_SPACING * MARKER_HEIGHT_FACTOR
            
            if not is_clipped_start:
                ax.plot([event.start, event.start],
                        [event_y_center - marker_delta, event_y_center + marker_delta],
                        color=event_color,
                        linewidth=LINE_WIDTH, zorder=3)

            if not is_clipped_end:
                if is_interrupted:
                    ax.text(event.end, event_y_center, '⚡',
                            fontsize=14, ha='center', va='center', color='red', zorder=4)
                else:
                    ax.plot([event.end, event.end],
                            [event_y_center - marker_delta, event_y_center + marker_delta],
                            color=event_color,
                            linewidth=LINE_WIDTH, zorder=3)
                    # Count normal exits *entirely* within the visible range
                    if not is_interrupted and event.start >= x_min and event.end <= x_max:
                        normal_exits2_count += 1
    
    # Store the y-position for the Branch section label
    branch_section_mid_y = current_y_offset + height2 / 2


    # --- Plot appearance setup ---
    ax.set_xlabel('Cycle')
    ax.set_title('Block in Exec Events Timeline', fontsize=16)
    ax.grid(axis='x', linestyle='--', alpha=0.7)

    ax.set_xlim(x_min, x_max)
    ax.set_ylim(0 - RANDOM_Y_OFFSET_MAGNITUDE, current_y_offset + height2 + RANDOM_Y_OFFSET_MAGNITUDE)

    ax.xaxis.set_major_formatter(FuncFormatter(format_ticks))

    ax.invert_yaxis() # Invert Y-axis so first track is at the top

    for spine in ax.spines.values():
        spine.set_visible(False)


    # Set custom Y-axis labels for the two sections
    ax.set_yticks([]) # Remove default y-ticks

    # Add horizontal Y-axis label for Zicond (placed far left, outside the data region)
    # Use fig.transFigure for positioning relative to the entire figure
    # Adjusted x to be further left (e.g., 0.01)
    fig.text(0.01, (zicond_section_mid_y / ax.get_ylim()[0] + 0.07) * (ax.get_ylim()[0] / (current_y_offset + height2 + RANDOM_Y_OFFSET_MAGNITUDE)), 
            "Branch code block",
            transform=fig.transFigure, rotation=0, va='center', ha='left', fontsize=11, weight='bold')

    # Add horizontal Y-axis label for Branch (placed far left, outside the data region)
    # Adjusted x to be further left (e.g., 0.01)
    fig.text(0.01, (branch_section_mid_y / ax.get_ylim()[0] - 0.15) * (ax.get_ylim()[0] / (current_y_offset + height2 + RANDOM_Y_OFFSET_MAGNITUDE)), 
            "Zicond code block",
            transform=fig.transFigure, rotation=0, va='center', ha='left', fontsize=11, weight='bold')


    # Add summary statistics (without "total events in range" and "Summary" title)
    summary_text = (
        f"Zicond: {normal_exits1_count} block commits\n"
        f"Branch: {normal_exits2_count} block commits"
    )

    # Place the summary in the middle-right part of the figure, within the data region
    # Using ax.transAxes for coordinates relative to the subplot's data area
    ax.text(0.95, 0.45, summary_text, # Adjust x from 0.95 to 0.7 to move it left
             fontsize=10, verticalalignment='center', horizontalalignment='right',
             bbox=dict(boxstyle='round,pad=0.5', fc='white', ec='white', lw=0.5, alpha=0.9),
             transform=ax.transAxes)


    # Adjust figure margins to make space for the labels
    plt.subplots_adjust(left=0.15, right=0.95, top=0.8, bottom=0.2) # Increased left margin

    plt.savefig(output_filename, dpi=300)
    print(f"Visualization saved to '{output_filename}'")
    plt.close(fig)

def main():
    """
    Main function to create dummy files and run the image visualization.
    """
    file_name_zicond = "/root/timelineA.txt"
    file_name_branch = "/root/timelineB.txt"
    output_image_name = "events_visualization.png"

    # Create dummy data for timeline-zicond.txt if it doesn't exist
    if not os.path.exists(file_name_zicond):
        dummy_data_zicond = """
This is some header text we don't care about.
Line 2 of header.
@
block in exec, block in exec, 10000, 30000, A long blocking call
block in exec, block in exec, 20000, 40000, An overlapping call
block in exec, block in exec, 35000, 50100, redirect before block
block in exec, block in exec, 55000, 60000, A short event
block in exec, block in exec, 56000, 58000, A nested event
block in exec, block in exec, 70000, 72000, A tiny event
block in exec, block in exec, 75000, 75200, First event in range
block in exec, block in exec, 75300, 75500, Second event in range
block in exec, block in exec, 75600, 75800, Third event in range (clipped end)
block in exec, block in exec, 76000, 76100, redirect before block (interrupted in range)
block in exec, block in exec, 76500, 77500, Ends outside range but starts in
@
This is the footer text.
"""
        try:
            with open(file_name_zicond, "w", encoding="utf-8") as f:
                f.write(dummy_data_zicond)
            print(f"Created dummy data file: '{file_name_zicond}'")
        except Exception as e:
            print(f"Failed to create dummy file: {e}")
            return

    # Create dummy data for timeline-branch.txt if it doesn't exist
    if not os.path.exists(file_name_branch):
        dummy_data_branch = """
Header for branch file.
@
block in exec, block in exec, 74000, 75100, Starts before range
block in exec, block in exec, 75000, 75300, Branch event 1
block in exec, block in exec, 75200, 75400, Branch event 2
block in exec, block in exec, 75500, 75700, Branch event 3 (redirect before block)
block in exec, block in exec, 75800, 76000, Branch event 4
block in exec, block in exec, 76050, 76200, Branch event 5 (redirect before block)
block in exec, block in exec, 76300, 76500, Branch event 6
block in exec, block in exec, 76600, 76800, Branch event 7 (redirect before block)
block in exec, block in exec, 76900, 77100, Branch event 8 (ends outside)
@
Footer for branch file.
"""
        try:
            with open(file_name_branch, "w", encoding="utf-8") as f:
                f.write(dummy_data_branch)
            print(f"Created dummy data file: '{file_name_branch}'")
        except Exception as e:
            print(f"Failed to create dummy file: {e}")
            return

    events_zicond = parse_events(file_name_zicond, "timeline-zicond.txt")
    events_branch = parse_events(file_name_branch, "timeline-branch.txt")
    
    if not events_zicond and not events_branch:
        print("No events were parsed from either file.")
        return
        
    draw_combined_visualization_image(
        events_zicond, file_name_zicond,
        events_branch, file_name_branch,
        output_image_name
    )

if __name__ == "__main__":
    main()
