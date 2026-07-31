import plotly.graph_objects as go
import pandas as pd
import numpy as np
import sys

def parse_input_file(filename):
    events = []
    dependencies = []
    colors = {}

    section = -1  # 0 = events, 1 = dependencies, 2 = colors

    with open(filename, 'r', encoding='utf-8') as f:
        for raw_line in f:
            line = raw_line.strip()
            if not line:
                continue  # skip empty lines

            if line == "@":
                section += 1
                continue

            if section == 0:  # Events section
                parts = [p.strip() for p in line.split(",")]
                if len(parts) != 5:
                    raise ValueError(f"Invalid event line: {line}")
                task, type_, start, end, description = parts
                events.append({
                    'Task': task,
                    'Type': type_,
                    'Start': int(start),
                    'End': int(end),
                    'Description': description
                })

            elif section == 1:  # Dependencies section
                parts = [p.strip() for p in line.split(",")]
                if len(parts) != 2:
                    raise ValueError(f"Invalid dependency line: {line}")
                from_task, to_task = parts
                dependencies.append({
                    'from': from_task,
                    'to': to_task
                })

            elif section == 2:  # Colors section
                parts = [p.strip() for p in line.split(",")]
                if len(parts) != 2:
                    raise ValueError(f"Invalid color line: {line}")
                type_, color = parts
                colors[type_] = color

    return events, dependencies, colors

if __name__ == "__main__":
    # parse arguments
    if len(sys.argv) != 3:
        print("Usage: python plot.py <input_file> <output_html_file>")
        sys.exit(1)
    events, dependencies, colors = parse_input_file(sys.argv[1])

    # Create sample data with multiple event types
    # derive event types from the events data, keep the order and reverse it
    event_types = pd.Series([event['Type'] for event in events]).unique()[::-1]

    # Create events data with absolute numbers for the timeline


    # Convert to DataFrame
    df = pd.DataFrame(events)

    print("Events:")
    print(events)

    print("Event Types:")
    print(event_types)

    # Create the timeline figure
    fig = go.Figure()

    # Y-positions for each event type (to create parallel timelines)
    y_positions = {event_type: i * 0.3 for i, event_type in enumerate(event_types)}

    # Add events to the timeline
    for i, event in df.iterrows():
        y_pos = y_positions[event['Type']]

        # Add rectangle shape for each event
        fig.add_trace(go.Scatter(
            x=[event['Start'], event['End'], event['End'], event['Start'], event['Start']],
            y=[y_pos-0.1, y_pos-0.1, y_pos+0.1, y_pos+0.1, y_pos-0.1],
            fill="toself",
            mode='lines',
            line=dict(color=colors[event['Type']]),
            fillcolor=colors[event['Type']],
            opacity=0.6,
            name=' ', #event['Task'],
            text=event['Description'],
            hoverinfo='text+name',
            showlegend=False
        ))

        # Calculate midpoint correctly
        midpoint = event['Start'] + (event['End'] - event['Start']) / 2

        # Add text label
        fig.add_trace(go.Scatter(
            x=[midpoint],
            y=[y_pos],
            text=' ',#event['Task'],
            mode='text',
            textfont=dict(
                size=16
            ),
            textposition='middle center',
            name=event['Task'],
            hoverinfo='none',
            showlegend=False
        ))

    # Add dependency arrows
    arrow_shapes = []
    for dep in dependencies:
        from_event = df[df['Task'] == dep['from']].iloc[0]
        to_event = df[df['Task'] == dep['to']].iloc[0]

        from_y = y_positions[from_event['Type']]
        to_y = y_positions[to_event['Type']]

        # start end points
        x0 = from_event['End']
        y0 = from_y
        x1 = to_event['Start']
        y1 = to_y

        # connection
        arrow_shapes.append(
            dict(
                type='line',
                x0=x0,
                y0=y0,
                x1=x1,
                y1=y1,
                line=dict(
                    color='black',
                    width=1.5,
                    #dash='dot'
                ),
                xref='x',
                yref='y'
            )
        )

        # angle
        dx = x1 - x0
        dy = y1 - y0
        angle = np.arctan2(dy, dx)

        arrow_length = 0.25

        # angle from the line
        arrow_angle = np.pi/12  # 30度

        arrow_x1 = x1 - arrow_length * np.cos(angle - arrow_angle)
        arrow_y1 = y1 - arrow_length * np.sin(angle - arrow_angle)

        arrow_x2 = x1 - arrow_length * np.cos(angle + arrow_angle)
        arrow_y2 = y1 - arrow_length * np.sin(angle + arrow_angle)

        # Add arrowheads
        arrow_shapes.append(
            dict(
                type='line',
                x0=arrow_x1,
                y0=arrow_y1,
                x1=x1,
                y1=y1,
                line=dict(
                    color='black',
                    width=1.5,
                ),
                xref='x',
                yref='y'
            )
        )

        arrow_shapes.append(
            dict(
                type='line',
                x0=arrow_x2,
                y0=arrow_y2,
                x1=x1,
                y1=y1,
                line=dict(
                    color='black',
                    width=1.5,
                ),
                xref='x',
                yref='y'
            )
        )

        # Calculate one unit before start for arrowhead (changed from one day to one unit)
        one_unit_before = to_event['Start'] - 1



    # Add arrows to the figure
    fig.update_layout(shapes=arrow_shapes)

    # Set the y-axis ticks to show event types
    fig.update_layout(
        yaxis=dict(
            tickmode='array',
            tickvals=list(y_positions.values()),
            ticktext=list(y_positions.keys()),
            zeroline=False,
            tickfont=dict(size=20),
        ),
        xaxis=dict(
            title=dict(text='Cycles',
                       font=dict(size=24)
                       ),
            tickfont=dict(size=20),
            # No longer need to specify 'type':'date' since we're using numbers
        ),
        title='Timeline',
        hovermode='closest',
        showlegend=False,
        plot_bgcolor='rgb(255, 255, 255)', # Sets the plot area to light gray
        paper_bgcolor='white',             # Sets the outer figure area to white
        font=dict(
            family="Times New Roman",  # Or "Times New Roman", "Verdana", etc.
            size=14,         # A new base size
            color="black"    # A new base color
        ),
        height=350,
    )

    # Add legend for event types
    for event_type, color in colors.items():
        fig.add_trace(go.Scatter(
            x=[None],
            y=[None],
            mode='markers',
            marker=dict(size=40, color=color),
            name=event_type,
            showlegend=True
        ))

    # Show the figure
    fig.show()
    fig.write_html(sys.argv[2])