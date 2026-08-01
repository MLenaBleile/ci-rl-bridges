import networkx as nx
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch
import numpy as np


class IntellectualGenealogy:
    def __init__(self):
        self.G = nx.DiGraph()
        self.fields = {
            'control': {'name': 'Control Theory', 'shape': 'hexagon', 'color': '#FFE6E6'},
            'neural': {'name': 'Neural Networks', 'shape': 'ellipse', 'color': '#E6F3FF'},
            'bayesian': {'name': 'Bayesian/Statistical', 'shape': 'diamond', 'color': '#F0E6FF'},
            'causal': {'name': 'Causal Inference', 'shape': 'box', 'color': '#E6FFE6'}
        }

    def add_person(self, name, dates, accomplishments, period=None, field=None):
        """
        Add a person node to the graph.

        Parameters:
        -----------
        name : str
            Person's name
        dates : str
            Years active or birth-death
        accomplishments : list of dict
            Each dict has 'desc': str and 'citation': str
        period : int or None
            Chronological period for ordering (lower = earlier)
        field : str or None
            Which field: 'control', 'neural', 'bayesian', 'causal'
        """
        self.G.add_node(name,
                        dates=dates,
                        accomplishments=accomplishments,
                        period=period,
                        field=field)

    def add_influence(self, influencer, influenced, reference, style='solid'):
        """
        Add a directed edge showing influence.

        Parameters:
        -----------
        influencer : str
            Name of the influencing person
        influenced : str
            Name of the influenced person
        reference : str
            Citation or description of the influence
        style : str
            'solid' for direct influence, 'dashed' for social/potential connections
        """
        self.G.add_edge(influencer, influenced, reference=reference, style=style)

    def get_person_info(self, name):
        """Return formatted information about a person."""
        if name not in self.G.nodes:
            return f"{name} not in graph"

        node_data = self.G.nodes[name]
        info = f"\n{name} ({node_data['dates']})\n"
        info += f"Field: {node_data.get('field', 'N/A')}\n"
        info += "Accomplishments:\n"
        for acc in node_data['accomplishments']:
            info += f"  - {acc['desc']} [{acc['citation']}]\n"

        # Get influences
        influences = list(self.G.predecessors(name))
        if influences:
            info += "Influenced by:\n"
            for inf in influences:
                ref = self.G[inf][name]['reference']
                info += f"  - {inf}: {ref}\n"

        return info

    def export_to_graphviz(self, filename='genealogy.dot',
                           layout='LR',
                           include_edge_labels=True,
                           include_accomplishments=False,
                           chronological=True):
        """
        Export to DOT format with rich formatting for Graphviz.

        Parameters:
        -----------
        filename : str
            Output filename
        layout : str
            'TB' (top-bottom), 'LR' (left-right), 'BT', 'RL'
        include_edge_labels : bool
            Whether to show influence references on edges
        include_accomplishments : bool
            Whether to include accomplishments in node labels (can be cluttered)
        chronological : bool
            If True, arrange nodes left-to-right by time period
        """

        dot_lines = []
        dot_lines.append('digraph IntellectualGenealogy {')
        dot_lines.append('    // Graph attributes')
        dot_lines.append(f'    rankdir={layout};')
        # Increased node margin and padding
        dot_lines.append('    node [style="filled", fontname="Helvetica", margin=0.3, pad=0.1];')
        # More prominent arrows with better visibility
        dot_lines.append('    edge [fontname="Helvetica-Oblique", fontsize=10, ')
        dot_lines.append('          arrowhead=vee, arrowsize=1.5, penwidth=2.5, ')
        dot_lines.append('          headclip=true, tailclip=true];')
        # Increased spacing to prevent arrow occlusion
        dot_lines.append('    graph [splines=ortho, nodesep=1.5, ranksep=2.5, overlap=false];')
        dot_lines.append('')

        # Add legend
        dot_lines.append('    // Legend')
        dot_lines.append('    subgraph cluster_legend {')
        dot_lines.append('        label="Field Legend";')
        dot_lines.append('        fontsize=12;')
        dot_lines.append('        style=dashed;')
        for field_key, field_info in self.fields.items():
            legend_node = f"legend_{field_key}"
            dot_lines.append(f'        "{legend_node}" [label="{field_info["name"]}", ')
            dot_lines.append(f'                           shape={field_info["shape"]}, ')
            dot_lines.append(f'                           fillcolor="{field_info["color"]}"];')
        dot_lines.append('    }')
        dot_lines.append('')

        # Add visible nodes with field-specific shapes and colors
        dot_lines.append('    // Nodes')
        for node in self.G.nodes():
            data = self.G.nodes[node]
            field = data.get('field', 'neural')
            field_info = self.fields.get(field, self.fields['neural'])
            shape = field_info['shape']
            color = field_info['color']

            # Create label
            if include_accomplishments and data['accomplishments']:
                label_parts = [f"<B>{node}</B>", f"<I>{data['dates']}</I>", ""]
                for acc in data['accomplishments']:
                    desc = acc['desc'].replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')
                    label_parts.append(f"• {desc}")
                label = '<<TABLE BORDER="0" CELLBORDER="0" CELLSPACING="0" CELLPADDING="4">'
                for part in label_parts:
                    label += f'<TR><TD ALIGN="LEFT">{part}</TD></TR>'
                label += '</TABLE>>'
            else:
                label = f'<<B>{node}</B><BR/><I>{data["dates"]}</I>>'

            dot_lines.append(f'    "{node}" [label={label}, shape={shape}, fillcolor="{color}"];')

        dot_lines.append('')

        # Add chronological constraints if requested
        if chronological:
            dot_lines.append('    // Chronological constraints (same rank = same time period)')

            # Group nodes by period
            periods = {}
            for node in self.G.nodes():
                period = self.G.nodes[node].get('period')
                if period is not None:
                    if period not in periods:
                        periods[period] = []
                    periods[period].append(node)

            # Add rank constraints for each period
            for period in sorted(periods.keys()):
                nodes_in_period = periods[period]
                if len(nodes_in_period) > 1:
                    node_list = '; '.join([f'"{n}"' for n in nodes_in_period])
                    dot_lines.append(f'    {{ rank=same; {node_list}; }}')

            dot_lines.append('')

        # Add edges
        dot_lines.append('    // Edges (influences)')
        for u, v in self.G.edges():
            edge_data = self.G[u][v]
            edge_style = edge_data.get('style', 'solid')

            # Check if edge is between nodes in the same period
            u_period = self.G.nodes[u].get('period')
            v_period = self.G.nodes[v].get('period')
            same_period = (u_period is not None and u_period == v_period)

            # Dashed edges for social connections that didn't lead to intellectual influence
            style_attr = 'style=dashed, color="#999999"' if edge_style == 'dashed' else 'color="#333333"'

            # Add constraint=false for edges within the same period to allow better routing
            if same_period:
                style_attr += ', constraint=false'

            if include_edge_labels:
                ref = edge_data['reference']
                if len(ref) > 60:
                    ref = ref[:57] + '...'
                ref_escaped = ref.replace('"', '\\"')
                dot_lines.append(f'    "{u}" -> "{v}" [label="{ref_escaped}", {style_attr}];')
            else:
                dot_lines.append(f'    "{u}" -> "{v}" [{style_attr}];')

        dot_lines.append('}')

        # Write to file with UTF-8 encoding
        with open(filename, 'w', encoding='utf-8') as f:
            f.write('\n'.join(dot_lines))

        print(f"Exported to {filename}")
        print("\nTo render with Graphviz:")
        print(f"  dot -Tpng {filename} -o genealogy.png -Gdpi=300")
        print(f"  dot -Tpdf {filename} -o genealogy.pdf")
        print(f"  dot -Tsvg {filename} -o genealogy.svg")

    def visualize_matplotlib(self, figsize=(16, 22), save_path=None):
        """
        Create a matplotlib visualization with shapes (not just colors)
        denoting each field, for legibility when printed in grayscale or
        at small size. Uses a portrait layout: time runs top-to-bottom,
        nodes within a period spread horizontally.
        """
        fig, ax = plt.subplots(figsize=figsize)

        # Map fields to matplotlib node shapes (in addition to fill colors)
        mpl_shapes = {
            'control': 'h',     # hexagon
            'neural': 'o',      # circle
            'bayesian': 'D',    # diamond
            'causal': 's',      # square
        }

        # Group nodes by period (chronological group)
        periods = {}
        for node in self.G.nodes():
            period = self.G.nodes[node].get('period', 0)
            if period not in periods:
                periods[period] = []
            periods[period].append(node)

        # Portrait layout: period -> vertical position (top = earliest),
        # within-period spread along horizontal axis.
        pos = {}
        vertical_period_spacing = 4.5  # units between successive periods
        horizontal_slot_width = 3.8    # units between adjacent nodes in same period
        for period, nodes in periods.items():
            y = -period * vertical_period_spacing  # negative so period 0 sits at top
            n = len(nodes)
            for i, node in enumerate(nodes):
                # Center the row of nodes around x=0
                x = (i - (n - 1) / 2) * horizontal_slot_width
                pos[node] = (x, y)

        # Separate solid and dashed edges
        solid_edges = [(u, v) for u, v in self.G.edges()
                       if self.G[u][v].get('style', 'solid') == 'solid']
        dashed_edges = [(u, v) for u, v in self.G.edges()
                        if self.G[u][v].get('style', 'solid') == 'dashed']

        # Draw solid edges. Margins must be large enough that arrowheads
        # emerge OUTSIDE the (much larger) nodes; with node_size=16000 the
        # node radius is roughly 65pt so we use ~70pt margins.
        nx.draw_networkx_edges(self.G, pos,
                               edgelist=solid_edges,
                               edge_color='#222222',
                               arrows=True,
                               arrowsize=45,
                               arrowstyle='-|>',
                               width=3.5,
                               alpha=0.9,
                               connectionstyle='arc3,rad=0.15',
                               min_source_margin=70,
                               min_target_margin=70)

        # Draw dashed edges (social connections)
        if dashed_edges:
            nx.draw_networkx_edges(self.G, pos,
                                   edgelist=dashed_edges,
                                   edge_color='#777777',
                                   arrows=True,
                                   arrowsize=40,
                                   arrowstyle='-|>',
                                   width=3.0,
                                   alpha=0.8,
                                   style='dashed',
                                   connectionstyle='arc3,rad=0.15',
                                   min_source_margin=70,
                                   min_target_margin=70)

        # Draw nodes — one call per field so each field gets its own shape
        for field_key, shape in mpl_shapes.items():
            nodes_in_field = [n for n in self.G.nodes()
                              if self.G.nodes[n].get('field') == field_key]
            if nodes_in_field:
                field_color = self.fields[field_key]['color']
                nx.draw_networkx_nodes(self.G, pos,
                                       nodelist=nodes_in_field,
                                       node_color=field_color,
                                       node_shape=shape,
                                       node_size=16000,
                                       alpha=0.95,
                                       edgecolors='black',
                                       linewidths=2)

        # Draw labels (larger font for legibility)
        labels = {node: f"{node}\n{self.G.nodes[node]['dates']}"
                  for node in self.G.nodes}
        nx.draw_networkx_labels(self.G, pos, labels,
                                font_size=14,
                                font_weight='bold')

        # Shape-based legend (each field gets a marker matching its node shape)
        from matplotlib.lines import Line2D
        legend_elements = [
            Line2D([0], [0],
                   marker=mpl_shapes[field_key],
                   color='w',
                   markerfacecolor=self.fields[field_key]['color'],
                   markeredgecolor='black',
                   markeredgewidth=1.5,
                   markersize=20,
                   label=self.fields[field_key]['name'],
                   linestyle='None')
            for field_key in ['control', 'neural', 'bayesian', 'causal']
        ]
        if dashed_edges:
            legend_elements.extend([
                Line2D([0], [0], color='#333333', linewidth=2.5, label='Direct influence'),
                Line2D([0], [0], color='#999999', linewidth=2.5, linestyle='--', label='Social connection')
            ])
        # Place legend OUTSIDE the plot area on the lower right so it doesn't
        # overlap with any nodes (the top row in particular).
        ax.legend(handles=legend_elements,
                  loc='lower center',
                  bbox_to_anchor=(0.5, -0.05),
                  ncol=3,
                  framealpha=0.95, fontsize=15)

        ax.axis('off')
        ax.margins(0.15)
        plt.tight_layout()

        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')

        return fig, ax


# Initialize the genealogy
gen = IntellectualGenealogy()

# =============================================================================
# PERIOD 0: Pre-1950s Foundations
# =============================================================================

gen.add_person(
    "Wright",
    "1889-1988",
    [{"desc": "Path analysis, evolutionary genetics",
      "citation": "Wright (1921, 1934)"}],
    period=0,
    field='causal'
)

gen.add_person(
    "Burks",
    "1902-1943",
    [{"desc": "Nature-nurture studies, applied path analysis",
      "citation": "Burks (1928)"}],
    period=0,
    field='causal'
)

gen.add_person(
    "Shannon",
    "1916-2001",
    [{"desc": "Information theory",
      "citation": "Shannon (1948)"}],
    period=0,
    field='bayesian'
)

# =============================================================================
# PERIOD 1: 1950s-early 1960s
# =============================================================================

# Control Theory
gen.add_person(
    "Pontryagin",
    "1908-1988",
    [{"desc": "Maximum principle in optimal control",
      "citation": "Pontryagin et al. (1956), presented at 1960 IFAC Congress"}],
    period=1,
    field='control'
)

gen.add_person(
    "Bellman",
    "1920-1984",
    [{"desc": "Dynamic programming, HJB equation",
      "citation": "Bellman (1957), presented at 1960 IFAC Congress"}],
    period=1,
    field='control'
)

gen.add_person(
    "Kalman",
    "1930-2016",
    [{"desc": "Kalman filter, controllability, observability",
      "citation": "Kalman (1960), presented at 1960 IFAC Congress"}],
    period=1,
    field='control'
)

gen.add_person(
    "Dreyfus",
    "1926-present",
    [{"desc": "Applied dynamic programming, early RL connections",
      "citation": "Dreyfus (1962) Applied Dynamic Programming with Bellman"}],
    period=1,
    field='control'
)

# Neural Networks
gen.add_person(
    "Rosenblatt",
    "1928-1971",
    [{"desc": "Perceptron",
      "citation": "Rosenblatt (1958)"}],
    period=1,
    field='neural'
)

# Bayesian/Statistical - The Savage Brothers
gen.add_person(
    "I.R. Savage",
    "1925-2004",
    [{"desc": "Nonparametric statistics",
      "citation": "Cited by Bellman (1957)"}],
    period=1,
    field='bayesian'
)

gen.add_person(
    "L.J. Savage",
    "1917-1971",
    [{"desc": "Foundations of Bayesian statistics",
      "citation": "Savage (1954) The Foundations of Statistics"}],
    period=1,
    field='bayesian'
)

# =============================================================================
# PERIOD 2: Late 1960s-1970s
# =============================================================================

gen.add_person(
    "Bryson & Ho",
    "1960s-70s",
    [{"desc": "Applied optimal control",
      "citation": "Bryson & Ho (1969)"}],
    period=2,
    field='control'
)

gen.add_person(
    "Minsky",
    "1927-2016",
    [{"desc": "Perceptrons limitations",
      "citation": "Minsky & Papert (1969)"}],
    period=2,
    field='neural'
)

gen.add_person(
    "Werbos",
    "1947-present",
    [{"desc": "Backpropagation",
      "citation": "Werbos (1974)"}],
    period=2,
    field='neural'
)

# Causal Inference pioneer
gen.add_person(
    "Cornfield",
    "1912-1979",
    [{"desc": "Causal inference in epidemiology, Cornfield's inequality",
      "citation": "Cornfield (1959) Principles of Research"}],
    period=2,
    field='causal'
)

# =============================================================================
# PERIOD 3: 1980s-1990s
# =============================================================================

gen.add_person(
    "Hinton",
    "1947-present",
    [{"desc": "Backprop revival, deep learning",
      "citation": "Rumelhart, Hinton, Williams (1986)"}],
    period=3,
    field='neural'
)


# Causal inference with time-varying treatments
gen.add_person(
    "Robins",
    "1950s-present",
    [{"desc": "G-computation, time-varying treatments",
      "citation": "Robins (1986, 1987)"}],
    period=3,
    field='causal'
)

gen.add_person(
    "Watkins",
    "1960s-present",
    [{"desc": "Q-learning algorithm",
      "citation": "Watkins (1989) PhD thesis"}],
    period=3,
    field='neural'
)

# =============================================================================
# PERIOD 4: 1990s-2000s
# =============================================================================

gen.add_person(
    "Pearl",
    "1936-present",
    [{"desc": "Causal graphical models",
      "citation": "Pearl (2000)"}],
    period=4,
    field='causal'
)

# THE ACTUAL BRIDGE: Bellman equations for treatment regimes
gen.add_person(
    "Murphy",
    "1960s-present",
    [{"desc": "Bellman equations for dynamic treatment regimes",
      "citation": "Murphy et al. (2003)"}],
    period=4,
    field='causal'
)

gen.add_person(
    "Rubin",
    "1943-present",
    [{"desc": "Potential outcomes framework",
      "citation": "Rubin (1974, 2005)"}],
    period=4,
    field='causal'
)

# =============================================================================
# PERIOD 5: 2010s
# =============================================================================

gen.add_person(
    "Mnih",
    "1984-present",
    [{"desc": "Deep Q-learning",
      "citation": "Mnih et al. (2015)"}],
    period=5,
    field='neural'
)

# =============================================================================
# INFLUENCES: Pre-1950s Foundations
# =============================================================================

gen.add_influence("Wright", "Pearl",
                  "Path analysis foundational to causal graphs")

gen.add_influence("Wright", "Burks",
                  "Applied path analysis to nature-nurture studies")

gen.add_influence("Burks", "Shannon",
                  "Influence on information theory development")

gen.add_influence("Shannon", "Bellman",
                  "Information theory influenced dynamic programming")

# =============================================================================
# INFLUENCES: Control Theory → Neural Networks
# =============================================================================

gen.add_influence("Bellman", "Kalman",
                  "Dynamic programming and optimal filtering, 1960 IFAC Congress")

gen.add_influence("Pontryagin", "Bryson & Ho",
                  "Applied maximum principle")
gen.add_influence("Bryson & Ho", "Werbos",
                  "Optimal control formulation")
gen.add_influence("Rosenblatt", "Werbos",
                  "Perceptron")
gen.add_influence("Werbos", "Hinton",
                  "Disputed/independent")
gen.add_influence("Rosenblatt", "Minsky",
                  "Critiqued limitations")
gen.add_influence("Minsky", "Hinton",
                  "Motivated multilayer nets")
gen.add_influence("Rosenblatt", "Hinton",
                  "Foundational work")
gen.add_influence("Hinton", "Mnih",
                  "PhD advisor")

# =============================================================================
# INFLUENCES: Control Theory → RL via Dreyfus
# =============================================================================

gen.add_influence("Bellman", "Dreyfus",
                  "Co-authored Applied Dynamic Programming (1962)")

gen.add_influence("Dreyfus", "Rosenblatt",
                  "Early neural network connections")

gen.add_influence("Dreyfus", "Watkins",
                  "Dynamic programming foundations for Q-learning")

gen.add_influence("Watkins", "Mnih",
                  "Q-learning algorithm basis for DQN")

gen.add_influence("Bellman", "Mnih",
                  "Bellman equation in Q-learning")

# =============================================================================
# THE PATH THAT WASN'T TAKEN: Social network without intellectual bridge
# =============================================================================

# Bellman cited I.R. Savage
gen.add_influence("I.R. Savage", "Bellman",
                  "Cited in Dynamic Programming (1957)")

# The Savage brothers exchanged ideas
gen.add_influence("I.R. Savage", "L.J. Savage",
                  "Brothers, exchange of ideas")

# L.J. Savage influenced Cornfield through correspondence
gen.add_influence("L.J. Savage", "Cornfield",
                  "1953 ASA meeting, correspondence on Bayesian methods")

# Cornfield influenced the potential outcomes tradition
gen.add_influence("Cornfield", "Rubin",
                  "Cornfield's insights on causality (Rubin 2012)")

# =============================================================================
# THE BRIDGE THAT WAS ACTUALLY BUILT (decades later)
# =============================================================================

# Robins extended causal inference to sequential treatments
gen.add_influence("Cornfield", "Robins",
                  "Causal inference in observational studies")

# Murphy explicitly connected Bellman to causal inference
gen.add_influence("Bellman", "Murphy",
                  "Bellman equations for optimal dynamic treatment regimes")
gen.add_influence("Robins", "Murphy",
                  "Time-varying treatment methods")

# Pearl's independent development
gen.add_influence("Bellman", "Pearl",
                  "Early AI search work (1980s)")

# Print summary organized by field
print("=== INTELLECTUAL GENEALOGY ===\n")

for field_key in ['causal', 'bayesian', 'control', 'neural']:
    print(f"\n{'=' * 60}")
    print(f"FIELD: {gen.fields[field_key]['name'].upper()}")
    print('=' * 60)
    for person in sorted(gen.G.nodes, key=lambda x: gen.G.nodes[x].get('period', 999)):
        if gen.G.nodes[person].get('field') == field_key:
            print(gen.get_person_info(person))

# Export to Graphviz
print("\n" + "=" * 60)
gen.export_to_graphviz(filename='genealogy.dot',
                       layout='LR',
                       include_edge_labels=True,
                       include_accomplishments=False,
                       chronological=True)

# Create matplotlib version
print("\n" + "=" * 60)
print("Creating matplotlib visualization...")
fig, ax = gen.visualize_matplotlib(save_path='genealogy_matplotlib.png')
plt.show()