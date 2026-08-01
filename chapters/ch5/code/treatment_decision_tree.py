# treatment_decision_tree.py
# Treatment decision tree with recursive visualization (Chapter 5).
# Note: rendering the graph to an image additionally requires the
# Graphviz system binaries (https://graphviz.org); building the graph
# object and predicting treatments works without them.

from graphviz import Digraph


class TreatmentNode:
    def __init__(self, feature=None, threshold=None, treatment=None,
                 node_id=None):
        self.feature = feature      # e.g., "age", "smoking_status"
        self.threshold = threshold  # e.g., 65
        self.treatment = treatment  # e.g., "drug1", "drug2"
        self.node_id = node_id      # unique name used for plotting
        self.left = None            # patients meeting condition
        self.right = None           # patients not meeting condition

    def get_label(self):
        if self.treatment:  # leaf node
            return self.treatment
        return f"{self.feature} > {self.threshold}?"

    def predict_treatment(self, patient):
        if self.treatment:  # leaf node
            return self.treatment

        if patient[self.feature] <= self.threshold:
            return self.left.predict_treatment(patient)
        else:
            return self.right.predict_treatment(patient)


# Build the treatment decision tree
root = TreatmentNode(feature="age", threshold=65, node_id="root")
one = TreatmentNode(treatment="Drug 1", node_id="young")
two = TreatmentNode(feature="smoking_status", threshold=0,
                    node_id="elderly_check")
three = TreatmentNode(treatment="Drug 1", node_id="elderly_nonsmoker")
four = TreatmentNode(treatment="Drug 2", node_id="elderly_smoker")
root.left = one
root.right = two
two.left = three
two.right = four


def visualize_treatment_tree(node, graph=None, parent=None,
                             edge_label=""):
    if graph is None:
        graph = Digraph()
        graph.attr(rankdir='TB')  # Top to bottom layout

    if node:
        # Create node with descriptive label and styling
        if node.treatment:  # Treatment node (leaf)
            graph.node(node.node_id,
                       label=node.get_label(),
                       shape='box',
                       style='filled',
                       fillcolor=
                       'lightgreen' if 'Drug 1' in node.treatment
                       else 'lightcoral')
        else:  # Decision node
            graph.node(node.node_id,
                       label=node.get_label(),
                       shape='ellipse',
                       style='filled',
                       fillcolor='lightblue')

        # Create edge with label
        if parent:
            graph.edge(parent.node_id, node.node_id, label=edge_label)

        # Recursively visualize children
        if node.left:
            visualize_treatment_tree(node.left, graph, node, "No")
        if node.right:
            visualize_treatment_tree(node.right, graph, node, "Yes")

    return graph


if __name__ == "__main__":
    # Example predictions
    marcus = {"age": 70, "smoking_status": 1}
    livia = {"age": 50, "smoking_status": 0}
    print("Marcus (age 70, smoker):    ",
          root.predict_treatment(marcus))
    print("Livia  (age 50, non-smoker):",
          root.predict_treatment(livia))

    graph = visualize_treatment_tree(root)
    print(graph.source)
    # To write an image (requires the Graphviz binaries):
    # graph.render("treatment_tree", format="png", cleanup=True)
