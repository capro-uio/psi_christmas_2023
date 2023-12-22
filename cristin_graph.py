#!/usr/bin/env python3
import json
import pickle
from pathlib import Path
from typing import List

import click
import matplotlib.pyplot as plt
import networkx as nx  # type: ignore
from tqdm import tqdm

from author import Author


def create_weighted_graph(authors: List[Author]) -> nx.Graph:
    """
    Creates a weighted graph from the given data.
    Weights are based on the number of co-authored publications.

    :param authors: A list of Author objects.
    :return: A networkx graph object.
    """

    G = nx.Graph()
    for author in authors:
        # Use cristinid as node and store Author data in node attributes
        G.add_node(author.cristinid, data=author.to_json())

    for i, author in enumerate(authors):
        for pubid in author.pubids:
            for j in range(i + 1, len(authors)):
                coauthor = authors[j]
                if pubid in coauthor.pubids:
                    # Use cristinid to check edges and add edges
                    if G.has_edge(author.cristinid, coauthor.cristinid):
                        G[author.cristinid][coauthor.cristinid]['weight'] += 1
                    else:
                        G.add_edge(author.cristinid, coauthor.cristinid, weight=1)
    return G


def save_graph(graph: nx.Graph, filename: str = 'data/graph') -> None:
    """
    Save the graph in JSON and pickled format.
    """
    for node, data in graph.nodes(data=True):
        if isinstance(data.get('data'), Author):  # Ensure only Author instances are converted
            data['data'] = data['data'].to_json()

    # Convert the graph to a node-link data format
    graph_data = nx.node_link_data(graph)

    # Save the graph in JSON format
    with open(f"{filename}.json", "w", encoding="utf-8") as f:
        json.dump(graph_data, f)

    # Save the graph to a pickled file
    with open(f"{filename}.pkl", "wb") as f:
        pickle.dump(graph, f)



def load_graph(filename: str = 'data/graph.pkl') -> nx.Graph:
    """
    Load the graph from a pickled file.
    """
    with open(filename, "rb") as f:
        graph_loaded = pickle.load(f)

    return graph_loaded


def display_graph(graph: nx.Graph) -> None:
    """
    Display the graph using matplotlib.

    :param graph: A networkx graph object.
    :param pos: Optional position mapping for graph nodes.
    """

    pos = nx.spring_layout(graph)  # or any other layout

    edge_weights = nx.get_edge_attributes(graph, 'weight')

    fig = plt.figure(figsize=(8, 6), facecolor='white')

    nx.draw_networkx_nodes(graph, pos, node_color='skyblue', node_size=700)

    for (node1, node2), weight in edge_weights.items():
        nx.draw_networkx_edges(graph, pos, edgelist=[(node1, node2)], width=weight*0.1)

    labels = {}
    for node in graph.nodes:
        # Access the author's data stored in the node's attributes
        author_data = graph.nodes[node].get('data')
        if author_data:
            labels[node] = author_data['surname']  # Extract the surname from the author's data

    nx.draw_networkx_labels(graph, pos, labels=labels, font_size=10, font_family="sans-serif")

    plt.axis("off")
    plt.show()

@click.command()
@click.option('--file_path', default='data/cristin.tsv', help='Path to file containing researchers.',
              type=click.Path(exists=True))
@click.option('--output', default='data/graph', help='Path to save graph.')
@click.option('--display', default=False, help='Display graph.')
def run(file_path: str, output: str, display: bool) -> None:
    """
    Run the script for a list of researchers.

    :param authors: A list of author names.
    :param file_path: Path to a file containing researcher names.
    """
    if not file_path:
        print("No file path provided. Exiting...")
        exit()

    # Read researchers from file if provided
    authors = []
    res_tmp = []
    with open(file_path, 'r', encoding='utf-8') as file:
        res_tmp = file.read().splitlines()
    for r in tqdm(res_tmp[1:], desc='Processing researchers'):
        firstname = r.split('\t')[0]
        surname = r.split('\t')[1]
        cristinid = r.split('\t')[2]
        # Create list of Author objects
        author = Author(firstname=firstname,
                        surname=surname,
                        cristinid=cristinid)
        if author.pubids:
            authors.append(author)

    graph = create_weighted_graph(authors)
    save_graph(graph, filename=output)
    if display:
        display_graph(graph)


if __name__ == '__main__':
    run() # pylint: disable=no-value-for-parameter
