# Basic tests for the cristin_graph module

from cristin_graph import create_weighted_graph, save_graph, load_graph
from author import Author

def test_create_weighted_graph():
    # Create a small list of Author instances for testing
    authors = [Author("Ola", "Nordmann", "1"), Author("Kari", "Nordkvinn", "2")]
    authors[0].pubids = (1, 2)
    authors[1].pubids = (2, 3)

    graph = create_weighted_graph(authors)

    # Test if the graph is created correctly
    assert len(graph.nodes) == 2
    assert len(graph.edges) == 1
    assert ('1', '2') in graph.edges or ('2', '1') in graph.edges

def test_graph_serialization_and_deserialization(tmp_path):
    authors = [Author("Ola", "Nordmann", "1"), Author("Kari", "Nordkvinn", "2")]
    authors[0].pubids = (1, 2)
    authors[1].pubids = (2, 3)

    graph = create_weighted_graph(authors)
    test_graph_filename = tmp_path / 'test_graph'
    save_graph(graph, filename=str(test_graph_filename))

    loaded_graph = load_graph(filename=str(test_graph_filename.with_suffix('.pkl')))

    # Test if graph is loaded correctly
    assert len(loaded_graph.nodes) == 2
    assert len(loaded_graph.edges) == 1
    assert ('1', '2') in loaded_graph.edges or ('2', '1') in loaded_graph.edges

