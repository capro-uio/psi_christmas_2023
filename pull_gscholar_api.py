# %%
import os
import json
from typing import List, Dict, Optional
from scholarly import scholarly
import click

def get_single_author(researcher: str) -> Optional[Dict]:
    """
    Queries Google Scholar for the given researcher and returns a single author object.

    :param researcher: The name of the researcher to query.
    :return: A dictionary representing the selected author or None if no valid selection is made.
    """
    query: List[Dict] = list(scholarly.search_author(researcher))

    if not query:
        print(f"No profiles found for {researcher}.")
        return None

    # Sort authors by 'University of Oslo' affiliation, then by 'citedby'
    query = sorted(query, key=lambda x: (x.get('affiliation') != 'University of Oslo', -x.get('citedby', 0)))

    if len(query) > 1:
        print(f"Multiple profiles found for '{researcher}'.")
        for i, q in enumerate(query):
            print(f"{i + 1}. Name: {q.get('name')}, Affiliation: {q.get('affiliation')}, "
                  f"Email domain: {q.get('email_domain')}, Cited by: {q.get('citedby')}")

        try:
            selection = int(input("Please select the correct profile by number or enter a different ID: "))
            if 1 <= selection <= len(query):
                return query[selection - 1]
            else:
                print("Invalid selection number.")
                return None
        except ValueError:
            print("Invalid input. Please enter a number.")
            return None
    else:
        return query[0]


def fill_author(author: dict):
    """
    Fills an author object with additional details using the scholarly library.

    :param author: The author object (dictionary) to be filled with more data.
    :return: The updated author object with additional details.
    """
    if 'scholar_id' in author:
        author_details = scholarly.fill(author)
        return author_details
    else:
        print("No valid scholar ID found in the author object.")
        return None


def store_data(data: dict, filename: str = 'data/data.json'):
    """
    Stores the data in the given dictionary in a file. Asks for confirmation if the file already exists.

    :param data: The data to be stored.
    :param filename: The filename where the data will be stored.
    :return: None
    """
    # Check if the file already exists
    if os.path.exists(filename):
        overwrite = input(f"The file '{filename}' already exists. Do you want to overwrite it? (yes/no): ").strip().lower()
        if overwrite not in ['yes', 'y']:
            print("Operation cancelled. Data not saved.")
            return

    # Save the data
    with open(filename, 'w', encoding='utf-8') as f:
        json.dump(data, f, indent=4)
    print(f"Data successfully saved to '{filename}'.")

@click.command()
@click.argument('researchers', nargs=-1)
@click.option('--file', '-f', 'file_path', type=click.Path(exists=True), help="Path to a file containing researcher names, one per line.")
def run(researchers: List[str], file_path: str):
    """
    Run the script for a list of researchers.

    :param researchers: A list of researcher names.
    :param file_path: Path to a file containing researcher names.
    """
    if not researchers and not file_path:
        print("No researchers provided. Please provide researchers as arguments or via a file.")
        return

    data = {}

    # Read researchers from file if provided
    if file_path:
        with open(file_path, 'r') as file:
            researchers.extend(file.read().splitlines())

    for r in researchers:
        print("Finding author: ", r)
        selected_author = get_single_author(researcher=r)
        if selected_author:
            print("Filling data for author: ", selected_author.get('name'))
            filled_author = fill_author(selected_author)
        else:
            continue

        if filled_author:
            data[r] = filled_author
        else:
            continue

    store_data(data=data)

    return data

if __name__ == '__main__':
    run()
