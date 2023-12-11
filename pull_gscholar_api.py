import os
import json
from typing import List, Dict, Optional
from scholarly import scholarly
import click
from tqdm import tqdm


import logging
#logging.basicConfig(level=logging.INFO)

def get_single_author(researcher: str) -> Optional[Dict]:
    """
    Queries Google Scholar for the given researcher and returns a single author object.

    :param researcher: The name of the researcher to query.
    :return: A dictionary representing the selected author or None if no valid selection is made.
    """
    query: List[Dict] = list(scholarly.search_author(researcher))

    if not query:
        tqdm.write(f"No profiles found for {researcher}.")
        return None



    # Sort authors by 'University of Oslo' affiliation, then by 'citedby'
    query = sorted(query, key=lambda x: (x.get('affiliation') != 'University of Oslo', -x.get('citedby', 0)))


    # # Add ipynb breakpoint:
    # from IPython.core.debugger import set_trace
    # set_trace()

    # First check if there are more than one author with affiliation 'University of Oslo'
    # Check if 'UiO', 'uio', 'Oslo' or 'oslo' is in the affiliation string
    if len([q for q in query if 'uio' in q.get('affiliation', '').lower() or 'oslo' in q.get('affiliation', '').lower()]) == 1:
        tqdm.write(f"Found a single profile for '{researcher}' with UiO affiliation, going with this.")
        return query[0]

    elif len(query) == 1:
        print(f"Found a single profile for '{researcher}', going with this.")
        return query[0]

    elif len(query) > 1:
        tqdm.write(f"Multiple profiles found for '{researcher}'.")
        for i, q in enumerate(query):
            tqdm.write(f"{i + 1}. Name: {q.get('name')}, Affiliation: {q.get('affiliation')}, "
                  f"Email domain: {q.get('email_domain')}, Cited by: {q.get('citedby')}")

        try:
            selection = int(input("Please select the correct profile by number or enter a different ID: "))
            if 1 <= selection <= len(query):
                return query[selection - 1]
            else:
                tqdm.write("Invalid selection number.")
                return None
        except ValueError:
            tqdm.write("Invalid input. Please enter a number.")
            return None
    else:
        tqdm.write(f"No profiles found for '{researcher}'.")
        return None


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
        tqdm.write("No valid scholar ID found in the author object.")
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
        fileop = input(f"The file '{filename}' already exists. Do you want to append/modify [default], create a new file, or abort? ([append]/overwrite/abort): ").strip().lower()
        if fileop == 'overwrite':
            # Save the data
            with open(filename, 'w', encoding='utf-8') as f:
                json.dump(data, f, indent=4)
            tqdm.write(f"Data successfully saved to '{filename}'.")
            return
        if fileop == 'abort':
            tqdm.write("Operation cancelled. Data not saved.")
            return
        else:
            # Append the data
            with open(filename, 'r+', encoding='utf-8') as f:
                file_data = json.load(f)
                file_data.update(data)
                f.seek(0)
                json.dump(file_data, f, indent=4)


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

    for r in tqdm(researchers, desc="Processing Researchers"):
        tqdm.write(f"Finding author: {r}")
        selected_author = get_single_author(researcher=r)
        if selected_author:
            tqdm.write(f"Filling data for author: {selected_author.get('name')}")
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
