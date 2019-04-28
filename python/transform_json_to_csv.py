import os
import json
import pandas as pd


def load_data():
    data_path = os.path.abspath("../venv/data/2019-04-25_jobs.json")

    with open(data_path) as file:
        data = json.load(file)

    return data


def transform_data(data):

    df = pd.DataFrame.from_dict(data, orient='index',
                                columns=['id', 'title', 'link', 'location', 'summary'])

    df.to_csv('../venv/data/2019_04_25_jobs.csv', index=False)



def main():
    data = load_data()

    transform_data(data)

    print("Goodbye")


if __name__ == '__main__':
    main()