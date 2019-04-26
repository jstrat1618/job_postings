import os
import json
import collections
import pandas as pd

Job = collections.namedtuple("Job", ['id', 'title', 'link', 'location', 'summary'])


def load_data():
    data_path = os.path.abspath("../venv/data/2019-04-25_jobs.json")

    with open(data_path) as file:
        data = json.load(file)

    return data


def process_data(data):

    dat = {}

    for key,val in data.items():
        id,title,link,location,summary = val
        dat['key'] = Job(id=id, title=title, link=link, location=location,summary=summary)

    return dat


def transform_to_csv(processed_data):

    id,title,link,location,summary = [],[],[],[],[]

    for key,val in processed_data.items():
        id.append(key)
        title.append(val.title)
        link.append(val.link)
        location.append(val.location)
        summary.append(summary)

    df = pd.DataFrame({'id':id, 'title':title, 'link':link, 'location':location, 'summary':summary})

    df.to_csv('../venv/2019_04_25_jobs.csv')





def main():
    data = load_data()

    dat = process_data(data)

    transform_to_csv(dat)

    print("Procssed Finished.")
    print("Goodbye!")


if __name__ == '__main__':
    main()