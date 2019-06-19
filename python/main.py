import datetime
import feedparser
import ssl
import collections
import sqlite3
import pandas as pd

Job = collections.namedtuple("Job", ['id', 'title', 'link', 'location', 'summary'])


def parse_feed(url = 'https://stackoverflow.com/jobs/feed'):
    # Allow unverified ssl; what is an ssl?- https://www.digicert.com/ssl/
    if hasattr(ssl, '_create_unverified_context'):
        ssl._create_default_https_context = ssl._create_unverified_context

    rss_feed = feedparser.parse(url)

    return rss_feed


def parse_entries(feed):
    entries = feed['entries']

    data = {}

    for entry in entries:

        job = job_info(entry)

        data[job.id] = job

    return data


def job_info(entry):
    job_id = get_element(entry, 'id')

    job_title = get_element(entry, 'title')

    job_link = get_element(entry, 'link')

    job_loc = get_element(entry, 'location')

    job_summary = get_element(entry, 'summary')

    job = Job(id=job_id, title=job_title, link=job_link, location=job_loc, summary=job_summary)

    return job


def get_element(x, key):

    try:
        return x[key]

    except KeyError:
        return ''


def transform_data(data):

    df = pd.DataFrame.from_dict(data, orient='index',
                                columns=['id', 'title', 'link', 'location', 'summary'])

    tdy = datetime.datetime.now().date().strftime('%m/%d/%Y')

    df['pulled'] = tdy

    return df


def load_old_data():
    conn = sqlite3.connect('../venv/data/jobs_database.db')
    df = pd.read_sql_query('SELECT * FROM main_jobs', conn)

    conn.close()

    return df


def filter_new_data(new_data, old_data):
    diff = set(new_data.id).difference(old_data.id)

    return new_data[new_data.id.isin(diff)]


def commit_data(data2commit):
    conn = sqlite3.connect('../venv/data/jobs_database.db')
    cur = conn.cursor()

    for index, job in data2commit.iterrows():
        cur.execute("INSERT INTO main_jobs VALUES(?, ?, ?, ?, ?, ?)",
                    (job.id, job.title, job.link, job.location, job.summary, job.pulled))

    conn.commit()
    conn.close()





def main():
    old_data = load_old_data()

    feed = parse_feed()

    cleaned_feed = parse_entries(feed)

    new_data = transform_data(cleaned_feed)

    df2commit = filter_new_data(new_data, old_data)

    commit_data(df2commit)


    print("Committed {} rows".format(df2commit.shape[0]))
    print("Finished")




if __name__ == '__main__':
    main()