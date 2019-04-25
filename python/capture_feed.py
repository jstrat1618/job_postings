import feedparser
import ssl
import collections
import json
import datetime

Job = collections.namedtuple("Job", ['id', 'title', 'link', 'location', 'summary'])


def parse_feed(url = 'https://stackoverflow.com/jobs/feed'):
    # Allow unverified ssl; what is an ssl?- https://www.digicert.com/ssl/
    if hasattr(ssl, '_create_unverified_context'):
        ssl._create_default_https_context = ssl._create_unverified_context

    rss_feed = feedparser.parse(url)

    return rss_feed


def parse_entries(feed):
    entries = feed['entries']

    print(len(entries))

    data = {}
    num_excpt = 0

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


def write_data(data):

    date_str = str(datetime.datetime.now().date())

    filename = '../venv/data/' + date_str + '_jobs.json'

    with open(filename, 'w') as job_data_file:
        json.dump(data, job_data_file)


def main():
    rss_feed = parse_feed()

    job_data = parse_entries(rss_feed)

    write_data(job_data)


if __name__ == '__main__':
    main()





