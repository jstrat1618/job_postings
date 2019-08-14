import sqlite3
import pandas as pd
import mysql.connector
from sqlalchemy import create_engine

# Connect to sqlite db
def conn_sqlite_db():
    lite_conn = sqlite3.connect('../venv/data/jobs_database.db')
    df = pd.read_sql_query('SELECT * FROM main_jobs', lite_conn)

    lite_conn.close()

    return df

def clean_data(df):
    df['pulled'] = pd.to_datetime(df['pulled'], format='%m/%d/%Y')

    df['id'] = df['id'].astype('int')

    return df

def commit2mysql(df):

    user = input("Input user: ")
    pwd = input("Input password: ")
    conn = mysql.connector.connect(
        host="localhost",
        user='root',
        passwd='Lqsym1123!',
        auth_plugin='mysql_native_password',
        database="job_posts"
    )

    str_init = 'mysql+mysqlconnector://{0}:{1}@localhost:3306/job_posts'.format(user, pwd)
    engine = create_engine(str_init, echo=False)
    df.to_sql(name='main_posts', con=engine, if_exists='replace', index=False)

    conn.close()


if __name__ == '__main__':
    old_df = conn_sqlite_db()
    new_df = clean_data(old_df)

    commit2mysql(new_df)
    print("Completed")




