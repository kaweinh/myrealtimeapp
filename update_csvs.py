import pandas as pd
import numpy as np
import pyodbc
import textwrap

print("Connecting to REX DB...")
conn = pyodbc.connect('Driver={ODBC Driver 13 for SQL Server};'
					 'Server=rexprd-db1.rex.aws.umbc.edu;'
					 'Database=iPSSA;'
					 'Port=****;'
					 'UID=*****;'
					 'PWD=******')
print("Connected")

def create_query_string(sql_full_path):
	with open(sql_full_path, 'r') as f_in:
		lines = f_in.read()
 
	# remove any common leading whitespace from every line    
	query_string = textwrap.dedent("""{}""".format(lines))
 
	return query_string

def update_csvs():
	#Get Data for all classes
	print("Retreiving Data...")

	query_string = 	create_query_string(r'2018_stats.sql') 
	df_2018 = pd.read_sql(query_string,conn)
	df_2018['ApplicationDay'] = np.arange(1, df_2018.shape[0] + 1)

	query_string = 	create_query_string(r'2019_stats.sql') 
	df_2019 = pd.read_sql(query_string,conn)
	df_2019['ApplicationDay'] = np.arange(1, df_2019.shape[0] + 1)

	query_string = 	create_query_string(r'2020_stats.sql') 
	df_2020 = pd.read_sql(query_string,conn)
	df_2020['ApplicationDay'] = np.arange(1, df_2020.shape[0] + 1)

	print("Writing files...")

	df_2018.to_csv('Fall18NewFreshman.csv')
	df_2019.to_csv('Fall19NewFreshman.csv')
	df_2020.to_csv('Fall20NewFreshman.csv')

	print("Done")

def main():
	update_csvs()

main()