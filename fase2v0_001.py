import MySQLdb as db
import sys
con = db.connect('localhost','root', '01K4m7Ra')
cur = con.cursor()
cur.execute('SHOW DATABASES;')
todas = cur.fetchall()
bandera=0

for base in todas:
	if base[0] == 'corridas':
		bandera=1

if bandera == 1:
	cur.execute('DROP DATABASE corridas;')
	cur.execute("DROP USER 'userRNAf'@'localhost';")

cur.execute('CREATE DATABASE corridas;')
cur.execute('USE corridas;')
cur.execute('CREATE TABLE combinaciones (nCP INT, colsP CHAR(30), colsQ CHAR(30), hidden INT, lr DOUBLE, mg DOUBLE, ss DOUBLE, picked INT, done INT, mape DOUBLE, sigma DOUBLE, lmls DOUBLE,  tf DOUBLE, tff DOUBLE, colObj INT);')
cur.execute("CREATE USER 'userRNAf'@'localhost' IDENTIFIED BY 'imt123';")
cur.execute("GRANT ALL ON corridas.* TO 'userRNAf'@'localhost';")

if con:
	con.close()
