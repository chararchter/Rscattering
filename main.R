# parametri		
dlt_L = 0.001 #m
dlt_t = 0.08	#min
a = 0.02		#m
z_Au = 79
z_Al = 13
df_Au = 1.50E-06	#m
df_Al = 8.00E-06	#m

column = c("half_l", "t_min", "N")
au = read.csv(file="2au.csv", header=FALSE, sep=",", col.names=column)
al = read.csv(file="2al.csv", header=FALSE, sep=",", col.names=column)

convertToM = function(au){
	au[,1] = au[,1] * 0.01 # convert l/2 to m
}

processData = function(data){
	Nmin = data[,3] / data[,2]
	dlt_Nmin = data[,3] * dlt_t / data[,2]^2
	sin05theta = a / sqrt(a^2 + data[,1]	^2)
	dlt_sin05theta = a * data[,1] * dlt_L * (a^2 + data[,1]^2)^(-1.5)
	theta = 2 * asin(sin05theta) * 180 / pi
	dlt_theta = 2 * dlt_sin05theta / sqrt(1 - sin05theta^2)
	r2 = sqrt(a^2 + data[,1]^2)
	dlt_r2 = data[,1] * dlt_L * (a^2 + data[,1]^2)^(-0.5)
	r2sin = 1/(r2^2 * sin05theta^4) / 10000
	dlt_r2sin = sqrt((2 * dlt_r2 * r2sin/r2)^2 +
		(4 * dlt_sin05theta * r2sin/sin05theta)^2)
	data_means = data.frame(Nmin, dlt_Nmin, sin05theta, dlt_sin05theta, theta,
		dlt_theta, r2, dlt_r2, r2sin, dlt_r2sin)
}

au[,1] = convertToM(au)
al[,1] = convertToM(al)

au_means = processData(au)
al_means = processData(al)

print('Aurum')
print(au)
print(au_means)
print('Aluminium')
print(al)
print(al_means)