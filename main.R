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

print(au)

convertToM = function(au){
	au[,1] = au[,1] * 0.01 # convert l/2 to m
}

au[,1] = convertToM(au)

Nmin = au[,3] / au[,2]
dlt_Nmin = au[,3] * dlt_t / au[,2]^2
sin05theta = a / sqrt(a^2 + au[,1]	^2)
dlt_sin05theta = a * au[,1] * dlt_L * (a^2 + au[,1]^2)^(-1.5)
theta = 2 * asin(sin05theta) * 180 / pi
dlt_theta = 2 * dlt_sin05theta / sqrt(1 - sin05theta^2)
r2 = sqrt(a^2 + au[,1]^2)
dlt_r2 = au[,1] * dlt_L * (a^2 + au[,1]^2)^(-0.5)

au_means = data.frame(Nmin, dlt_Nmin, sin05theta, dlt_sin05theta, theta,
	dlt_theta, r2, dlt_r2)

print(au)
print(au_means)