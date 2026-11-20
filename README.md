# Metodos Estadisticos
## curso de metodos estadisticos 2026
**semana dos: inicio de curso metodos estadisticos**
+ preparar area de trabajo
+ crear mi area de trabajo sherline22
+ crear mi primer repositorio
+ modificar el archivo _README_
#importar datos ---
 
11
#usar la funcion "read.cvs" para importar datos
 
12
 
13
exp <- read.csv("D:/repositorio/Met_est_2026/vivero.csv",
 
14
                header = TRUE)
 
15
exp$Tratamiento <- as.factor(exp$Tratamiento)
 
16
exp$Tratamiento
 
17
 
18
#Grafica----
 
19
 
20
#Boxplot de los datos
 
21
 
22
boxplot(exp$IE ~ exp$Tratamiento,
 
23
        xlab = "Factor Fertilizante",
 
24
        ylab = "Índice (IE)",
 
25
        col = "green",
 
26
        main = "Unidad experimental")
 
27
    
 
28
file.choose()
 
29
 
30
# Gastos totales
 
31
 
32
300 + 240 + 1527 + 400 + 1500 + 1833
 
33
5800
 
34
 
 
35
celular <- 300
 
36
celular
 
37
300
 
38
 
 
39
transporte <- 240
 
40
transporte
 
41
 240
 
42
 
 
43
comestibles <- 1527
 
44
comestibles
 
45
1527
 
46
 
47
gimnasio <- 400
 
48
gimnasio
 
49
400
 
50
 
 
51
alquiler <- 1500
 
52
alquiler
 
53
1500
 
54
 
 
55
otros <- 1833
 
56
otros
 
57
1833
 
58
 
 
59
gastos <- c(celular, transporte, comestibles, gimnasio, alquiler, otros)
 
60
 barplot(gastos)
 
61
 
 
62
 
 
63
 costal <- c(87.7, 80.01, 77.28, 78.76, 81.52, 74.2, 80.71, 79.5, 77.87, 81.94,
 
64
            80.7, 82.32, 75.78, 80.19, 83.91, 79.4, 77.52, 77.62, 81.4, 74.89,
 
65
            82.95, 73.59, 77.92, 77.18, 79.83, 81.23, 79.28, 78.44, 79.01,
 
66
            80.47, 76.23, 78.89, 77.14, 69.94, 78.54, 79.7, 82.45, 77.29,
 
67
            75.52, 77.21, 75.99, 81.94, 80.41, 77.7)
 
68
 
69
#Explique con sus propias palabras que es H0 y H1
 
70
# H0: es él valor total de peso en los costales
 
71
# H1: es él valor dél peso que permite determinar si la media es menor al peso de costales
 
72
 
73
#¿Por qué corresponde utilizar una prueba de una cola?
 
74
# porque se quiere determinar si es menor que 80kg
 
75
 
76
#Calcule la media y desviación estándar de los 44 costales.
 
77
mean(87.7, 80.01, 77.28, 78.76, 81.52, 74.2, 80.71, 79.5, 77.87, 81.94,
 
78
     80.7, 82.32, 75.78, 80.19, 83.91, 79.4, 77.52, 77.62, 81.4, 74.89,
 
79
     82.95, 73.59, 77.92, 77.18, 79.83, 81.23, 79.28, 78.44, 79.01,
 
80
     80.47, 76.23, 78.89, 77.14, 69.94, 78.54, 79.7, 82.45, 77.29,
 
81
     75.52, 77.21, 75.99, 81.94, 80.41, 77.7)
 
82
# media: 87.7
 
83
 
84
sd(costal)
 
85
# desviacion estandar: 3.056023
 
86
 
87
t.test(costal, mu = 80, alternative = "less")
 
88
 
89
# estadístico t: -2.3644
 
90
#grados de libertad: 43
 
91
#valor de p: 0.01132
 
92
#media observada:78.91068
 
93
 
94
# Datos sin valor extremo
 
95
x1 <- c(1.2, 1.5, 1.7, 1.8)
 
96
 
97
# resumen general
 
98
summary(x1)
 
99
# media
 
100
mean(x1)
 
101
#mediana
 
102
median(x1)
 
103
# varianza
 
104
var(x1)
 
105
#desviacion estandar
 
106
sd(x1)
 
107
 
108
#datos con valor extremo
 
109
x2 <- c(1.2, 1.5, 1.7, 1.8, 4.8)
 
110
 
111
#resumen general
 
112
summary(x2)
 
113
#media
 
114
mean(x2)
 
115
#mediana
 
116
median(x2)
 
117
#varianza
 
118
var(x2)
 
119
#desviaciob estandar
 
120
sd(x2)
 
121
 
122
 
123
anillos <- data.frame(
 
124
  Arbol = 1:40,
 
125
  RW = c(1.42, 1.58, 1.71, 1.63, 1.85, 1.94, 2.06, 1.76, 1.68, 1.89, 2.14, 1.53, 1.79, 1.97, 2.23, 1.61, 1.82, 2.08, 1.73, 1.87, 1.91, 2.04, 1.76, 1.83, 2.12, 1.69, 1.57, 1.88, 2.21, 1.95, 1.74, 1.81, 2.09, 1.66, 1.92, 2.17, 1.72, 1.86, 2.02, 3.48)
 
126
)
 
127
 
128
# ==============================================================
 
129
# Evaluación III - Principios de estadística
 
130
# Anillos de crecimiento y temperatura
 
131
#
 
132
# 
 
133
# ==============================================================
 
134
 
135
 
136
# ==============================================================
 
137
# EJERCICIO 1
 
138
# ==============================================================
 
139
 
140
# --- a) Ingresar los datos en el objeto "anillos" ---------------
 
141
#        Variables: Temperatura (AFres, ACal) y Ancho (mm)
 
142
Afres <- c(2.45,1.98, 3.12, 2.67, 1.85, 2.98, 3.35, 2.20, 2.76, 2.58)
 
143
Acal <- c(2.51, 1.90, 3.20, 2.55, 1.92, 2.84, 3.41, 2.12, 2.70, 2.66)
 
144
 
145
 
146
 
147
# --- b) Hipótesis de partida ------------------------------------
 
148
# H0:las variables son iguales
 
149
# H1:las variables no son iguales
 
150
 
151
 
152
# --- c) Media de AFres y ACal -----------------------------------
 
153
mean(Afres)
 
154
mean(Acal)
 
155
 
156
 
157
# --- d) Desviación estándar de AFres y ACal ---------------------
 
158
sd(Afres)
 
159
sd(Acal)
 
160
 
161
 
162
# --- e) Diferencia media entre ambos años -----------------------
 
163
mean(Afres - Acal)
 
164
 
165
 
166
# --- f) Valor de t ----------------------------------------------
 
167
t.test(Afres, Acal, paired = TRUE, 
 
168
       alternative = "two.sided",
 
169
       conf.level = 0.95)
 
170
 
171
 
172
# --- g) ¿Diferencias significativas? / Parámetro / Valor --------
 
173
#No hay diferencias, el valor del parametro es 0.850
 
174
 
175
 
176
# --- h) Conclusión sobre la decisión del responsable ------------
 
177
# Respuesta:
 
178
#No esta protegida
 
179
 
180
 
181
# ==============================================================
 
182
# EJERCICIO 2
 
183
# ==============================================================
 
184
 
185
# --- Ingresar los datos -----------------------------------------
 
186
Afres2 <- c(2.45, 1.98, 3.12, 2.67, 1.85, 2.98, 3.35, 2.20, 2.76, 2.58)
 
187
Acal2 <- c(2.10, 1.85, 2.64, 2.48, 1.70, 2.41, 2.95, 2.15, 2.30, 2.35)
 
188
 
189
 
190
 
191
# --- Hipótesis --------------------------------------------------
 
192
# H0:
 
193
# H1:
 
194
 
195
 
196
# --- a) Media de AFres y ACal -----------------------------------
 
197
mean(Afres2)
 
198
mean(Acal2)
 
199
 
200
 
201
# --- b) Desviación estándar de AFres y ACal ---------------------
 
202
sd(Afres2)
 
203
sd(Acal2)
 
204
 
205
 
206
# --- c) Diferencia media entre ambos años -----------------------
 
207
mean(Afres2 - Acal2)
 
208
 
209
 
210
# --- d) Valor de t ----------------------------------------------
 
211
t.test(Afres2, Acal2, paired = TRUE, 
 
212
       alternative = "two.sided",
 
213
       conf.level = 0.95)
 
214
 
215
 
216
# --- e) ¿Diferencias significativas? / Parámetro / Valor --------
 
217
#si hay diferencias, el valor es p < 0.05
 
218
 
219
# --- f) Decisión de la Comisión Forestal ------------------------
 
220
# Respuesta: Lo mas recomendable es checar el sitio y tomar medidas de prevencion
 
221
 
222
 
223
# Analisis de varianza ANOVA
 
224
 
225
pinos <- read.csv("Datos_Rascon_Anova.csv")
 
226

Unstage line	
 
 
227
boxplot(pinos$DAP ~ pinos$Paraje,
 
228
        col = "blue",
 
229
        xlab = "DAP (cm)",
 
230
        main = "Analisis de varianza")
 
231
 
232
tapply(pinos$DAP, pinos$Paraje, mean)
 
233
tapply(pinos$DAP, pinos$Paraje, var)
 
234
 
235
 
236
#Funcion aov
 
237
pin.aov <- aov(pinos$DAP ~ pinos$Paraje)
 
238
summary(pin.aov)
 
239
 
240
#la funcion de TukeyHSD() me ayuda a encontrar las diferencias
 
241
TukeyHSD(pin.aov)
 
242
 
243
plot(TukeyHSD(pin.aov))
