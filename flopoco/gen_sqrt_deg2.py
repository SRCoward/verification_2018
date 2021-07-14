import csv
import matplotlib.pyplot as plt
import numpy as np

with open('sqrt_table_deg2.csv') as csv_file:
    row_num = 0
    csv_reader = csv.reader(csv_file, delimiter=',')
    for row in csv_reader:
        # print(int(row[1],2), int(row[2],2))
        fin  = open('sqrt_general_deg2.tptp','rt')
        fout = open('sqrt/sqrt_'+str(row_num)+'.tptp','wt')
        # print(row)
        Y  = float(int(row[1],2))/2**6

        # All coefficients are unsigned numbers of varying bit width
        coeff = row[0]
        # Bit 0 of the string corresponds to the MSB of the coeff table
        print(row_num, float(int(coeff[0:27],2))/2**26, float(int(coeff[27:45] ,2))/2**19,float(int(coeff[45:54],2)-2**9)/2**12)

        A0 = "("+str(int(coeff[0:27],2))+"/2^26)" # 27 bits
        A1 = "("+str(int(coeff[27:45] ,2))+"/2^19)" # 18 bits
        A2 = "("+str(int(coeff[45:54]  ,2) - 2**9)+"/2^12)" # 10 bits
        for line in fin:
            # Fill in the polynomial coefficients
            line = line.replace("__Y",str(Y))
            line = line.replace("__A2",str(A2))
            line = line.replace("__A1",str(A1))
            line = line.replace("__A0",str(A0))

            # Fill in the bounds on X
            fout.write(line)

        fout.close()
        fin.close()
        row_num = row_num + 1


X = np.linspace(-2**(-7), 2**(-7)-2**(-16),2**(10))
Z = np.linspace(0, 2**(-16)-2**(-23),2**(10))
Y = X+Z
sqrt_x = np.sqrt(1+Y+0.46875+2**(-7))
poly_x = (-399.0/2**12)*X**2 + (107866.0/2**18)*X + (40773297.0/2**25)
plt.plot(X, sqrt_x-poly_x,label="sqrt-poly")
# plt.plot(X, poly_x,label="poly")
plt.legend()
plt.show()

print(int("100110101011011001101111111",2), int("110100111100110011",2), int("011011110",2),int("011101",2))