import csv
import matplotlib.pyplot as plt
import numpy as np

with open('sqrt_table_deg4.csv') as csv_file:
    row_num = 0
    csv_reader = csv.reader(csv_file, delimiter=',')
    for row in csv_reader:

        for index in range(0,4):

            # print(int(row[1],2), int(row[2],2))
            fin  = open('sqrt_general_deg4_ub.tptp','rt') if index<=2 else open('sqrt_general_deg4_lb.tptp','rt')
            fout = open('sqrt_4/sqrt_'+str(row_num)+'_'+str(index)+'.tptp','wt')
            # print(row)
            Y  = float(int(row[1],2))/2**8

            # All coefficients are unsigned numbers of varying bit width
            coeff = row[0]
            # Bit 0 of the string corresponds to the MSB of the coeff table

            A0_flt = float(int(coeff[0:58]    ,2))/2**57 # 58 bits
            A1_flt = float(int(coeff[58:105]  ,2))/2**48 # 47 bits
            A2_flt = float(int(coeff[105:141] ,2) - 2**36)/2**39 # 36 bits
            A3_flt = float(int(coeff[141:167] ,2))/2**30  # 26 bits
            A4_flt = float(int(coeff[167:184] ,2) - 2**17)/2**21 # 17 bits
            print(row_num, A0_flt, A1_flt, A2_flt, A3_flt, A4_flt)

            A0 = "("+str(int(coeff[0:58]    ,2))+"/2^57)" # 58 bits
            A1 = "("+str(int(coeff[58:105]  ,2))+"/2^48)" # 47 bits
            A2 = "("+str(int(coeff[105:141] ,2) - 2**36)+"/2^39)" # 36 bits
            A3 = "("+str(int(coeff[141:167] ,2))+"/2^30)" # 26 bits
            A4 = "("+str(int(coeff[167:184] ,2) - 2**17)+"/2^21)" # 17 bits

            U = "(=0, 2^(-9)  - 2^(-27))" if index%2==0 else "(=-2^(-9), 0)"
            E4 = "2^(-29) - 2^(-48)" if (index==0 or index==3) else "0"
            E3 = "2^(-38) - 2^(-65)" if (index==0 or index==1) else "0"
            E2 = "2^(-47) - 2^(-84)" if (index==0 or index==3) else "0"
            E1 = "2^(-57) - 2^(-100)" if (index==0 or index==1) else "0"
            E0 = "2^(-52) - 2^(-57)" if (index==0 or index==1) else "0"

            e0 = "2^(-46) - 2^(-53)" if (index==1 or index==2) else "0"


            for line in fin:
                # Fill in the polynomial coefficients
                line = line.replace("__Y",str(Y))
                line = line.replace("__A4",str(A4))
                line = line.replace("__A3",str(A3))
                line = line.replace("__A2",str(A2))
                line = line.replace("__A1",str(A1))
                line = line.replace("__A0",str(A0))

                line = line.replace("__U",str(U))
                line = line.replace("__E4",str(E4))
                line = line.replace("__E3",str(E3))
                line = line.replace("__E2",str(E2))
                line = line.replace("__E1",str(E1))
                line = line.replace("__E0",str(E0))

                line = line.replace("__e0",str(e0))

                # Fill in the bounds on X
                fout.write(line)

            fout.close()
            fin.close()
        row_num = row_num + 1


X = np.linspace(-2**(-9), 2**(-9)-2**(-16),2**(10))
Z = np.linspace(0, 2**(-16)-2**(-23),2**(10))
Y = X+Z
sqrt_x = np.sqrt(1+X+2**(-9))
#poly_x = (-399.0/2**12)*X**2 + (107866.0/2**18)*X + (40773297.0/2**25)
poly_x = 1.0009760861279355 + 0.4995124328434457*X -0.12463468110036047*(X**2) + 0.06219599395990372*(X**3) - 0.038796424865722656*(X**4)
poly_2 = ((((-81362/2**21)*X + (66782440/2**30))*(X) + (-68518640547/2**39))*(X) + (140600250401292/2**48))*(X) + (144255856911761536/2**57) 

plt.plot(X, sqrt_x-poly_2,label="sqrt-poly")
# plt.plot(X, poly_x,label="poly")
plt.legend()
plt.show()


