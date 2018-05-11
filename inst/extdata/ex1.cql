
 Title ex1;

  data ex1.txt;

 format pid 11-15 responses 1-9;

 codes 1,2,3,4,5,9;

 label << ex1.lab;

 key 252552444 ! 1;

 model item;

 estimate ! nodes = 30;
Original Sufficient Statistics
Parameter No.              Sufficient Statistic
      1                       -60.00000
      2                      -392.00000
      3                      -333.00000
      4                      -145.00000
      5                        26.00000
      6                       -31.00000
      7                       -89.00000
      8                      -148.00000
Modified Sufficient Statistics
Parameter No.              Sufficient Statistic
      1                       -60.00000
      2                      -392.00000
      3                      -333.00000
      4                      -145.00000
      5                        26.00000
      6                       -31.00000
      7                       -89.00000
      8                      -148.00000
.......................................................
Iteration =    1
   Deviance=     12908.114 
   Covariance 
      1.43209
   Correlation 
      1.00000
   Regression Coefficients 
     -0.17735
   Item Parameters 
         1  -0.14279
         2  -0.89421
         3  -0.34980
         4   0.30768
         5   0.60631
         6   0.16068
         7  -0.05939
         8  -0.17471
Maximum change in item parameters was   -0.89421 for parameter   2 
Maximum change in regression parameters was   -0.17735
Maximum change in covariance parameters was    0.43209
.......................................................
Iteration =    2
   Deviance=     12124.359 
   Covariance 
      1.94636
   Correlation 
      1.00000
   Regression Coefficients 
     -0.26652
   Item Parameters 
         1   0.04720
         2  -1.27410
         3  -0.68002
         4   0.28496
         5   0.92187
         6   0.38454
         7   0.01467
         8  -0.22991
Maximum change in item parameters was   -0.37988 for parameter   2 
Maximum change in regression parameters was   -0.08917
Maximum change in covariance parameters was    0.51427
Change in the deviance was    783.755
.......................................................
Iteration =    3
   Deviance=     11846.666 
   Covariance 
      2.49066
   Correlation 
      1.00000
   Regression Coefficients 
     -0.31252
   Item Parameters 
         1   0.12584
         2  -1.46609
         3  -0.91769
         4   0.17291
         5   1.03625
         6   0.54391
         7   0.12310
         8  -0.20898
Maximum change in item parameters was   -0.23767 for parameter   3 
Maximum change in regression parameters was   -0.04600
Maximum change in covariance parameters was    0.54430
Change in the deviance was    277.692
.......................................................
Iteration =    4
   Deviance=     11725.765 
   Covariance 
      2.99413
   Correlation 
      1.00000
   Regression Coefficients 
     -0.33966
   Item Parameters 
         1   0.18904
         2  -1.56041
         3  -1.07146
         4   0.05853
         5   1.04336
         6   0.62183
         7   0.20976
         8  -0.15989
Maximum change in item parameters was   -0.15377 for parameter   3 
Maximum change in regression parameters was   -0.02714
Maximum change in covariance parameters was    0.50346
Change in the deviance was    120.901
.......................................................
Iteration =    5
   Deviance=     11669.965 
   Covariance 
      3.40248
   Correlation 
      1.00000
   Regression Coefficients 
     -0.35727
   Item Parameters 
         1   0.25585
         2  -1.59074
         3  -1.15776
         4  -0.02943
         5   1.00587
         6   0.64000
         7   0.25811
         8  -0.11449
Maximum change in item parameters was   -0.08796 for parameter   4 
Maximum change in regression parameters was   -0.01761
Maximum change in covariance parameters was    0.40835
Change in the deviance was     55.800
.......................................................
Iteration =    6
   Deviance=     11645.205 
   Covariance 
      3.69498
   Correlation 
      1.00000
   Regression Coefficients 
     -0.36873
   Item Parameters 
         1   0.31848
         2  -1.58121
         3  -1.19399
         4  -0.08510
         5   0.95960
         6   0.62576
         7   0.27329
         8  -0.08619
Maximum change in item parameters was    0.06263 for parameter   1 
Maximum change in regression parameters was   -0.01146
Maximum change in covariance parameters was    0.29250
Change in the deviance was     24.761
.......................................................
Iteration =    7
   Deviance=     11634.548 
   Covariance 
      3.87957
   Correlation 
      1.00000
   Regression Coefficients 
     -0.37568
   Item Parameters 
         1   0.36691
         2  -1.55291
         3  -1.19763
         4  -0.11254
         5   0.92174
         6   0.60037
         7   0.26781
         8  -0.07567
Maximum change in item parameters was    0.04843 for parameter   1 
Maximum change in regression parameters was   -0.00695
Maximum change in covariance parameters was    0.18459
Change in the deviance was     10.657
.......................................................
Iteration =    8
   Deviance=     11629.990 
   Covariance 
      3.98020
   Correlation 
      1.00000
   Regression Coefficients 
     -0.37939
   Item Parameters 
         1   0.39696
         2  -1.52172
         3  -1.18431
         4  -0.11976
         5   0.89784
         6   0.57658
         7   0.25358
         8  -0.07761
Maximum change in item parameters was    0.03120 for parameter   2 
Maximum change in regression parameters was   -0.00371
Maximum change in covariance parameters was    0.10063
Change in the deviance was      4.558
.......................................................
Iteration =    9
   Deviance=     11628.047 
   Covariance 
      4.02484
   Correlation 
      1.00000
   Regression Coefficients 
     -0.38100
   Item Parameters 
         1   0.41032
         2  -1.49680
         3  -1.16589
         4  -0.11531
         5   0.88696
         6   0.55995
         7   0.23881
         8  -0.08564
Maximum change in item parameters was    0.02491 for parameter   2 
Maximum change in regression parameters was   -0.00161
Maximum change in covariance parameters was    0.04464
Change in the deviance was      1.943
.......................................................
Iteration =   10
   Deviance=     11627.238 
   Covariance 
      4.03772
   Correlation 
      1.00000
   Regression Coefficients 
     -0.38144
   Item Parameters 
         1   0.41189
         2  -1.48136
         3  -1.14960
         4  -0.10624
         5   0.88533
         6   0.55127
         7   0.22771
         8  -0.09476
Maximum change in item parameters was    0.01629 for parameter   3 
Maximum change in regression parameters was   -0.00044
Maximum change in covariance parameters was    0.01288
Change in the deviance was      0.809
.......................................................
Iteration =   11
   Deviance=     11626.913 
   Covariance 
      4.03613
   Correlation 
      1.00000
   Regression Coefficients 
     -0.38136
   Item Parameters 
         1   0.40713
         2  -1.47461
         3  -1.13855
         4  -0.09720
         5   0.88869
         6   0.54882
         7   0.22137
         8  -0.10209
Maximum change in item parameters was    0.01105 for parameter   3 
Maximum change in regression parameters was    0.00008
Maximum change in covariance parameters was   -0.00159
Change in the deviance was      0.326
.......................................................
Iteration =   12
   Deviance=     11626.780 
   Covariance 
      4.03045
   Correlation 
      1.00000
   Regression Coefficients 
     -0.38114
   Item Parameters 
         1   0.40039
         2  -1.47394
         3  -1.13295
         4  -0.09052
         5   0.89366
         6   0.55012
         7   0.21907
         8  -0.10663
Maximum change in item parameters was   -0.00674 for parameter   1 
Maximum change in regression parameters was    0.00022
Maximum change in covariance parameters was   -0.00568
Change in the deviance was      0.133
.......................................................
Iteration =   13
   Deviance=     11626.719 
   Covariance 
      4.02572
   Correlation 
      1.00000
   Regression Coefficients 
     -0.38096
   Item Parameters 
         1   0.39438
         2  -1.47648
         3  -1.13150
         4  -0.08674
         5   0.89813
         6   0.55294
         7   0.21940
         8  -0.10859
Maximum change in item parameters was   -0.00600 for parameter   1 
Maximum change in regression parameters was    0.00018
Maximum change in covariance parameters was   -0.00473
Change in the deviance was      0.061
.......................................................
Iteration =   14
   Deviance=     11626.687 
   Covariance 
      4.02352
   Correlation 
      1.00000
   Regression Coefficients 
     -0.38089
   Item Parameters 
         1   0.39029
         2  -1.47996
         3  -1.13249
         4  -0.08538
         5   0.90118
         6   0.55579
         7   0.22097
         8  -0.10875
Maximum change in item parameters was   -0.00409 for parameter   1 
Maximum change in regression parameters was    0.00008
Maximum change in covariance parameters was   -0.00220
Change in the deviance was      0.032
.......................................................
Iteration =   15
   Deviance=     11626.669 
   Covariance 
      4.02362
   Correlation 
      1.00000
   Regression Coefficients 
     -0.38089
   Item Parameters 
         1   0.38820
         2  -1.48303
         3  -1.13444
         4  -0.08558
         5   0.90274
         6   0.55791
         7   0.22274
         8  -0.10797
Maximum change in item parameters was   -0.00306 for parameter   2 
Maximum change in regression parameters was   -0.00001
Maximum change in covariance parameters was    0.00010
Change in the deviance was      0.018
.......................................................
Iteration =   16
   Deviance=     11626.659 
   Covariance 
      4.02511
   Correlation 
      1.00000
   Regression Coefficients 
     -0.38095
   Item Parameters 
         1   0.38766
         2  -1.48509
         3  -1.13636
         4  -0.08650
         5   0.90314
         6   0.55911
         7   0.22416
         8  -0.10691
Maximum change in item parameters was   -0.00206 for parameter   2 
Maximum change in regression parameters was   -0.00006
Maximum change in covariance parameters was    0.00149
Change in the deviance was      0.010
.......................................................
Iteration =   17
   Deviance=     11626.654 
   Covariance 
      4.02706
   Correlation 
      1.00000
   Regression Coefficients 
     -0.38103
   Item Parameters 
         1   0.38803
         2  -1.48612
         3  -1.13777
         4  -0.08754
         5   0.90287
         6   0.55954
         7   0.22503
         8  -0.10599
Maximum change in item parameters was   -0.00141 for parameter   3 
Maximum change in regression parameters was   -0.00007
Maximum change in covariance parameters was    0.00195
Change in the deviance was      0.005
.......................................................
Iteration =   18
   Deviance=     11626.652 
   Covariance 
      4.02882
   Correlation 
      1.00000
   Regression Coefficients 
     -0.38109
   Item Parameters 
         1   0.38876
         2  -1.48636
         3  -1.13856
         4  -0.08837
         5   0.90232
         6   0.55948
         7   0.22540
         8  -0.10538
Maximum change in item parameters was   -0.00083 for parameter   4 
Maximum change in regression parameters was   -0.00007
Maximum change in covariance parameters was    0.00176
Change in the deviance was      0.002
.......................................................
Iteration =   19
   Deviance=     11626.651 
   Covariance 
      4.03007
   Correlation 
      1.00000
   Regression Coefficients 
     -0.38114
   Item Parameters 
         1   0.38949
         2  -1.48615
         3  -1.13884
         4  -0.08889
         5   0.90179
         6   0.55919
         7   0.22543
         8  -0.10508
Maximum change in item parameters was    0.00072 for parameter   1 
Maximum change in regression parameters was   -0.00005
Maximum change in covariance parameters was    0.00126
Change in the deviance was      0.001
.......................................................
Iteration =   20
   Deviance=     11626.650 
   Covariance 
      4.03078
   Correlation 
      1.00000
   Regression Coefficients 
     -0.38117
   Item Parameters 
         1   0.39002
         2  -1.48577
         3  -1.13879
         4  -0.08911
         5   0.90139
         6   0.55885
         7   0.22528
         8  -0.10501
Maximum change in item parameters was    0.00053 for parameter   1 
Maximum change in regression parameters was   -0.00003
Maximum change in covariance parameters was    0.00071
Change in the deviance was      0.001
.......................................................
Iteration =   21
   Deviance=     11626.650 
   Covariance 
      4.03104
   Correlation 
      1.00000
   Regression Coefficients 
     -0.38117
   Item Parameters 
         1   0.39031
         2  -1.48540
         3  -1.13858
         4  -0.08912
         5   0.90117
         6   0.55859
         7   0.22508
         8  -0.10508
Maximum change in item parameters was    0.00037 for parameter   2 
Maximum change in regression parameters was   -0.00001
Maximum change in covariance parameters was    0.00026
Change in the deviance was      0.000
.......................................................
Iteration =   22
   Deviance=     11626.650 
   Covariance 
      4.03103
   Correlation 
      1.00000
   Regression Coefficients 
     -0.38117
   Item Parameters 
         1   0.39042
         2  -1.48514
         3  -1.13836
         4  -0.08903
         5   0.90110
         6   0.55842
         7   0.22490
         8  -0.10519
Maximum change in item parameters was    0.00027 for parameter   2 
Maximum change in regression parameters was    0.00000
Maximum change in covariance parameters was   -0.00002
Change in the deviance was      0.000
.......................................................
Iteration =   23
   Deviance=     11626.650 
   Covariance 
      4.03088
   Correlation 
      1.00000
   Regression Coefficients 
     -0.38117
   Item Parameters 
         1   0.39039
         2  -1.48499
         3  -1.13818
         4  -0.08891
         5   0.90112
         6   0.55841
         7   0.22478
         8  -0.10530
Maximum change in item parameters was    0.00018 for parameter   3 
Maximum change in regression parameters was    0.00001
Maximum change in covariance parameters was   -0.00015
Change in the deviance was      0.000

 show !estimate=latent >> ex1.shw;

 itanal >> ex1.itn;

 show cases !estimates=latent >> ex1_05.pv;

 set n_plausible=10;

 show cases !estimates=latent >> ex1_10.pv;
