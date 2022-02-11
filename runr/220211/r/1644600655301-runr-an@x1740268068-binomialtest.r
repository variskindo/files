source("KW_bin_set.R")
th0=0.05
th=0.1
th1=0.15
l0=
157.696751972207	
l1=193.349705609267	
th=0.076846178793028
r=1
# 211	128	0.100662344846268	0.099686560438144	38.618882262587
H=Hbound(l0,l1,th0,th1,th)
H


 test=modified_kw(H=0,l0,l1,th0,th1,th)
 H=length(test)+1
cc=acceptAt(H,l0,l1,th0,th1)
H
alpha=1-operating_characteristic(test,th0,th,cc)
  beta=operating_characteristic(test,th1,th,cc)
# beta=OCMod(test,l0,l1,th0,th1,th)
 asn=average_sample_number(test,th)

print(paste(" alpha",alpha,"beta",beta,"ASN",asn))
 print(paste(l0*alpha+l1*beta+asn))
Lagr(test,l0,l1,th0,th1,th)
# print(date())
# 
# a=original_kw(H=1,option=1,l0,l1,th0,th1) 
# th=a[1]
# a[2]
# test=modified_kw(H=0,l0,l1,th0,th1,th)
#  H=length(test)+1
# cc=acceptAt(H,l0,l1,th0,th1)
# H
# alpha=1-OC(test,th0,th,cc)
#   beta=OC(test,th1,th,cc)
# # beta=OCMod(test,l0,l1,th0,th1,th)
#  asn=average_sample_number(test,th)
# th
# print(paste(" alpha",alpha,"beta",beta,"ASN",asn))
#  print(paste(l0*alpha+l1*beta+asn))
# 
#  print(paste("ASN",average_sample_number(test,th)))
#  print(paste("ASN under H_0",average_sample_number(test,th0)))
#  print(paste("ASN under H_1",average_sample_number(test,th1)))
#  
#  l0=700
# l1=1100
# # H2bound(l0,l1,th0,th1,th)
# # for(i in seq(1,67))
# # print(paste(L2bound(i,l0,l1,th0,th1,th),U2bound(i,l0,l1,th0,th1,th)))
# a=best_2SPRT_kw(l0,l1,th0,th1)
# th=a[1]
# test=two_SPRT(l0,l1,th0,th1,th)
# H=length(test)+1
# cc=acceptAt(H,l0,l1,th0,th1)
# alpha=1-OC(test,th0,th,cc)
#   beta=OC(test,th1,th,cc)
# # beta=OCMod(test,l0,l1,th0,th1,th)
#  asn=average_sample_number(test,th)
# average_sample_number(test,th0)
# average_sample_number(test,th1)
# #  prob_not_to_stop_before(test,th,88)
# #  sample_number_quantile(test, th, 0.99)
# print(paste(" alpha",alpha,"beta",beta,"ASN",asn))
#  print(paste(l0*alpha+l1*beta+asn))
# th
# H
# length(test)
# 
#  x=y=z=seq(1,H)
# for(i in 1:(H-1)){
# y[i]=test[[i]]$from
# z[i]=test[[i]]$from+test[[i]]$length-1
# }
# y[H]=z[H]=cc
# plot(x,y,ylim=c(0,130),type="l")
# par(new=TRUE)
# plot(x,z,ylim=c(0,130),type="l")
# H
# # a=simKW(test,th0,cc)
# # al=1-a[1]
# # # ASN(test,th0)
# # b=simKW(test,th1,cc)
# # be=b[1]
# # # ASN(test,th1)
# # c=simKW(test,th,cc)
# # # OC(test,th)
# # N=c[2]
# # # beta=OCMod(test,l0,l1,th0,th1,th)
# # 
# # print(paste("simulated alpha",al,"beta",be,"ASN",N))
# #  print(paste("sim lagr",l0*al+l1*be+N))
