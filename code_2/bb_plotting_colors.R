
########################
### Plotting Helpers ###
########################

##### tibbles containing expected score as a function of (n,p) for random brackets ##### 

color_vec =  c(
  "chalky3"="firebrick2", 
  "chalky10"="firebrick", 
  "random"="dodgerblue2"
)
name_vec1 = c(
  # "chalky3"="one\nchalky\nbracket\n(u_max=3)\n", 
  # "chalky10"="one\nchalky\nbracket\n(u_max=10)\n",
  "chalky3"="one\nchalky\nbracket\n(U=3)\n", 
  "chalky10"="one\nchalky\nbracket\n(U=10)\n",
  "random"="one\nrandom\nbracket\n"
)

my_palette_1 = c(
  rev(brewer.pal(name="PuRd",n=9)[4:9]),
  brewer.pal(name="Blues",n=9)[3:9]
  # rev(brewer.pal(name="Blues",n=9)[3:9])
)

my_palette_n1 = c(
  rev(brewer.pal(name="PuRd",n=9)[4:9]),
  brewer.pal(name="Blues",n=9)[3:4]#[3:9]
)

my_palette_n2 = c(
  # rev(brewer.pal(name="Purples",n=9)[4:7]),
  # brewer.pal(name="Reds",n=9)[3:7]#[3:9]
  rev(brewer.pal(name="PuRd",n=9)[4:9]),
  brewer.pal(name="Blues",n=9)[4:6]#[3:9]
)

my_palette_nk1 = c(
  rev(brewer.pal(name="PuRd",n=9)[4:9]),
  brewer.pal(name="Blues",n=9)[4:9]
)



