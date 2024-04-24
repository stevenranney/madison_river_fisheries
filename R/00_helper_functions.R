

# Assign "trout" length categories
assign_bnt_psd <- function(data){
  
  ifelse((data>=150)&(data<230), "S-Q",
         ifelse((data>=230)&(data<300), "Q-P",
                ifelse((data>=300)&(data<380), "P-M",
                       ifelse((data>=380)&(data<460), "M-T",
                              ifelse(data>=460, ">T", "SS")))))
}

assign_rbt_psd <- function(data){
  
  ifelse((data>=250)&(data<400), "S-Q",
         ifelse((data>=400)&(data<500), "Q-P",
                ifelse((data>=500)&(data<650), "P-M",
                       ifelse((data>=650)&(data<800), "M-T",
                              ifelse(data>=800, ">T", "SS")))))
}

round_down <- function(x,to=10)
{
  to*(x %/% to)
}

# Calculate Ws
# a + b*log10(L)

calculate_bnt_ws <- function(data){
  
  -4.867+(2.960*data) # Lotic, milewski and brown 1994
  
}

calculate_rbt_ws <- function(data){
  
  -5.023+(3.024*data) #Lotic, simpkins and hubert 1996
  
}