kalkulator = function(a,b,operacja){
  if(operacja == "+")
  {
    return(a+b)
  }
  if(operacja == "-")
  {
    return(a-b)
  }
  if(operacja == "*")
  {
    return(a*b)
  }
  if(operacja == "/")
  {
    return(a/b)
  }
  
}

print(kalkulator(1,1,"+"))