# ISyE 6416 Homework 1

# Bisection problem
quant = function(x){
  0.05-0.5*pbeta((5/(x*x+5)),2.5,0.5)
}

bisection = function(f,a,b,n,tol = 1e-4){
  if ((f(a) * f(b)) > 0){
    print('Ends of interval have the same sign')
  }else {
    while (abs(a-b)>tol){
      c = (a+b)/2
      if ((f(a) * f(c)) < 0){
        b = c
      }else{
        a = c
      }
    }
  }
}

t = bisection(quant, 1.291,2.582,log2((2.582-1.291)/1e-4))
t
  

