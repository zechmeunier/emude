function derivs(u, du, p, t) 
    x,y = u
    du .= NN([x,y], p , NNstates)[1]
 end
