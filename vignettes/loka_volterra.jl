function derivs(u,nn,p,t)
  du1 = p.r*u[1] - nn[1]
  du2 = p.theta*nn[1] + p.m*u[2]
  return [du1,du2]
end 