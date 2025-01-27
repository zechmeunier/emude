using Lux
using Random 

function build_custom_derivs_function_R(f_julia,p_julia,inputs,outputs)

  NN = Lux.Chain(Lux.Dense(length(inputs),10,tanh),Lux.Dense(10,outputs))
  rng = Random.default_rng()
  params, states = Lux.setup(rng,NN)
  init_params = merge(NamedTuple(p_julia), (NN = params, ))

  function derivs(du, u, p, t)
      nn = [0.0]
      if length(inputs) == 1
           nn = NN([u[round.(Int, inputs)]],p.NN,states)[1]
      else
          nn = NN(u[round.(Int, inputs)],p.NN,states)[1]
      end
      du .= f_julia(u,nn,p,t)
  end
  
  return derivs, init_params
end


function build_custom_ode(f_julia,p_julia,inputs,outputs)
  init_params = NamedTuple(p_julia),
  function derivs(du, u, p, t)
      du .= f_julia(u,p,t)
  end
  return derivs, init_params
end