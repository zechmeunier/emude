using UniversalDiffEq
using ComponentArrays
using Lux
using Random 
using DataFrames
using DiffEqFlux

data = DataFrame(
    t = [0.0, 1.0, 2.0],
    u1 = [1.0, 2.0, 3.0],
    u2 = [4.0, 5.0, 6.0]
)

function build_custom_derivs_function_R(f_julia,p_julia,inputs,hidden_units,outputs)
  #inputs, hidden_units, outputs = [inputs,hidden_units,outputs]
  NN = Lux.Chain(Lux.Dense(length(inputs),hidden_units,tanh),Lux.Dense(hidden_units,outputs))
  rng = Random.default_rng()
  params, states = Lux.setup(rng,NN)
  params = params |> ComponentArray
  init_params = ComponentArray(rparams = p_julia, NN = params)

  function derivs(du, u, p, t)
      nn = [0.0]
      if length(inputs) == 1
           nn = NN(u[round.(Int, inputs)],p.NN,states)[1]
      else
          nn = NN(u[round.(Int, inputs)],p.NN,states)[1]
      end
      du = f_julia(u,nn,p.rparams,t)
  end

  return derivs, init_params
end

f_julia = (u, nn, p, t) -> u[1] + nn[1] + p[1] * t
p_julia = [1.0]
inputs = [1]

deriv, parameters = build_custom_derivs_function_R(f_julia,p_julia,inputs,10,1)


model = CustomDerivatives(data, deriv, parameters)

gradient_descent!(model, verbose=true)

predict(model, data)