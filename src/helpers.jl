using UniversalDiffEq
using ComponentArrays
using Lux
using Random
using DiffEqFlux
using DataFrames

function build_custom_derivs_function_R(f_julia,p_julia,inputs,hidden_units,outputs)
  hidden_units, outputs = Integer.([hidden_units,outputs])
  NN = Lux.Chain(Lux.Dense(length(inputs),hidden_units,tanh),Lux.Dense(hidden_units,outputs))
  rng = Random.default_rng()
  params, states = Lux.setup(rng,NN)
  params = params |> ComponentArray
  if length(p_julia) == 1
    p_julia = [p_julia]
  end
  init_params = ComponentArray(rparams = p_julia, NN = params)

  function derivs(u, p, t)
      nn = [0.0]
      if length(inputs) == 1
           nn = NN(u[round.(Int, [inputs])],p.NN,states)[1]
      else
          nn = NN(u[round.(Int, inputs)],p.NN,states)[1]
      end
      du = f_julia(u,nn,p.rparams,t)
  end

  return derivs, init_params
end

function build_multi_custom_derivs_function_R(f_julia,p_julia,inputs,hidden_units,outputs)
  hidden_units, outputs = [hidden_units,outputs] .|> Integer
  NN = Lux.Chain(Lux.Dense(length(inputs),hidden_units,tanh),Lux.Dense(hidden_units,outputs))
  rng = Random.default_rng()
  params, states = Lux.setup(rng,NN)
  params = params |> ComponentArray
   if length(p_julia) == 1
    p_julia = [p_julia]
  end
  init_params = ComponentArray(rparams = p_julia, NN = params)

  function derivs(u, i, p, t)
      nn = [0.0]
      if length(inputs) == 1
           nn = NN(u[round.(Int, [inputs])],p.NN,states)[1]
      else
          nn = NN(u[round.(Int, inputs)],p.NN,states)[1]
      end
      f_julia(u,i,nn,p.rparams,t)
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

function retrieve_model_parameters(model)
  param_names = labels(model.parameters)
  parameters = model.parameters
  param_tuple = NamedTuple{Tuple(Symbol.(param_names))}(Tuple(parameters))
  return param_tuple
end