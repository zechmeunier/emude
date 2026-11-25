using UniversalDiffEq
using ComponentArrays
using Lux
using Random
using DiffEqFlux
using DataFrames

function build_custom_derivs_function_R(f_julia, p_julia, inputs, hidden_units, outputs)
    # Robustly convert inputs into a 1D Int Vector regardless of scalar vs vector R input
    inputs = round.(Int, vec([inputs...]))
    hidden_units, outputs = Integer.([hidden_units, outputs])
    
    NN = Lux.Chain(Lux.Dense(length(inputs), hidden_units, tanh), Lux.Dense(hidden_units, outputs))
    rng = Random.default_rng()
    params, states = Lux.setup(rng, NN)
    params = params |> ComponentArray
    
    if length(p_julia) == 1
        p_julia = [p_julia]
    end
    init_params = ComponentArray(rparams = p_julia, NN = params)

    # Method 1: No covariates
    function derivs(u, p, t)
        nn = NN(u[inputs], p.NN, states)[1]
        du = f_julia(u, nn, p.rparams, t)
    end

    # Method 2: With covariates (X)
    function derivs(u, X, p, t)
        nn = NN(u[inputs], p.NN, states)[1]
        du = f_julia(u, X, nn, p.rparams, t)
    end

    return derivs, init_params
end

function build_multi_custom_derivs_function_R(f_julia, p_julia, inputs, hidden_units, outputs)
    # Robustly convert inputs into a 1D Int Vector regardless of scalar vs vector R input
    inputs = round.(Int, vec([inputs...]))
    hidden_units, outputs = Integer.([hidden_units, outputs])
    
    NN = Lux.Chain(Lux.Dense(length(inputs), hidden_units, tanh), Lux.Dense(hidden_units, outputs))
    rng = Random.default_rng()
    params, states = Lux.setup(rng, NN)
    params = params |> ComponentArray
    
    if length(p_julia) == 1
        p_julia = [p_julia]
    end
    init_params = ComponentArray(rparams = p_julia, NN = params)

    # Method 1: No covariates
    function derivs(u, i, p, t)
        i_idx = Int(round(i))
        nn = NN(u[inputs], p.NN, states)[1]
        du = f_julia(u, i_idx, nn, p.rparams, t)
    end

    # Method 2: With covariates (X)
    function derivs(u, i, X, p, t)
        i_idx = Int(round(i))
        nn = NN(u[inputs], p.NN, states)[1]
        du = f_julia(u, i_idx, X, nn, p.rparams, t)
    end

    return derivs, init_params
end

function build_custom_ode(f_julia,p_julia)
  init_params = NamedTuple(p_julia)
  function derivs(u, p, t)
      f_julia(u,p,t)
  end
  return derivs, init_params
end

function retrieve_model_parameters(model)
  param_names = labels(model.parameters)
  parameters = model.parameters
  param_tuple = NamedTuple{Tuple(Symbol.(param_names))}(Tuple(parameters))
  return param_tuple
end

function build_custom_diffs_function_R(f_julia, p_julia, inputs, hidden_units, outputs)
    # Robustly convert inputs into a 1D Int Vector regardless of scalar vs vector R input
    inputs = round.(Int, vec([inputs...]))
    hidden_units, outputs = Integer.([hidden_units, outputs])

    NN = Lux.Chain(Lux.Dense(length(inputs), hidden_units, tanh), Lux.Dense(hidden_units, outputs))
    rng = Random.default_rng()
    params, states = Lux.setup(rng, NN)
    params = params |> ComponentArray

    if length(p_julia) == 1
        p_julia = [p_julia]
    end
    init_params = ComponentArray(rparams = p_julia, NN = params)

    # Method 1: No covariates
    function step(u, p, t)
        nn = NN(u[inputs], p.NN, states)[1]
        return f_julia(u, nn, p.rparams, t)
    end

    # Method 2: With covariates (X)
    function step(u, X, p, t)
        nn = NN(u[inputs], p.NN, states)[1]
        return f_julia(u, X, nn, p.rparams, t)
    end

    return step, init_params
end

function build_multi_custom_diffs_function_R(f_julia, p_julia, inputs, hidden_units, outputs)
    # Robustly convert inputs into a 1D Int Vector regardless of scalar vs vector R input
    inputs = round.(Int, vec([inputs...]))
    hidden_units, outputs = Integer.([hidden_units, outputs])

    NN = Lux.Chain(Lux.Dense(length(inputs), hidden_units, tanh), Lux.Dense(hidden_units, outputs))
    rng = Random.default_rng()
    params, states = Lux.setup(rng, NN)
    params = params |> ComponentArray

    if length(p_julia) == 1
        p_julia = [p_julia]
    end
    init_params = ComponentArray(rparams = p_julia, NN = params)

    # Method 1: No covariates
    function step(u, i, p, t)
        i_idx = Int(round(i))
        nn = NN(u[inputs], p.NN, states)[1]
        return f_julia(u, i_idx, nn, p.rparams, t)
    end

    # Method 2: With covariates (X)
    function step(u, i, X, p, t)
        i_idx = Int(round(i))
        nn = NN(u[inputs], p.NN, states)[1]
        return f_julia(u, i_idx, X, nn, p.rparams, t)
    end

    return step, init_params
end