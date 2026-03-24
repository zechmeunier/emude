# test_helpers.jl
# Julia-native unit tests for inst/Julia/helpers.jl
#
# Run from the package root with:
#   julia --project=. inst/Julia/test/test_helpers.jl
#
# Or from R after emude_setup():
#   JuliaCall::julia_source(system.file("Julia/test/test_helpers.jl", package = "emude"))
#
# Requires the following Julia packages to be installed in the project environment:
#   UniversalDiffEq, ComponentArrays, Lux, Random, DiffEqFlux, DataFrames

using Test
using DataFrames

# Load helpers under test by path relative to this file
include(joinpath(@__DIR__, "..", "helpers.jl"))

# ── build_custom_derivs_function_R ───────────────────────────────────────────

@testset "build_custom_derivs_function_R" begin

    # Minimal single-state derivatives function for testing
    f_single = (u, nn, p, t) -> [p[1] .* u[1] .- nn[1]]
    p_single = [0.5]
    inputs   = [1]

    @testset "returns a two-element tuple" begin
        result = build_custom_derivs_function_R(f_single, p_single, inputs, 5, 1)
        @test length(result) == 2
    end

    @testset "first element is a Function" begin
        derivs, _ = build_custom_derivs_function_R(f_single, p_single, inputs, 5, 1)
        @test isa(derivs, Function)
    end

    @testset "second element is a ComponentArray" begin
        _, params = build_custom_derivs_function_R(f_single, p_single, inputs, 5, 1)
        @test isa(params, ComponentArray)
    end

    @testset "params ComponentArray has :rparams key" begin
        _, params = build_custom_derivs_function_R(f_single, p_single, inputs, 5, 1)
        @test haskey(params, :rparams)
    end

    @testset "params ComponentArray has :NN key" begin
        _, params = build_custom_derivs_function_R(f_single, p_single, inputs, 5, 1)
        @test haskey(params, :NN)
    end

    @testset "no-covariate derivs(u, p, t) is callable" begin
        derivs, params = build_custom_derivs_function_R(f_single, p_single, inputs, 5, 1)
        u = [0.4]
        result = derivs(u, params, 0.0)
        @test isa(result, AbstractVector)
        @test length(result) == 1
    end

    @testset "covariate derivs(u, x, p, t) is callable" begin
        derivs, params = build_custom_derivs_function_R(f_single, p_single, inputs, 5, 1)
        u = [0.4]; x = [0.1]
        result = derivs(u, x, params, 0.0)
        @test isa(result, AbstractVector)
    end

    @testset "Float64 hidden_units coerces without error" begin
        @test_nowarn build_custom_derivs_function_R(f_single, p_single, inputs, 5.0, 1.0)
    end

    @testset "multi-input NN (inputs = [1, 2]) is callable" begin
        f_two = (u, nn, p, t) -> [p[1] .* u[1] .+ p[2] .* u[2] .- nn[1]]
        p_two    = [0.5, 0.3]
        inputs2  = [1, 2]
        derivs, params = build_custom_derivs_function_R(f_two, p_two, inputs2, 5, 1)
        u = [0.4, 0.6]
        result = derivs(u, params, 0.0)
        @test isa(result, AbstractVector)
    end

    @testset "scalar p_julia wrapped in vector — params is still ComponentArray" begin
        _, params = build_custom_derivs_function_R(f_single, [1.0], inputs, 5, 1)
        @test isa(params, ComponentArray)
    end

end  # build_custom_derivs_function_R


# ── build_multi_custom_derivs_function_R ─────────────────────────────────────

@testset "build_multi_custom_derivs_function_R" begin

    f_multi = (u, i, nn, p, t) -> [p[1] .* u[1] .- nn[1]]
    p_multi = [0.5]
    inputs  = [1]

    @testset "returns a two-element tuple" begin
        result = build_multi_custom_derivs_function_R(f_multi, p_multi, inputs, 5, 1)
        @test length(result) == 2
    end

    @testset "first element is a Function" begin
        derivs, _ = build_multi_custom_derivs_function_R(f_multi, p_multi, inputs, 5, 1)
        @test isa(derivs, Function)
    end

    @testset "second element is a ComponentArray" begin
        _, params = build_multi_custom_derivs_function_R(f_multi, p_multi, inputs, 5, 1)
        @test isa(params, ComponentArray)
    end

    @testset "four-arg derivs(u, i, p, t) is callable" begin
        derivs, params = build_multi_custom_derivs_function_R(f_multi, p_multi, inputs, 5, 1)
        u = [0.4]; i = 1
        result = derivs(u, i, params, 0.0)
        @test isa(result, AbstractVector)
    end

    @testset "five-arg derivs(u, i, x, p, t) is callable" begin
        f_cov = (u, i, x, nn, p, t) -> [p[1] .* u[1] .- nn[1]]
        derivs, params = build_multi_custom_derivs_function_R(f_cov, p_multi, inputs, 5, 1)
        u = [0.4]; x = [0.1]; i = 1
        result = derivs(u, i, x, params, 0.0)
        @test isa(result, AbstractVector)
    end

    @testset "Float64 hidden_units coerces without error" begin
        @test_nowarn build_multi_custom_derivs_function_R(f_multi, p_multi, inputs, 5.0, 1.0)
    end

end  # build_multi_custom_derivs_function_R


# ── build_custom_ode ──────────────────────────────────────────────────────────

@testset "build_custom_ode" begin

    f_ode  = (u, p, t) -> [p.r .* u[1]]
    p_ode  = (r = 0.5,)
    inputs = [1]

    @testset "returns a two-element tuple" begin
        result = build_custom_ode(f_ode, p_ode, inputs, 1)
        @test length(result) == 2
    end

    @testset "first element is a Function" begin
        derivs, _ = build_custom_ode(f_ode, p_ode, inputs, 1)
        @test isa(derivs, Function)
    end

    @testset "derivs(du, u, p, t) mutates du in place" begin
        derivs, params = build_custom_ode(f_ode, p_ode, inputs, 1)
        du = [0.0]
        u  = [1.0]
        derivs(du, u, params, 0.0)
        @test du[1] != 0.0
    end

    @testset "derivs(du, u, p, t) result is numerically consistent with f_ode" begin
        derivs, params = build_custom_ode(f_ode, p_ode, inputs, 1)
        du = [0.0]
        u  = [2.0]
        derivs(du, u, params, 0.0)
        # f_ode gives r * u[1] = 0.5 * 2.0 = 1.0
        @test isapprox(du[1], 1.0, atol = 1e-8)
    end

    @testset "Float64 outputs coerces without error" begin
        @test_nowarn build_custom_ode(f_ode, p_ode, inputs, 1.0)
    end

end  # build_custom_ode


# ── retrieve_model_parameters ─────────────────────────────────────────────────
# Requires constructing a live UniversalDiffEq model. Skip if CustomDerivatives
# is unavailable (package not loaded).

@testset "retrieve_model_parameters" begin

    @testset "returns a NamedTuple from a live model" begin
        # Build a minimal single-state model to get real model.parameters
        data_test = DataFrame(
            t  = [0.0, 1.0, 2.0, 3.0, 4.0],
            u1 = [0.4, 0.5, 0.45, 0.48, 0.44]
        )
        f_julia = (u, nn, p, t) -> [p[1] .* u[1] .- nn[1]]
        p_julia = [0.5]
        inputs  = [1]
        derivs, params = build_custom_derivs_function_R(f_julia, p_julia, inputs, 5, 1)
        model = CustomDerivatives(data_test, derivs, params, time_column_name = "t")
        result = retrieve_model_parameters(model)
        @test isa(result, NamedTuple)
    end

    @testset "returned NamedTuple has at least one key" begin
        data_test = DataFrame(
            t  = [0.0, 1.0, 2.0, 3.0, 4.0],
            u1 = [0.4, 0.5, 0.45, 0.48, 0.44]
        )
        f_julia = (u, nn, p, t) -> [p[1] .* u[1] .- nn[1]]
        derivs, params = build_custom_derivs_function_R(f_julia, [0.5], [1], 5, 1)
        model = CustomDerivatives(data_test, derivs, params, time_column_name = "t")
        result = retrieve_model_parameters(model)
        @test length(keys(result)) >= 1
    end

end  # retrieve_model_parameters
