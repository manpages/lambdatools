defmodule Futurex.Mixfile do
  use Mix.Project

  def project do
    [ app: :lambdatools,
      version: "0.0.1",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
      [applications: List.foldl(deps!, [], function do  
        {{app, _source}, :req}, acc0 -> [app|acc0]
        _, acc0 -> acc0
      end),
      mod: {Future.App, []}]
  end

  # Returns the list of dependencies in the format:
  # { :foobar, "0.1", git: "https://github.com/elixir-lang/foobar.git" }
  defp deps do
    :proplists.get_keys(deps!)
  end

  defp deps! do
    [
      {{:genx, github: "yrashk/genx"}, :req},
      {{:xup, github: "yrashk/xup"}, :req},
    ]
  end
end
