defmodule ExLambda do
  defmodule HigherOrder do
    def y(f) do
      g = fn(g) -> f.((g.(g)).(&1)) end
      g.(g)
    end
  end

  def start do
    :ok = Application.start(:lambdatools)
  end
end
