defmodule Future do
  @moduledoc """
  Simple library to work with Futures. Provides means to
  create, peek at and get values from futures.
  """

  alias(Future.Sup.SOFO, as: Sup)
  alias(Future.Mon, as: Mon)
  alias(Future.Srv, as: F)

  @spec new((() -> any)) :: pid
  @doc "Creates a new future and returns its pid."
  def new(f) when is_function(f) do 
    {:ok, pid} = :supervisor.start_child(Sup, [f])
    Mon.add_consumer(f, pid)
    {:future, f}
  end
  def new(badarg), do: :erlang.error("Future.new/1 expected (() -> any), got #{inspect badarg}")

  @spec get((() -> any), integer) :: any
  @doc "Hangs execution to get the future value."
  def get(f, timeout // 20) when is_function(f) and is_integer(timeout) do
    state = Mon.state_of(f)
    if (state.status == :running) do
      get_receive(f, timeout, state)
    else
      get_return(f, state)
    end
  end

  @spec get_return((() -> any), F.Ref.t) :: any
  defp get_return(f, state) do
    Mon.del_consumer(f)
    state.value
  end

  @spec get_receive((() -> any), integer, F.Ref.t) :: any
  defp get_receive(f, timeout, state) do
    Mon.subscribe(f, :erlang.self)
    receive do
      {{F, Mon.where_is(f)}, state} -> (
        Mon.del_consumer(f)
        state.value
      )
      _ -> get(f, timeout)
    after
      timeout -> :timeout
    end
  end
end
