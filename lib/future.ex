defmodule Future do
  @moduledoc """
  Simple library to work with Futures. Provides means to
  create, peek at and get values from futures.
  """

  @timeout 2000

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
  def get(f, timeout // @timeout) when is_function(f) and is_integer(timeout) do
    state = Mon.state_of(f)
    if (state != :undefined) do
      if (state.status == :running) do
        get_receive_maybe(f, timeout, state)
      else
        get_return_maybe(f, state)
      end
    else
      :noproc
    end
  end

  @spec get_return_maybe((() -> any), F.Ref.t | :undefined) :: any
  defp get_return_maybe(_, :undefined), do: :undefined
  defp get_return_maybe(f, state) do
    Mon.del_consumer(f)
    state.value
  end

  @spec get_receive_maybe((() -> any), integer, F.Ref.t | :undefined) :: any
  defp get_receive_maybe(_, _, :undefined), do: :undefined
  defp get_receive_maybe(f, timeout, state) do
    Mon.subscribe(f, :erlang.self)
    fpid = Mon.where_is(f)
    receive do
      {{F, fpid}, state} -> (
        Mon.del_consumer(f)
        state.value
      )
      _ -> get(f, timeout)
    after
      timeout -> :timeout
    end
  end
end
