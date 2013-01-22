defmodule Future.Srv do
  @moduledoc """
  Main part of future implementation.
  Documentation is promised.
  """
  use GenServer.Behaviour
  import GenX.GenServer

  defrecord Ref, 
    pid: nil,
    status: :running,
    value: :undefined,
    report_to: :ordsets.new do record_type(
        pid: nil | pid, 
        status: :running | :value | :error | :exception, 
        value: any,
        report_to: [pid | atom | {atom, pid}]
      )
    end

  @spec start_link((() -> any)) :: {:ok, pid}
  def start_link(f) do 
    :gen_server.start_link(__MODULE__, f, [])
  end

  @spec init((() -> any)) :: {:ok, pid}
  def init(f) do 
    IO.puts "Starting #{inspect :erlang.self}"
    new(:erlang.self, f)
    {:ok, Ref.new.pid(:erlang.self)}
  end

  def terminate(_shutdown, _state), do: :ok

  defcast new(f), state: state do
    self = :erlang.self
    :erlang.spawn_link(fn() ->
      y = f.()
      store(self, y)
      broadcast(self)
    end)
    {:noreply, state.status(:running)}
  end

  defcast subscribe(pid), state: state do
    {:noreply, state.report_to(
      :ordsets.add_element(pid, state.report_to)
    )}
  end

  defcall store(value), state: state do
    {:reply, :ok, state.value(value).status(:value)}
  end

  defcast broadcast, state: state do
    if (state.report_to != :ordsets.new) do
      Enum.each(state.report_to, (&1 <- {{__MODULE__, :erlang.self}, state}))
    end
    {:noreply, state}
  end

  defcast halt, state: state do
    IO.puts "Halting #{inspect :erlang.self}"
    :supervisor.terminate_child(Future.Sup.SOFO, :erlang.self)
    {:noreply, state}
  end

  defcall peek, state: state, do: {:reply, state, state}
end
