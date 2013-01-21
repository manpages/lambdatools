defmodule Future.Mon do
  use GenServer.Behaviour
  import GenX.GenServer

  defcast register(f, pid), export: __MODULE__, state: state do 
    :erlang.put({:future, f}, {pid, 1})
    {:noreply, state}
  end

  defcast unregister(f), export: __MODULE__, state: state do 
    :erlang.erase({:future, f})
    {:noreply, state}
  end

  defcall where_is(f), export: __MODULE__, state: state do 
    {:reply, :erlang.get({:future, f}), state}
  end

  def start_link do
    IO.puts "#{__MODULE__}: Starting monitor"
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, nil, [])
  end

  def init(_) do 
    IO.puts "#{__MODULE__}: Initializing"
    {:ok, []}
  end
end
