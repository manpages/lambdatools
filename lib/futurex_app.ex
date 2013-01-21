import Xup

defsupervisor Future.Sup, strategy: :rest_for_one do
  worker do: [id: Future.Mon]

  supervisor SOFO, strategy: :simple_one_for_one do
    worker do: [id: Future.Srv]
  end
end

defmodule Future.Mon do
  use GenServer.Behaviour
  import GenX.GenServer

  defcast register(f, pid), export: __MODULE__, state: state do 
    IO.puts "#{__MODULE__}: registering #{inspect f} on #{inspect pid}"
    :erlang.put({:future, f}, pid)
    {:noreply, state}
  end

  defcast unregister(f), export: __MODULE__, state: state do 
    IO.puts "#{__MODULE__}: unregistering #{inspect f}"
    :erlang.erase({:future, f})
    {:noreply, state}
  end

  defcall where_is(f), export: __MODULE__, state: state do 
    {:reply, :erlang.get({:future, f}), state}
  end

  defcall get_state, export: __MODULE__, state: state do
    {:reply. state, state}
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

defmodule Future.App do
  use Application.Behaviour

  def start(_, _) do
    Future.Sup.start_link()
  end 
end

defmodule MockingBird do
  use GenServer.Behaviour
  import GenX.GenServer

  defrecord State, id: nil

  defcall get_state, from: _from, state: state, do: {:reply, state, state}

  def start_link do
    :gen_server.start_link __MODULE__, nil, []
  end

  def start_link(rid) do
    :gen_server.start_link __MODULE__, rid, []
  end

  def init(rid), do: (
    {:ok, State.new(id: rid)}
  )

end
