#  worker do: [id: MockingBird]

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
