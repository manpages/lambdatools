import Xup

defmodule Future do
  @moduledoc """
  Simple library to work with Futures. Provides means to
  create, peek at and get values from futures.
  """

  alias(Future.Srv, as: F)

  @spec new((() -> any)) :: pid
  @doc "Creates a new future and return it's pid."
  def new(f) when is_function(f) do 
    {:ok, pid} = :supervisor.start_child(Future.Sup, [f])
    pid
  end
  def new(badarg), do: :erlang.error("Future.new/1 expected (() -> any), got #{inspect badarg}")

  @spec get(pid, integer) :: F.Ref.t | :timeout
  @doc "Hangs execution to get the future value."
  def get(fpid, timeout // 20) when is_pid(fpid) and is_integer(timeout) do
    state = F.peek(fpid)
    if (state.status == :running) do
      F.subscribe(fpid, :erlang.self)
      receive do
        {{F, fpid}, state} -> state.value
        _ -> get(fpid, timeout)
      after
        timeout -> :timeout
      end
    else
      F.stop(fpid)
      state.value
    end
  end
  # it produces a warning:
  # def get(arg, arg2), do: :erlang.error("Future.get/2 expected pid, integer got #{inspect arg} #{inspect arg2}")

  defsupervisor Sup, strategy: :simple_one_for_one do
    worker do: [id: Future.Srv]
  end

  defmodule Srv do
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
    def start_link(f), do: :gen_server.start_link(__MODULE__, f, [])

    @spec init((() -> any)) :: {:ok, pid}
    def init(f) do 
      new(:erlang.self, f)
      {:ok, Ref.new.pid(:erlang.self)}
    end

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

    defcast store(value), state: state do
      {:noreply, state.value(value).status(:value)}
    end

    defcast broadcast, state: state do
      if (state.report_to != :ordsets.new) do
        Enum.each(state.report_to, (&1 <- {{__MODULE__, :erlang.self}, state}))
        stop(:erlang.self)
      end
      {:noreply, state}
    end

    defcast stop, state: state do
      :supervisor.terminate_child(Future.Sup, :erlang.self)
      {:noreply, state}
    end

    defcall peek, state: state, do: {:reply, state, state}

    defcall get, state: state do
      if (state.status != :running) do
        stop(:erlang.self)
      end
      {:reply, state, state}
    end
  end
  
end
