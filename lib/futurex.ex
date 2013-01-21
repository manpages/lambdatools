import Xup

defmodule Future do
  @moduledoc """
  Simple library to work with Futures. Provides means to
  create, peek at and get values from futures.
  """

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
      status: :undefined,
      value: :undefined,
      report_to: :ordsets.new do record_type(
          pid: nil | pid, 
          status: :undefined | :running | :value | :error | :exception, 
          value: any,
          report_to: [pid | atom | {atom, pid}]
        )
      end

    #@spec start_link(() -> any) :: {:ok, pid}
    def start_link(f), do: :gen_server.start_link(__MODULE__, f, [])

    def init(f) do 
      new(:erlang.self, f)
      {:ok, Ref.new.pid(:erlang.self)}
    end

    defcast new(f), state: state do
      :erlang.spawn_link(fn() ->
        y = f.()
        store(:erlang.self, y)
        broadcast(:erlang.self)
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
      Enum.each(state.report_to, (&1 <- state))
      {:noreply, state}
    end

  end
  
end
