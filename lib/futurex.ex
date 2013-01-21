defmodule Future do
  @moduledoc """
  Simple library to work with Futures. Provides means to
  create, peek at and get values from futures.
  """
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
      report_to: [],
      f: nil do record_type(
          pid: nil | pid, 
          status: :undefined | :running | :value | :error | :exception, 
          value: any,
          report_to: [pid | atom | {atom, pid}],
          f: nil | (any -> any)
        )
      end

    def start_link(f), do: :gen_server.start_link(__MODULE__, f, [])
    def init(f) ,do: {:ok, Ref.new().pid(:erlang.self)}

  end
  
end
