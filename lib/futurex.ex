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
      status: :starting,
      value: :undefined,
      report_to: [] do record_type(
          pid: nil | pid, 
          status: :undefined | :running | :value | :error | :exception, 
          value: any,
          report_to: [pid | atom | {atom, pid}]
        )
      end

  end
  
end
