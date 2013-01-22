defmodule Future.Mon do
  @moduledoc """
  Monitor and manage futures. Uses process dictionary.
  Problems?
  """
  use GenServer.Behaviour
  import GenX.GenServer

  alias(Future.Srv, as: F)

  #async
  #@doc "Adds consumer of future f that runs on pid fpid"
  defcast add_consumer(f, fpid), export: __MODULE__ do
    case(get!({:future, f})) do
     :undefined -> put!({:future, f}, {fpid, 1})
     {_, ref_count} -> put!({:Future, f}, {fpid, ref_count+1})
    end
    {:noreply, []}
  end

  #@doc "Deletes consumer of future and stops future server if there are no consumers left"
  defcast del_consumer(f), export: __MODULE__ do
    case(get!({:future, f})) do
      :undefined -> :noproc # error maybe? that's a bad sign!
      {fpid, ref_count} -> del_consumer!(f, fpid, ref_count)
    end
    {:noreply, []}
  end

  #@doc "Subscribes caller to the future value announcement"
  defcast subscribe(f, from), export: __MODULE__ do
    F.subscribe(where_is!(f), from)
    {:noreply, []}
  end

  #sync
  #@doc "Returns actual pid of f"
  defcall where_is(f), export: __MODULE__ do 
    {:reply, where_is!(f), []}
  end

  #@doc "Returns actual state of f"
  defcall state_of(f), export: __MODULE__ do
    return = where_is!(f)
    return = if (return == :undefined), do: return, else: F.peek(return)
    {:reply, return, []}
  end

  #private
  @spec put!({:future, (() -> any)}, {pid, integer}) :: :undefined | {pid, integer}
  defp put!(x,y), do: :erlang.put(x,y)
  @spec get!({:future, (() -> any)}) :: :undefined | {pid, integer}
  defp get!(x), do: :erlang.get(x)
  @spec erase!({:future, (() -> any)}) :: :undefined | {pid, integer}
  defp erase!(x), do: :erlang.erase(x)
  @spec where_is!((() -> any)) :: :undefined | pid
  defp where_is!(f) do 
    case (:erlang.get({:future, f})) do
      {pid, _} -> pid
      other -> other
    end
  end
  defp del_consumer!(f, fpid, ref_count) do
    if ((ref_count-1) > 0) do
      put!({:future, f}, {fpid, ref_count-1})
    else
      IO.puts "Down goes Brown"
      F.halt(fpid)
      erase!({:future, f})
    end
  end

  #behaviour
  def start_link do
    :gen_server.start_link({:local, __MODULE__}, __MODULE__, nil, [])
  end

  def init(_) do 
    {:ok, []}
  end

  def terminate(_shutdown, state), do: :ok
end
