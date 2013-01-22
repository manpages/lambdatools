import Xup

defsupervisor Future.Sup, strategy: :rest_for_one do
  worker do: [id: Future.Mon]

  supervisor SOFO, strategy: :simple_one_for_one do
    worker do: [id: Future.Srv, restart: :permanent, shutdown: :brutal_kill]
  end
end

defmodule Future.App do
  use Application.Behaviour

  def start(_, _) do
    Future.Sup.start_link()
  end 
end
