import Xup

defsupervisor Future.Sup, strategy: :rest_for_one do
  worker do: [id: Future.Mon]

  supervisor SOFO, strategy: :simple_one_for_one do
    worker do: [id: Future.Srv]
  end
end

defmodule Future.App do
  use Application.Behaviour

  def start(_, _) do
    Future.Sup.start_link()
  end 
end
