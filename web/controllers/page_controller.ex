defmodule Campjs.PageController do
  use Campjs.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
