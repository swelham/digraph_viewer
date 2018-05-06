# Digraph Viewer

Digraph viewer provides a visual representation of an erlang digraph through a web based interface.

## Installation

Head over to hex to find the latest version to install.

Once installed make sure to add `digraph_viewer` to your list of applications (or start it manually).

```erlang
%% ---------
%% your_app.app.src
%% ---------
...
{applications, [digraph_viewer]}
...
```

```elixir
# mix.exs

def application do
  [applications: [:digraph_viewer]]
end
```

## Usage

Before the digraph can be viewed it must be registered with `digraph_viewer`.

```erlang
G = digraph:new(),
digraph_viewer:register(G).
```

```elixir
g = :digraph.new()
:digraph_viewer.register(g)
```

You can now view the digraph by visiting [http://localhost:8080/] (http://localhost:8080/).