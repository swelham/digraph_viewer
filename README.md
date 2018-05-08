# Digraph Viewer

Digraph viewer provides a visual representation of an erlang digraph through a web based interface.

![digraph_viewer screen grab](https://s3.amazonaws.com/digraphviewer/digraph_viewer_screen_grab.png)

## Installation

Head over to [hex](https://hex.pm/packages/digraph_viewer) to find the latest version to install.

Once installed make sure to add `digraph_viewer` to your list of applications (or start it manually).

```erlang
%% erlang
%% ---------
%% your_app.app.src
%% ---------
...
{applications, [digraph_viewer]}
...
```

```elixir
# elixir]
# ---------
# mix.exs
# ---------

def application do
  [applications: [:digraph_viewer]]
end
```

## Usage

Before the digraph can be viewed it must be registered with `digraph_viewer`.

```erlang
%% erlang

G = digraph:new(),
digraph_viewer:register(G).
```

```elixir
# elixir

g = :digraph.new()
:digraph_viewer.register(g)
```

You can now view the digraph by visiting http://localhost:8080/.