(function() {
  var graphs = {};
  var selectedGraph = null;
  
  document.addEventListener('DOMContentLoaded', function() {
    fetch('/data')
      .then(function(response) {
        return response.json();
      })
      .then(function(data) {
        var listEl = document.getElementById('graph_list');
        
        if (data.length > 0) {
          data.forEach(function(info) {
            buildGraphListItem(listEl, info);
            graphs[info.id] = info;
          });
          
          renderGraph(data[0]);
        }
      });
  });
  
  function createElement(templateName, id) {
    var el = document
      .querySelector('[data-template="' + templateName + '"]')
      .cloneNode(true);
    
    el.setAttribute('id', '_' + id);
    el.removeAttribute('data-template');
    return el;
  }
  
  function buildGraphListItem(list, data) {
    var infoEl = createElement('graph-list-item', data.id);
    var infoSelector = '#_' + data.id;
    
    list.appendChild(infoEl);
    
    var nameEl = document.querySelector(infoSelector + ' [data-name]');
    var proctectionEl = document.querySelector(infoSelector + ' [data-protection]');
    var cyclicityEl = document.querySelector(infoSelector + ' [data-cyclicity]');
    var memoryEl = document.querySelector(infoSelector + ' [data-memory]');
    
    nameEl.innerText = data.name || data.id;
    proctectionEl.innerText = data.protection;
    cyclicityEl.innerText = data.cyclicity;
    memoryEl.innerText = data.memory + ' words';
    
    infoEl.addEventListener('click', function() {
      var id = this.id.slice(1);
      
      if (id !== selectedGraph.id) {
        renderGraph(graphs[id]);
      }
    });
  }
  
  function renderGraph(graphData) {
    var vertices = graphData.vertices.map(function(v) {
      return {id: v.id, label: v.id}
    });
    
    var edges = graphData.edges.map(function(v) {
      return {
        label: v.id,
        from: v.source,
        to: v.target,
        arrows:'to',
        font: {align: 'horizontal'}}
    });
    
    var nodes = new vis.DataSet(vertices);
    var edges = new vis.DataSet(edges);

    var container = document.getElementById('graph_container');
    var data = {
      nodes: nodes,
      edges: edges
    };
    var options = {};
    new vis.Network(container, data, options);
    setSelectedGraph(graphData);
  }
  
  function setSelectedGraph(graph) {
    selectedGraph = graph;
    document.getElementById('graph_header').innerText = graph.name || graph.id;
  }
})();