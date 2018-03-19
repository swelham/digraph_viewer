(function() {
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
          });
          
          renderGraph(data[0]);
        }
      });
  });
  
  function createElement(templateName, id) {
    var el = document
      .querySelector('[data-template="' + templateName + '"]')
      .cloneNode(true);
    
    el.setAttribute('id', 'id_' + id);
    el.removeAttribute('data-template');
    return el;
  }
  
  function buildGraphListItem(list, data) {
    var infoEl = createElement('graph-list-item', data.id);
    var infoSelector = '#id_' + data.id;
    
    list.appendChild(infoEl);
    
    var nameEl = document.querySelector(infoSelector + ' [data-name]');
    var proctectionEl = document.querySelector(infoSelector + ' [data-protection]');
    var cyclicityEl = document.querySelector(infoSelector + ' [data-cyclicity]');
    var memoryEl = document.querySelector(infoSelector + ' [data-memory]');
    
    nameEl.innerText = data.name || data.id;
    proctectionEl.innerText = data.protection;
    cyclicityEl.innerText = data.cyclicity;
    memoryEl.innerText = data.memory + ' words';
  }
  
  function renderGraph(graphData) {
    var svg = d3.select("svg"),
      width = +svg.attr("width"),
      height = +svg.attr("height");
    
    var link_force =  d3.forceLink(graphData.edges)
      .distance(100)
      .id(function(d) { return d.id; });

    var simulation = d3.forceSimulation()
      .nodes(graphData.vertices)
      .force("center_force", d3.forceCenter(width / 2, height / 2))
      .force("charge_force", d3.forceManyBody().strength(-240))
      .force("edges",link_force)
      .on("tick", tickActions);

    var edge = svg.append("g")
      .attr("class", "edges")
      .selectAll("line")
      .data(graphData.edges)
      .enter()
      .append("line")
      .attr("stroke-width", 2);
      
    var vertex = svg.selectAll(".vertex")
      .data(graphData.vertices)
      .enter()
      .append('g')
      .classed('vertex', true)
      .call(d3.drag()
          .on("start", dragstarted)
          .on("drag", dragged)
          .on("end", dragended));;
      
    vertex.append("circle")
      .attr("r", 25)
      .style("fill", '#408BC9')
      .append("title")
      .text(function(d) { return d.id; });

    vertex.append("text")
      .attr('fill', '#fff')
      .attr('text-anchor', 'middle')
      .text(function(d) { return d.id; });
    
    function tickActions() {
      vertex.attr("transform", function(d) { return 'translate(' + [d.x, d.y] + ')'; })

      edge
        .attr("x1", function(d) { return d.source.x; })
        .attr("y1", function(d) { return d.source.y; })
        .attr("x2", function(d) { return d.target.x; })
        .attr("y2", function(d) { return d.target.y; });
    }
    
    function dragstarted(d) {
      if (!d3.event.active) simulation.alphaTarget(0.3).restart();
      d.fx = d.x;
      d.fy = d.y;
    }

    function dragged(d) {
      d.fx = d3.event.x;
      d.fy = d3.event.y;
    }

    function dragended(d) {
      if (!d3.event.active) simulation.alphaTarget(0);
      d.fx = null;
      d.fy = null;
    }
  }
})();