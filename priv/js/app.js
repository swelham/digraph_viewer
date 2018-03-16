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
    
    nameEl.innerText = data.id;
    proctectionEl.innerText = data.protection;
    cyclicityEl.innerText = data.cyclicity;
    memoryEl.innerText = data.memory + ' words';
  }
  
  function renderGraph(graphData) {
    var svg = d3.select("svg"),
      width = +svg.attr("width"),
      height = +svg.attr("height");
    
    var link_force =  d3.forceLink(graphData.links)
      .distance(100)
      .id(function(d) { return d.id; });

    var simulation = d3.forceSimulation()
      .nodes(graphData.nodes)
      .force("center_force", d3.forceCenter(width / 2, height / 2))
      .force("charge_force", d3.forceManyBody().strength(-240))
      .force("links",link_force)
      .on("tick", tickActions);

    var link = svg.append("g")
      .attr("class", "links")
      .selectAll("line")
      .data(graphData.links)
      .enter()
      .append("line")
      .attr("stroke-width", 2);
      
    var node = svg.selectAll(".node")
      .data(graphData.nodes)
      .enter()
      .append('g')
      .classed('node', true)
      .call(d3.drag()
          .on("start", dragstarted)
          .on("drag", dragged)
          .on("end", dragended));;
      
    node.append("circle")
      .attr("r", 25)
      .style("fill", '#408BC9')
      .append("title")
      .text(function(d) { return d.id; });

    node.append("text")
      .attr('fill', '#fff')
      .attr('text-anchor', 'middle')
      .text(function(d) { return d.id; });
    
    function tickActions() {
      node.attr("transform", function(d) { return 'translate(' + [d.x, d.y] + ')'; })

      link
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