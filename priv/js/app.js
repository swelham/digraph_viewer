(function() {
  document.addEventListener('DOMContentLoaded', function() {
    fetch('/data')
      .then(function(response) {
        return response.json();
      })
      .then(function(data) {
        var list = document.getElementById('graph_list');
        
        data.forEach(function(info) {
          var infoEl = document.createElement('li');
          infoEl.classList.add('ph3');
          infoEl.classList.add('pv3');
          infoEl.classList.add('bb');
          infoEl.classList.add('b--light-silver');
          infoEl.innerText = info.id;
          list.appendChild(infoEl);
          
          renderGraph(info.id);
        });
      });
  });
})();