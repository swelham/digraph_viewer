(function() {
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
  
  document.addEventListener('DOMContentLoaded', function() {
    fetch('/data')
      .then(function(response) {
        return response.json();
      })
      .then(function(data) {
        var listEl = document.getElementById('graph_list');
        
        data.forEach(function(info) {
          buildGraphListItem(listEl, info);
        });
      });
  });
})();