const tabContainers = document.querySelectorAll('.tabs');

tabContainers.forEach(tabContainer => {

  const tabButtons = tabContainer.querySelectorAll('.tab-button');
  const tabContents = tabContainer.querySelectorAll('.tab-content');
  const tabSelect = tabContainer.querySelector('#tabSelect');

  function activateTab(tabName) {
    tabButtons.forEach(btn => {
      btn.classList.toggle('active', btn.dataset.tab === tabName);
    });

    tabContents.forEach(content => {
      content.classList.toggle('active', content.id === tabName);
    });

    if (tabSelect) {
      tabSelect.value = tabName;
    }
  }

  // Button click
  tabButtons.forEach(button => {
    button.addEventListener('click', () => {
      activateTab(button.dataset.tab);
    });
  });

  // Dropdown change
  if (tabSelect) {
    tabSelect.addEventListener('change', (e) => {
      activateTab(e.target.value);
    });
  }

  // Activate the first tab in each container on page load
  if (tabContents.length > 0) {
    activateTab(tabContents[0].id);
  }
})
