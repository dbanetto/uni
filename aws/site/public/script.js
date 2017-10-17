window.onload = () => {
  let form = document.getElementById('compute');
  form.addEventListener('submit', submitCompute, false);
}

function submitCompute(event) {
  event.preventDefault();

  let params = { "max": null, "loops": null };

  params.max = event.target.querySelector('input[name=max]').value;
  params.loops = event.target.querySelector('input[name=loops]').value;
  params.size = event.target.querySelector('select[name=size]').value;

  fetch('/compute', { credentials: 'include', method: 'POST', body: JSON.stringify(params), mode: 'same-origin',
    headers: {
      'Accept': 'application/json',
      'Content-Type': 'application/json'
    }
  })
    .then((r) => r.json())
    .then((result) => {
      console.log(result);
      let div = document.getElementById('results');

      div.innerHTML = JSON.stringify(result);

    })
    .catch((err) => {
      let div = document.getElementById('results');

      div.innerHTML = "Error occured, " + err;
      console.log("Error occured, " + err);
    });

  return false;
}

