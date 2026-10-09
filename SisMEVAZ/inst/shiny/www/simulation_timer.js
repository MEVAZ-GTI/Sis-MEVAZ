// O relogio roda no navegador, inclusive enquanto o R executa a simulacao.
(() => {
  "use strict";

  function initialize() {
    const active = new Map();
    const pad = (number) => String(number).padStart(2, "0");

    function synchronize() {
      for (const [progress, timer] of active) {
        if (!progress.isConnected) {
          clearInterval(timer.interval);
          active.delete(progress);
        }
      }

      document.querySelectorAll(".shiny-progress-notification").forEach((progress) => {
        const message = progress.querySelector(".progress-message");
        if (!message || message.textContent.trim() !== "Executando Simulação MEVAZ..." || active.has(progress)) return;

        const label = document.createElement("div");
        label.className = "mevaz-simulation-timer";
        label.setAttribute("role", "timer");
        label.setAttribute("aria-live", "off");
        const started = performance.now();
        function update() {
          const seconds = Math.floor((performance.now() - started) / 1000);
          const hours = Math.floor(seconds / 3600);
          const minutes = Math.floor((seconds % 3600) / 60);
          const time = (hours ? pad(hours) + ":" : "") + pad(minutes) + ":" + pad(seconds % 60);
          label.textContent = "Tempo decorrido: " + time;
        }
        update();
        progress.appendChild(label);
        active.set(progress, { interval: setInterval(update, 1000) });
      });
    }

    new MutationObserver(synchronize).observe(document.body, {
      childList: true, subtree: true, characterData: true
    });
    synchronize();
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initialize, { once: true });
  } else {
    initialize();
  }
})();
