
const CANISTER_URL = "https://a4gq6-oaaaa-aaaab-qaa4q-cai.raw.icp0.io/?id=p6ttk-lyaaa-aaaah-avb2q-cai";
let scrapedProps = [];

document.getElementById("scrapeBtn").addEventListener("click", async () => {
  const [tab] = await chrome.tabs.query({ active: true, currentWindow: true });
  
  if (!tab.url.includes("prizepicks.com")) {
    document.getElementById("status").innerHTML = '<span style="color:#e74c3c">Please open PrizePicks first</span>';
    return;
  }

  document.getElementById("status").textContent = "Scraping lines...";

  try {
    const results = await chrome.scripting.executeScript({
      target: { tabId: tab.id },
      func: () => {
        const props = [];
        const cards = document.querySelectorAll("[class*=\"ProjectionCard\"], [class*=\"projection-card\"], [data-testid*=\"projection\"]");
        
        cards.forEach(card => {
          try {
            const nameEl = card.querySelector("[class*=\"name\"], [class*=\"player-name\"], h3, h4");
            const statEl = card.querySelector("[class*=\"stat\"], [class*=\"category\"]");
            const lineEl = card.querySelector("[class*=\"line\"], [class*=\"value\"], [class*=\"score\"]");
            
            if (nameEl && lineEl) {
              const line = parseFloat(lineEl.textContent.trim());
              if (!isNaN(line)) {
                props.push({
                  player: nameEl.textContent.trim(),
                  stat: statEl ? statEl.textContent.trim() : "Unknown",
                  line: line,
                  scraped_at: new Date().toISOString()
                });
              }
            }
          } catch(e) {}
        });

        if (props.length === 0) {
          const allText = document.body.innerText;
          const lines = allText.split("\n").filter(l => l.trim());
          lines.forEach((line, i) => {
            const num = parseFloat(line.trim());
            if (!isNaN(num) && num > 0.5 && num < 100 && i > 0) {
              const prevLine = lines[i-1].trim();
              if (prevLine.length > 3 && prevLine.length < 50 && isNaN(parseFloat(prevLine))) {
                props.push({
                  player: prevLine,
                  stat: "Props",
                  line: num,
                  scraped_at: new Date().toISOString()
                });
              }
            }
          });
        }

        return props;
      }
    });

    scrapedProps = results[0].result || [];
    
    if (scrapedProps.length > 0) {
      document.getElementById("status").innerHTML = `<span class="green">Found ${scrapedProps.length} props!</span>`;
      document.getElementById("sendBtn").style.display = "block";
      
      const list = document.getElementById("propsList");
      list.innerHTML = "<div class=\"props-list\">" + 
        scrapedProps.slice(0, 10).map(p => 
          `<div class="prop-item"><span class="purple">${p.player}</span> ${p.stat} <span class="green">${p.line}</span></div>`
        ).join("") + 
        (scrapedProps.length > 10 ? `<div class="prop-item">...and ${scrapedProps.length - 10} more</div>` : "") +
        "</div>";
    } else {
      document.getElementById("status").innerHTML = '<span style="color:#e67e22">No props found. Make sure NBA is selected on PrizePicks.</span>';
    }
  } catch(e) {
    document.getElementById("status").innerHTML = `<span style="color:#e74c3c">Error: ${e.message}</span>`;
  }
});

document.getElementById("sendBtn").addEventListener("click", async () => {
  document.getElementById("status").textContent = "Sending to canister...";
  
  try {
    await chrome.storage.local.set({ 
      prizepicks_props: scrapedProps,
      last_updated: new Date().toISOString()
    });
    
    document.getElementById("status").innerHTML = `<span class="green">Sent ${scrapedProps.length} props to agent!</span>`;
  } catch(e) {
    document.getElementById("status").innerHTML = `<span style="color:#e74c3c">Error: ${e.message}</span>`;
  }
});
