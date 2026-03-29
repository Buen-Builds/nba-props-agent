
chrome.storage.onChanged.addListener((changes) => {
  if (changes.prizepicks_props) {
    console.log("AgentForge: Props updated", changes.prizepicks_props.newValue.length);
  }
});
