const internals = {};
internals.anchorSelector = 'a[target="_blank"][href*="github.com"]';
internals.observers = {};

const starColorSettingId = "show-github-star-star-color";
const numberColorSettingId = "show-github-star-number-color";
const githubTokenSettingId = "show-github-star-github-token";
const starCache = {};

function debounce(fn, wait = 500) {
  let timeoutId = null;
  let debouncedFn = function debouncedFn(...args) {
    window.clearTimeout(timeoutId);
    timeoutId = window.setTimeout(fn, wait, ...args);
  };

  return debouncedFn;
}

function onload() {
  // { extensionAPI }

  console.log("LOGSEQ_SHOW_GITHUB_STAR_PLUGIN load");

  startObserver("div#main-content-container");
  startObserver("div#right-sidebar");

  logseq.beforeunload(onunload);
}

function onunload() {
  console.log("LOGSEQ_SHOW_GITHUB_STAR_PLUGIN unload");

  stopObserver("div#main-content-container");
  stopObserver("div#right-sidebar");
}

function startObserver(selector) {
  let rootEl = top.document.querySelector(selector);

  if (rootEl == null) {
    return;
  }

  // reference: https://developer.mozilla.org/en-US/docs/Web/API/MutationObserver/MutationObserver
  let observerCallback = function observerCallback(mutationList, observer) {
    // we don't care about mutationList; it's simpler (and faster) to just query the DOM directly
    Array.from(rootEl.querySelectorAll(internals.anchorSelector)).forEach(
      addGithubStar
    );
  };

  // debounce the observer callback: "will postpone its execution until after wait = 500ms have elapsed since the last time it was invoked.";
  // otherwise we would be calling querySelectorAll + addGithubStar for every keystroke, which would be unnecessary

  // 创建一个新的 MutationObserver 实例
  internals.observers[selector] = new MutationObserver(
    debounce(observerCallback)
  );

  let observerOptions = {
    attributes: false, // 不关心元素的属性变化
    childList: true, // 关心元素的子元素的添加或移除
    subtree: true, // 关心所有子树中的变化，不仅仅是直接子元素
  };

  // 开始观察 rootEl 元素
  internals.observers[selector].observe(rootEl, observerOptions);

  // force initial execution
  observerCallback();
}

function stopObserver(selector) {
  internals.observers[selector].disconnect();

  let rootEl = top.document.querySelector(selector);
  Array.from(rootEl.querySelectorAll(internals.anchorSelector)).forEach(
    removeGithubStar
  );
}

function getGithubRepoInfo(url) {
  const regex = /https?:\/\/github\.com\/([^\/]+)\/([^\/]+)/;
  const match = url.match(regex);
  if (match) {
    return {
      userName: match[1],
      repoName: match[2],
    };
  }
  return null;
}

function getGithubStarCount(repoInfo) {
  const { userName, repoName } = repoInfo;
  const url = `https://api.github.com/repos/${userName}/${repoName}`;

  const key = `${userName}/${repoName}`;
  if (starCache[key] !== undefined) {
    return Promise.resolve(starCache[key]);
  }

  return fetch(url, {
    type: "GET",
    headers: {
      Authorization: logseq.settings?.[githubTokenSettingId] || "",
    },
  })
    .then((response) => response.json())
    .then((data) => {
      const count = data?.stargazers_count;
      const message = data?.message;

      if (count !== undefined) {
        starCache[key] = count;
        return count;
      } else if (message?.includes("API rate limit exceeded")) {
        throw new Error("API rate limit exceeded");
      } else if (message?.includes("Not Found")) {
        throw new Error("Not Found");
      } else {
        throw new Error("cant get star count");
      }
    });
}

function addGithubStar(el) {
  // skip anchor elements that have already been processed
  if (el.dataset.showGithubStar === "true") {
    return;
  }

  const url = el.href;
  const repoInfo = getGithubRepoInfo(url);
  if (!repoInfo) {
    return;
  }

  const starEl = document.createElement("span");
  // width="24"
  const svgContent = `
    <svg
      xmlns="http://www.w3.org/2000/svg"
      className="icon icon-tabler icon-tabler-star-filled"
      width="16"
      height="16"
      viewBox="0 0 24 24"
      stroke-width="2"
      stroke="currentColor"
      fill="none"
      stroke-linecap="round"
      stroke-linejoin="round"
    >
      <path stroke="none" d="M0 0h24v24H0z" fill="none"></path>
      <path
        d="M8.243 7.34l-6.38 .925l-.113 .023a1 1 0 0 0 -.44 1.684l4.622 4.499l-1.09 6.355l-.013 .11a1 1 0 0 0 1.464 .944l5.706 -3l5.693 3l.1 .046a1 1 0 0 0 1.352 -1.1l-1.091 -6.355l4.624 -4.5l.078 -.085a1 1 0 0 0 -.633 -1.62l-6.38 -.926l-2.852 -5.78a1 1 0 0 0 -1.794 0l-2.853 5.78z"
        stroke-width="0"
        fill="currentColor"
      ></path>
    </svg>
  `;

  // ⭐
  starEl.innerHTML = svgContent;
  starEl.style["display"] = "flex";
  starEl.style["width"] = "16px";
  starEl.style["height"] = "16px";
  starEl.style["justify-content"] = "center";
  starEl.style["align-items"] = "center";
  starEl.style["color"] = logseq.settings?.[starColorSettingId] || "orange";

  const numEl = document.createElement("span");
  numEl.style["padding-left"] = "2px";
  numEl.style["color"] = logseq.settings?.[numberColorSettingId] || "orange";

  const boxEl = document.createElement("span");
  boxEl.className = "githubStarBox";
  boxEl.style["display"] = "inline-flex";
  boxEl.style["line-height"] = "1";
  boxEl.style["padding-left"] = "6px";
  boxEl.style["vertical-align"] = "top";
  boxEl.style["margin-top"] = "4px";

  getGithubStarCount(repoInfo)
    .then((starCount) => {
      if (el.querySelector(".githubStarBox")) {
        return;
      }

      numEl.innerText = starCount;

      boxEl.appendChild(starEl);
      boxEl.appendChild(numEl);
      el.appendChild(boxEl);

      el.dataset.showGithubStar = "true";
    })
    .catch((err) => {
      console.log("get GithubStarCount err", err);
    });
}

function removeGithubStar(el) {
  if (el.dataset.showGithubStar === "true") {
    const boxEl = el.querySelector(".githubStarBox");
    el.removeChild(boxEl);
    el.dataset.showGithubStar = "";
  }
}

logseq
  .useSettingsSchema([
    {
      key: starColorSettingId,
      description:
        "Star's color, default is 'orange'. The format is CSS color, such as 'orange', '#FFA500', 'rgb(255,165,0)'",
      type: "string",
      default: "",
    },
    {
      key: numberColorSettingId,
      description:
        "Number's color, default is 'orange'. The format is CSS color, such as 'orange', '#FFA500', 'rgb(255,165,0)'",
      type: "string",
      default: "",
    },
    {
      key: githubTokenSettingId,
      description: "Your Github Personal access token",
      type: "string",
      default: "",
    },
  ])
  .ready(onload)
  .catch(console.error);
