// openalexAPI page: builds an OpenAlex-style request, shows records or groups.
// Everything goes through the same API that openalexPro uses.
"use strict";

const $ = (id) => document.getElementById(id);
const state = { mode: "classic", page: 1, lastUrl: null, groupCol: null };
const SORTS = [
  ["", "Default (relevance with a search, else most cited)"],
  ["cited_by_count:desc", "Most cited"],
  ["publication_year:desc", "Newest"],
  ["publication_year:asc", "Oldest"],
  ["relevance_score:desc", "Most relevant (needs a search)"],
];
const GROUP_SORTS = [["", "Largest groups first"], ["key:asc", "By key"]];

function el(tag, attrs = {}, ...children) {
  const e = document.createElement(tag);
  for (const [k, v] of Object.entries(attrs)) {
    if (k === "text") e.textContent = v; else e.setAttribute(k, v);
  }
  for (const c of children) if (c != null) e.append(c);
  return e;
}

function setSorts(grouped) {
  const sel = $("sort");
  const keep = sel.value;
  sel.replaceChildren(...(grouped ? GROUP_SORTS : SORTS).map(([v, t]) => el("option", { value: v, text: t })));
  if ([...sel.options].some((o) => o.value === keep)) sel.value = keep;
}

function setMode(mode) {
  state.mode = mode;
  document.querySelectorAll(".modes button").forEach((b) =>
    b.setAttribute("aria-pressed", String(b.dataset.mode === mode)));
  $("classic").hidden = mode !== "classic";
  $("oql").hidden = mode !== "oql";
  // OQL says itself what to count by ("count of works by type")
  $("groupby").disabled = mode === "oql";
}

// The API URL (path + query) for the current form and page
function buildUrl() {
  const p = new URLSearchParams();
  const grouped = state.mode === "classic" && $("groupby").value !== "";
  let path;
  if (state.mode === "oql") {
    const oql = $("oqltext").value.trim();
    if (!oql) throw new Error("Enter an OQL query.");
    path = "/";
    p.set("oql", oql);
  } else {
    path = "/works";
    const filter = $("filter").value.trim().replace(/\s*,\s*/g, ",");
    if (filter) p.set("filter", filter);
    const search = $("search").value.trim();
    if (search) p.set($("exact").checked ? "search.exact" : "search", search);
    if (grouped) p.set("group_by", $("groupby").value);
  }
  if ($("sort").value) p.set("sort", $("sort").value);
  p.set("per_page", $("perpage").value);
  p.set("page", String(state.page));
  if (!grouped) {
    const sel = ["id", "display_name", "publication_year", "type", "cited_by_count", "authorships", "doi"];
    if (state.mode === "classic" && $("search").value.trim()) sel.push("relevance_score");
    p.set("select", sel.join(","));
  }
  return path + "?" + p.toString();
}

async function run(resetPage = true) {
  if (resetPage) state.page = 1;
  $("error").hidden = true;
  let url;
  try { url = buildUrl(); } catch (e) { return showError(e.message); }
  $("run").disabled = true;
  try {
    const resp = await fetch(url, { headers: { Accept: "application/json" } });
    const data = await resp.json();
    if (!resp.ok) return showError(errorText(data, resp.status));
    state.lastUrl = url;
    state.groupCol = state.mode === "classic" ? $("groupby").value : null;
    render(data);
  } catch (e) {
    showError("Request failed: " + e.message);
  } finally {
    $("run").disabled = false;
  }
}

function errorText(data, status) {
  const errs = data?.validation?.errors;
  if (errs?.length) return errs.map((e) => e.message).join("\n");
  return data?.message || data?.error || "HTTP " + status;
}

function showError(msg) {
  $("error").textContent = msg;
  $("error").hidden = false;
}

function render(data) {
  const m = data.meta || {};
  $("status").hidden = false;
  $("count").textContent = (m.count ?? 0).toLocaleString() + " works";
  $("time").textContent = m.db_response_time_ms != null ? `(${m.db_response_time_ms} ms)` : "";
  const oql = m.x_query?.oql;
  $("oqlecho").hidden = !oql;
  if (oql) $("oqlecho").querySelector("code").textContent = oql;

  const grouped = Array.isArray(data.group_by) && data.group_by.length > 0 && !data.results;
  $("results").hidden = grouped;
  $("groups").hidden = !grouped;
  if (grouped) renderGroups(data.group_by, m); else renderRecords(data.results || [], m);
}

function renderRecords(rows, m) {
  const tbody = document.querySelector("#results tbody");
  const rel = rows.some((r) => r.relevance_score != null);
  $("relhead").hidden = !rel;
  tbody.replaceChildren(...rows.map((r) => {
    const short = String(r.id || "").replace(/^.*\//, "");
    const title = el("a", { href: "/works/" + encodeURIComponent(short), target: "_blank", rel: "noopener",
                            text: r.display_name || "(no title)" });
    const names = (r.authorships || []).slice(0, 3).map((a) => a.author?.display_name).filter(Boolean);
    const more = (r.authorships || []).length > 3 ? " et al." : "";
    const cell = el("td", {}, title, names.length ? el("span", { class: "authors", text: names.join(", ") + more }) : null);
    const relCell = el("td", { class: "num", text: r.relevance_score != null ? r.relevance_score.toFixed(2) : "" });
    relCell.hidden = !rel;
    return el("tr", {}, cell,
      el("td", { text: r.publication_year ?? "" }),
      el("td", { text: r.type ?? "" }),
      el("td", { class: "num", text: (r.cited_by_count ?? "").toLocaleString() }),
      relCell);
  }));
  pager("prev", "next", "pageinfo", m);
}

function renderGroups(groups, m) {
  const max = Math.max(...groups.map((g) => g.count), 1);
  const list = document.querySelector("#groups .bars");
  list.replaceChildren(...groups.map((g) => {
    const name = g.key_display_name ?? g.key;
    // clicking a group narrows the filters to it
    const label = g.key === "unknown" || !state.groupCol
      ? el("span", { class: "label", title: g.key, text: name })
      : el("button", { type: "button", class: "label", title: "Filter on " + g.key, text: name });
    if (label.tagName === "BUTTON") label.addEventListener("click", () => narrow(g.key));
    const bar = el("span", { class: "bar" });
    bar.style.width = (100 * g.count / max) + "%";
    return el("li", {}, label, el("span", { class: "track" }, bar, el("span", { class: "n", text: g.count.toLocaleString() })));
  }));
  pager("gprev", "gnext", "gpageinfo", m, groups.length);
}

function narrow(key) {
  const value = String(key).replace(/^https:\/\/openalex\.org\/(types|languages|countries|licenses|source-types|sdgs)\//, "");
  const f = $("filter").value.trim();
  $("filter").value = (f ? f + "," : "") + state.groupCol + ":" + value;
  $("groupby").value = "";
  setSorts(false);
  run();
}

// Previous / next by page; records know the total, groups only whether a page is full
function pager(prevId, nextId, infoId, m, groupsOnPage) {
  const per = m.per_page || Number($("perpage").value);
  const pages = groupsOnPage == null ? Math.max(1, Math.ceil((m.count || 0) / per)) : null;
  $(prevId).disabled = state.page <= 1;
  $(nextId).disabled = pages != null ? state.page >= pages : groupsOnPage < per;
  $(infoId).textContent = pages != null ? `Page ${state.page} of ${pages}` : `Page ${state.page}`;
}

function step(delta) {
  state.page = Math.max(1, state.page + delta);
  run(false);
}

async function loadCorpus() {
  try {
    const c = await (await fetch("/corpus")).json();
    $("corpus").textContent = `${c.works.toLocaleString()} works · ${c.corpus}`;
    $("filters").replaceChildren(...c.filters.map((f) => {
      const b = el("button", { type: "button", text: f });
      b.addEventListener("click", () => {
        const v = $("filter").value.trim();
        $("filter").value = (v ? v + "," : "") + f + ":";
        $("filter").focus();
      });
      return b;
    }));
    for (const g of c.group_by || []) $("groupby").append(el("option", { value: g, text: g }));
  } catch (e) {
    $("corpus").textContent = "Could not read /corpus: " + e.message;
  }
}

document.querySelectorAll(".modes button").forEach((b) => b.addEventListener("click", () => setMode(b.dataset.mode)));
$("query").addEventListener("submit", (e) => { e.preventDefault(); run(); });
$("groupby").addEventListener("change", () => setSorts($("groupby").value !== ""));
$("prev").addEventListener("click", () => step(-1));
$("next").addEventListener("click", () => step(1));
$("gprev").addEventListener("click", () => step(-1));
$("gnext").addEventListener("click", () => step(1));
$("copy").addEventListener("click", async () => {
  if (!state.lastUrl) return;
  const full = location.origin + state.lastUrl.replace(/([?&])page=\d+&?/, "$1").replace(/[?&]$/, "");
  try { await navigator.clipboard.writeText(full); $("copy").textContent = "Copied"; }
  catch { prompt("API URL:", full); }
  setTimeout(() => { $("copy").textContent = "Copy API URL"; }, 1500);
});

setSorts(false);
loadCorpus();
