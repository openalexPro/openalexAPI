// Pure functions of the openalexAPI page: no DOM, so they can be tested
// (tests/testthat/test-page.R runs them with V8).
"use strict";

var OAQ = (function () {
  var RECORD_FIELDS = ["id", "display_name", "publication_year", "type", "cited_by_count",
                       "authorships", "doi"];

  function encode(params) {
    return Object.keys(params).map(function (k) {
      return encodeURIComponent(k) + "=" + encodeURIComponent(params[k]);
    }).join("&");
  }

  // The API URL (path + query) for a form:
  //   {mode: "classic"|"oql", oql, filter, search, exact, groupBy, sort, perPage}
  // `page` adds the page's view parameters (per_page, page, select); without
  // it the URL holds only the query, for openalexPro::pro_fetch().
  // `columns` (the corpus's served columns, from /corpus): only these are
  // selected, so a corpus without e.g. cited_by_count still answers.
  function buildUrl(form, page, columns) {
    var p = {};
    var grouped = form.mode === "classic" && !!form.groupBy;
    var path;
    if (form.mode === "oql") {
      var oql = (form.oql || "").trim();
      if (!oql) throw new Error("Enter an OQL query.");
      path = "/";
      p.oql = oql;
    } else {
      path = "/works";
      var filter = (form.filter || "").trim().replace(/\s*,\s*/g, ",");
      if (filter) p.filter = filter;
      var search = (form.search || "").trim();
      if (search) p[form.exact ? "search.exact" : "search"] = search;
      if (grouped) p.group_by = form.groupBy;
    }
    if (form.sort) p.sort = form.sort;
    if (page != null) {
      p.per_page = String(form.perPage || 25);
      p.page = String(page);
      if (!grouped) {
        var sel = RECORD_FIELDS.filter(function (f) { return !columns || columns.indexOf(f) >= 0; });
        if (form.mode === "classic" && (form.search || "").trim()) sel.push("relevance_score");
        p.select = sel.join(",");
      }
    }
    var q = encode(p);
    return q ? path + "?" + q : path;
  }

  // The filter value for a group key: slugs of non-native entities without
  // their https://openalex.org/<entity>/ prefix (OpenAlex accepts the other
  // keys, IDs included, as they are)
  function groupFilterValue(key) {
    var slug = /^https:\/\/openalex\.org\/(types|languages|countries|licenses|source-types|sdgs)\//;
    var k = String(key);
    // slug keys are URL-encoded (source-types/ebook%20platform)
    return slug.test(k) ? decodeURIComponent(k.replace(slug, "")) : k;
  }

  // A grouped response has meta.groups_count (a number, also 0) and no results
  function isGrouped(data) {
    return data != null && data.meta != null && data.meta.groups_count != null &&
      data.results === undefined;
  }

  // Parse a response body: JSON, else a message with the status
  function parseBody(text, status) {
    try { return JSON.parse(text); }
    catch (e) { return { message: "HTTP " + status + ": " + String(text).slice(0, 300) }; }
  }

  return { buildUrl: buildUrl, groupFilterValue: groupFilterValue, isGrouped: isGrouped,
           parseBody: parseBody };
})();
