import{r as d,u as A,R as k,j as I,a as B}from"./vendor.90e92a33.js";const T=function(){const l=document.createElement("link").relList;if(l&&l.supports&&l.supports("modulepreload"))return;for(const e of document.querySelectorAll('link[rel="modulepreload"]'))s(e);new MutationObserver(e=>{for(const t of e)if(t.type==="childList")for(const f of t.addedNodes)f.tagName==="LINK"&&f.rel==="modulepreload"&&s(f)}).observe(document,{childList:!0,subtree:!0});function g(e){const t={};return e.integrity&&(t.integrity=e.integrity),e.referrerpolicy&&(t.referrerPolicy=e.referrerpolicy),e.crossorigin==="use-credentials"?t.credentials="include":e.crossorigin==="anonymous"?t.credentials="omit":t.credentials="same-origin",t}function s(e){if(e.ep)return;e.ep=!0;const t=g(e);fetch(e.href,t)}};T();const U=()=>{const[c,l]=d.exports.useState(logseq.isMainUIVisible),g=A();return k.useEffect(()=>{const s="ui:visible:changed",e=async({visible:t})=>{g()&&l(t)};return logseq.on(s,e),()=>{logseq.off(s,e)}},[]),c},n=I.exports.jsx,b=I.exports.jsxs;function V(){const c=U(),[l,g]=d.exports.useState(!0),[s,e]=d.exports.useState(""),[t,f]=d.exports.useState(""),[h,S]=d.exports.useState(!0),[N,x]=d.exports.useState(!0),[E,y]=d.exports.useState(!1),[i,q]=d.exports.useState([]);function u(){var p=document.getElementById("Replace Status");E==!0&&i.length!=0?(p.className=p.className.replace("opacity-20","opacity-100"),x(!1)):(p.className=p.className.replace("opacity-100","opacity-20"),x(!0))}function M(){return l?"entire database":"current page"}if(k.useEffect(()=>{c&&(u(),document.getElementById("titleLabel").innerHTML="Searching in "+M())}),c){let p=function(a){e(a.target.value),document.getElementById("matchCount").innerHTML="Matches: ?",s!=""&&y(!1),u()},C=function(a){f(a.target.value),u()},w=function(){y(!1),u(),document.getElementById("matchCount").innerHTML="Matches: ?",document.getElementById("titleLabel").innerHTML="Searching in "+M(),g(!l)},R=function(){y(!1),document.getElementById("matchCount").innerHTML="Matches: ?",u(),S(!h)},L=function(){if(N==!1){var a=confirm(`Are you sure you want to replace all instances of "${s}" with "${t}" across ${i.length} blocks?`);if(a==!0){for(const r in i){console.log(i[r]);const m=i[r][0].uuid;console.log(m);const v=i[r][0].content;console.log(v);var o;h?o=new RegExp(s,"ig"):o=new RegExp(s,"g"),console.log(o),console.log(v.replaceAll(o,t)),logseq.Editor.updateBlock(i[r][0].uuid,i[r][0].content.replaceAll(o,t)),logseq.hideMainUI()}logseq.App.showMsg(`Successfully replaced in ${i.length} blocks", "success`)}else logseq.hideMainUI(),logseq.App.showMsg("Replacement Operation Cancelled","error")}};async function $(){if(l)h?logseq.DB.datascriptQuery(`
      [:find (pull ?b [*])
         :where
         [?b :block/content ?c]
         [(re-pattern "(?i)${s}") ?p]
         [(re-find ?p ?c)]
       ]
      `).then(o=>{a(o)}):logseq.DB.datascriptQuery(`
            [:find (pull ?b [*])
            :where
            [?b :block/content ?c]
            [(re-pattern "hello") ?p]
            [(re-find ?p ?c)]
          ]
      `).then(o=>{a(o)});else{let o=(await logseq.Editor.getCurrentPage()).name;console.log(o);let r=o;console.log(`[:find (pull ?b [*])
        :where
        [?b :block/page [:block/name "${r}"]]
        [?b :block/content ?c]
        [(re-pattern "(?i)${s}") ?p]
  [(re-find ?p ?c)]
]`),h?logseq.DB.datascriptQuery(`[:find (pull ?b [*])
               :where
               [?b :block/page [:block/name "${r}"]]
               [?b :block/content ?c]
               [(re-pattern "(?i)${s}") ?p]
         [(re-find ?p ?c)]
 ]`).then(m=>{a(m)}):logseq.DB.datascriptQuery(`
            [:find (pull ?b [*])
               :where
               [?b :block/page [:block/name "${r}"]]
               [?b :block/content ?c]
               [(re-pattern "${s}") ?p]
         [(re-find ?p ?c)]
 ]
      `).then(m=>{a(m)})}function a(o){document.getElementById("matchCount").innerHTML=`Matches: ${o.length}`,y(!0),u(),q(o)}}return b("div",{className:"grid place-items-center justify-center h-screen",children:[b("div",{className:"bg-gray-400 items-center rounded-md p-4 max-w-6/10",children:[n("label",{className:"text-3xl font-bold",id:"titleLabel",children:"Searching in current page"}),n("br",{}),n("br",{}),b("div",{className:"grid grid-cols-2 gap-4 place-items-auto",children:[n("div",{children:n("h1",{className:"font-bold text-size-23px p-3",children:"Find"})}),n("input",{onChange:p,className:"rounded-md"}),n("label",{className:"px-4",id:"matchCount",children:"Matches: ?"}),n("button",{onClick:$,className:" bg-dark-200 p-2 px-4 rounded-md text-light-300",children:"Search!"}),n("h1",{className:"font-bold text-size-23px p-3",children:"Replace"}),n("input",{onChange:C,className:"rounded-md"}),n("label",{className:"px-4",children:"Warning: This operation is irreversible. Make sure you have a backup of your data"}),n("button",{id:"Replace Status",onClick:L,className:"bg-dark-200 p-2 px-4 rounded-md text-light-300 opacity-20",children:"Replace All!"}),b("div",{children:[n("input",{type:"checkbox",checked:l,onChange:w})," ","Search ENTIRE database"]}),b("div",{children:[n("input",{type:"checkbox",checked:h,onChange:R})," ","Ignore Case(Case insensitive)?"]})]})]}),n("br",{})]})}return null}const j={id:"logseq-find-replace-plugin",icon:"./icon.png"},P=()=>{document.addEventListener("keydown",function(c){c.keyCode===27&&logseq.hideMainUI({restoreEditingCursor:!0}),c.stopPropagation()},!1)},F=j.id;function H(){console.info(`#${F}: MAIN`),B.render(n(k.StrictMode,{children:n(V,{})}),document.getElementById("app")),P();function c(){return{toggleFindReplaceUI(){logseq.showMainUI()}}}logseq.provideModel(c()),logseq.setMainUIInlineStyle({zIndex:11});const l="open-find-and-replace";logseq.App.registerUIItem("toolbar",{key:l,template:`
      <a class="button" data-on-click="toggleFindReplaceUI">
        <i class="ti ti-replace"></i>
      </a>
    `})}logseq.ready(H).catch(console.error);
